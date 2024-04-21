package sockat.utilities

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue
import scala.util.Random

case class AsyncFIFOParameters[T <: Data] (
    dataType: HardType[T],
    addressWidth: Int = 1,
    enqueueClockDomain: ClockDomain,
    dequeueClockDomain: ClockDomain
)

case class AsyncFIFO[T <: Data] (
    parameters: AsyncFIFOParameters[T]
) extends Component {
    val io = new Bundle {
        val enqueue = slave(Stream(parameters.dataType())) addTag(crossClockDomain)
        val dequeue = master(Stream(parameters.dataType())) addTag(crossClockDomain)
    }

    val enqueueReset = if (parameters.enqueueClockDomain.config.resetActiveLevel == HIGH) parameters.enqueueClockDomain.readResetWire else ~parameters.enqueueClockDomain.readResetWire
    val dequeueReset = if (parameters.dequeueClockDomain.config.resetActiveLevel == HIGH) parameters.dequeueClockDomain.readResetWire else ~parameters.dequeueClockDomain.readResetWire

    val enqueueResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    enqueueResetSynchronizer.io.clock := parameters.enqueueClockDomain.readClockWire
    enqueueResetSynchronizer.io.async := enqueueReset || dequeueReset

    val dequeueResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    dequeueResetSynchronizer.io.clock := parameters.dequeueClockDomain.readClockWire
    dequeueResetSynchronizer.io.async := dequeueReset || enqueueReset

    val enqueueDomain = ClockDomain(
        clock = parameters.enqueueClockDomain.readClockWire,
        reset = enqueueResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val dequeueDomain = ClockDomain(
        clock = parameters.dequeueClockDomain.readClockWire,
        reset = dequeueResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val depth = Math.pow(2, parameters.addressWidth).toInt

    val enqueueArea = new ClockingArea(enqueueDomain) {
        val memory = Mem(
            wordType = parameters.dataType,
            wordCount = depth
        )

        val writePointer = Reg(UInt(parameters.addressWidth + 1 bits)) init(0)
        val readPointerSync = Vec.fill(2)(
            Reg(Bits(parameters.addressWidth + 1 bits)) init(0) addTag(crossClockDomain)
        )
        val readPointer = fromGray(readPointerSync(0))

        when (io.enqueue.fire) {
            writePointer := writePointer + 1
            memory(writePointer(parameters.addressWidth - 1 downto 0)) := io.enqueue.payload
        }

        io.enqueue.ready := ((readPointer(parameters.addressWidth) === writePointer(parameters.addressWidth)) || (readPointer(parameters.addressWidth - 1 downto 0) =/= writePointer(parameters.addressWidth - 1 downto 0))) && ~enqueueResetSynchronizer.io.sync
    }

    val dequeueArea = new ClockingArea(dequeueDomain) {
        val readPointer = Reg(UInt(parameters.addressWidth + 1 bits)) init(0)
        val writePointerSync = Vec.fill(2)(
            Reg(Bits(parameters.addressWidth + 1 bits)) init(0) addTag(crossClockDomain)
        )
        val writePointer = fromGray(writePointerSync(0))

        when (io.dequeue.fire) {
            readPointer := readPointer + 1
        }

        io.dequeue.payload := enqueueArea.memory(readPointer(parameters.addressWidth - 1 downto 0))
        io.dequeue.valid := readPointer =/= writePointer && ~dequeueResetSynchronizer.io.sync
    }

    dequeueArea.writePointerSync(1) := toGray(enqueueArea.writePointer)
    dequeueArea.writePointerSync(0) := dequeueArea.writePointerSync(1)

    enqueueArea.readPointerSync(1) := toGray(dequeueArea.readPointer)
    enqueueArea.readPointerSync(0) := enqueueArea.readPointerSync(1)
}

object AsyncFIFOSimulation {
    def test(
        dut: AsyncFIFO[UInt],
        enqueueProbability: Int,
        dequeueProbability: Int,
        enqueueClockFrequency: Int,
        dequeueClockFrequency: Int
    ) = {
        def driver(
            dut: AsyncFIFO[UInt],
            queue: Queue[Int]
        ) = {
            val rand = new scala.util.Random

            while (true) {
                dut.io.enqueue.valid #= rand.nextInt(100) < enqueueProbability
                dut.io.enqueue.payload.randomize()

                dut.parameters.enqueueClockDomain.waitSampling()
                if (dut.io.enqueue.valid.toBoolean && dut.io.enqueue.ready.toBoolean) {
                    queue.enqueue(dut.io.enqueue.payload.toInt)
                }
            }
        }

        def monitor(
            dut: AsyncFIFO[UInt],
            queue: Queue[Int]
        ) = {
            val rand = new scala.util.Random

            while (true) {
                dut.io.dequeue.ready #= rand.nextInt(100) < dequeueProbability
                dut.parameters.dequeueClockDomain.waitSampling()

                if (dut.io.dequeue.valid.toBoolean && dut.io.dequeue.ready.toBoolean) {
                    assert(dut.io.dequeue.payload.toInt == queue.dequeue())
                }
            }
        }

        val queue = Queue[Int]()

        dut.parameters.enqueueClockDomain.forkStimulus(frequency = HertzNumber(enqueueClockFrequency))
        dut.parameters.dequeueClockDomain.forkStimulus(frequency = HertzNumber(dequeueClockFrequency))
        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, queue))

        dut.parameters.enqueueClockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val enqueueProbabilities = Seq(10, 30, 50, 70, 90)
        val dequeueProbabilities = Seq(10, 30, 50, 70, 90)
        val enqueueClockFrequencies = Seq(100000000, 123456789)
        val dequeueClockFrequencies = Seq(34831093, 243928723)
        val tests = for {
            i <- enqueueProbabilities
            j <- dequeueProbabilities
            k <- enqueueClockFrequencies
            l <- dequeueClockFrequencies
        } yield (i, j, k, l)

        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .compile(
                                    AsyncFIFO(AsyncFIFOParameters(
                                        dataType = UInt(8 bits),
                                        addressWidth = 8,
                                        ClockDomain.external(
                                            name = "enqueueClock",
                                            config = ClockDomainConfig(
                                                clockEdge = RISING,
                                                resetKind = ASYNC,
                                                resetActiveLevel = HIGH,
                                                clockEnableActiveLevel = HIGH
                                            )
                                        ),
                                        ClockDomain.external(
                                            name = "dequeueClock",
                                            config = ClockDomainConfig(
                                                clockEdge = RISING,
                                                resetKind = ASYNC,
                                                resetActiveLevel = HIGH,
                                                clockEnableActiveLevel = HIGH
                                            )
                                        )
                                    ))
                                )

        tests.foreach({case (enqueueProbability, dequeueProbability, enqueueClockFrequency, dequeueClockFrequency) => {
            compiled.doSim(dut => test(dut, enqueueProbability, dequeueProbability, enqueueClockFrequency, dequeueClockFrequency))
        }})
    }
}