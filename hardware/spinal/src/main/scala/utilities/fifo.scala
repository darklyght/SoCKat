package sockat.utilities

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue
import scala.util.Random

case class FIFOParameters[T <: Data] (
    dataType: HardType[T],
    addressWidth: Int = 1
)

case class FIFO[T <: Data] (
    parameters: FIFOParameters[T]
) extends Component {
    val io = new Bundle {
        val enqueue = slave(Stream(parameters.dataType()))
        val dequeue = master(Stream(parameters.dataType()))
    }

    val depth = Math.pow(2, parameters.addressWidth).toInt

    val readPointer = Reg(UInt(parameters.addressWidth + 1 bits)) init(0)
    val writePointer = Reg(UInt(parameters.addressWidth + 1 bits)) init(0)

    val memory = Mem(
        wordType = parameters.dataType,
        wordCount = depth
    )

    when (io.enqueue.fire) {
        writePointer := writePointer + 1
        memory(writePointer(parameters.addressWidth - 1 downto 0)) := io.enqueue.payload
    }

    when (io.dequeue.fire) {
        readPointer := readPointer + 1
    }

    io.enqueue.ready := (readPointer(parameters.addressWidth) === writePointer(parameters.addressWidth)) || (readPointer(parameters.addressWidth - 1 downto 0) =/= writePointer(parameters.addressWidth - 1 downto 0))
    io.dequeue.payload := memory(readPointer(parameters.addressWidth - 1 downto 0))
    io.dequeue.valid := readPointer =/= writePointer
}

object FIFOSimulation {
    def test(
        dut: FIFO[UInt],
        enqueueProbability: Int,
        dequeueProbability: Int
    ) = {
        def driver(
            dut: FIFO[UInt],
            queue: Queue[Int]
        ) = {
            val rand = new scala.util.Random

            while (true) {
                dut.io.enqueue.valid #= rand.nextInt(100) < enqueueProbability
                dut.io.enqueue.payload.randomize()

                dut.clockDomain.waitSampling()
                if (dut.io.enqueue.valid.toBoolean && dut.io.enqueue.ready.toBoolean) {
                    queue.enqueue(dut.io.enqueue.payload.toInt)
                }
            }
        }

        def monitor(
            dut: FIFO[UInt],
            queue: Queue[Int]
        ) = {
            val rand = new scala.util.Random

            while (true) {
                dut.io.dequeue.ready #= rand.nextInt(100) < dequeueProbability
                dut.clockDomain.waitSampling()

                if (dut.io.dequeue.valid.toBoolean && dut.io.dequeue.ready.toBoolean) {
                    assert(dut.io.dequeue.payload.toInt == queue.dequeue())
                }
            }
        }

        val queue = Queue[Int]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(100000000))
        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, queue))

        dut.clockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val enqueueProbabilities = Seq(10, 30, 50, 70, 90)
        val dequeueProbabilities = Seq(10, 30, 50, 70, 90)
        val tests = for {
            i <- enqueueProbabilities
            j <- dequeueProbabilities
        } yield (i, j)

        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .compile(
                                    FIFO(FIFOParameters(
                                        dataType = UInt(8 bits),
                                        addressWidth = 8
                                    ))
                                )

        tests.foreach({case (enqueueProbability, dequeueProbability) => {
            compiled.doSim(dut => test(dut, enqueueProbability, dequeueProbability))
        }})
    }
}