package sockat.uart

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue
import scala.util.Random

import sockat.utilities._

case class Receiver (
    parameters: UARTParameters
) extends Component {
    val io = new Bundle {
        val data = master Flow(UInt(8 bits))
        val serial = in UInt(1 bits) addTag(crossClockDomain)
    }

    val bitCount = parameters.clockFrequency / parameters.baudRate
    val counterWidth = log2Up(bitCount.toInt)

    val bitCounter = Reg(UInt(counterWidth bits)) init(0)
    val byteCounter = Reg(UInt(4 bits)) init(0)

    val deserializer = Deserializer(
        dataType = UInt(1 bits),
        width = 8,
        outputWidth = 8,
        resetFunction = (register: UInt) => {
            register init(0)
        },
        defaultFunction = (register: UInt) => {
            register := 0
        }
    )
 
    val bit = bitCounter === bitCount - 1
    val sample = bitCounter === bitCount / 2
    val started = byteCounter =/= 0
    val start = ((byteCounter === 1 && bitCounter === U(bitCounter.range -> true)) || ~started) && io.serial === 0

    when (start || bit) {
        bitCounter := 0
    } otherwise {
        bitCounter := bitCounter + 1
    }

    when (start) {
        byteCounter := 10
    } elsewhen (bit && started) {
        byteCounter := byteCounter - 1
    }

    deserializer.io.load := sample && started
    deserializer.io.shift := sample && started
    deserializer.io.input := io.serial

    io.data.payload := deserializer.io.output.asBits.asUInt
    io.data.valid := byteCounter === 1 && sample && io.serial === 1
}

object ReceiverVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/uart/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            Receiver(UARTParameters())
        )
    }
}

object ReceiverSimulation {
    def test(
        dut: Receiver
    ) = {
        def driver(
            dut: Receiver,
            queue: Queue[Int]
        ) = {
            val rand = new scala.util.Random
            val baudPeriod = dut.parameters.clockFrequency / dut.parameters.baudRate

            while (true) {
                var buffer = rand.nextInt(256)
                queue.enqueue(buffer)

                dut.io.serial #= 0
                dut.clockDomain.waitSampling(baudPeriod)

                for (bit <- 0 to 7) {
                    dut.io.serial #= ((buffer >> bit) & 1)
                    dut.clockDomain.waitSampling(baudPeriod)
                }

                dut.io.serial #= 1
                dut.clockDomain.waitSampling(baudPeriod)
            }
        }

        def monitor(
            dut: Receiver,
            queue: Queue[Int]
        ) = {
            val baudPeriod = dut.parameters.clockFrequency / dut.parameters.baudRate

            waitUntil(dut.io.data.valid.toBoolean == false)

            while (true) {
                dut.clockDomain.waitSampling()

                if (dut.io.data.valid.toBoolean) {
                    assert(dut.io.data.payload.toInt == queue.dequeue())
                }
            }
        }

        val queue = Queue[Int]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(dut.parameters.clockFrequency))
        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, queue))

        dut.clockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .compile(
                                    Receiver(
                                        UARTParameters(
                                            baudRate = 7372800
                                        )
                                    )
                                )

        compiled.doSim(dut => test(dut))
    }
}