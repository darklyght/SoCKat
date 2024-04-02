package sockat.uart

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue

import sockat.utilities._

case class Transmitter (
    parameters: UARTParameters
) extends Component {
    val io = new Bundle {
        val serial = out Bool()
        val data = slave Stream(UInt(8 bits))
    }

    val bitCount = parameters.clockFrequency / parameters.baudRate
    val counterWidth = log2Up(bitCount.toInt)

    val bitCounter = Reg(UInt(counterWidth bits)) init(0)
    val byteCounter = Reg(UInt(4 bits)) init(0)

    val serializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = 9,
            inputWidth = 9,
            resetFunction = (register: Bool) => {
                register init(True)
            },
            defaultFunction = (register: Bool) => {
                register := True
            }
        )
    )

    val sample = bitCounter === bitCount - 1
    val started = byteCounter =/= 0
    val start = ~started && io.data.valid

    when (start || sample) {
        bitCounter := 0
    } otherwise {
        bitCounter := bitCounter + 1
    }

    when (start) {
        byteCounter := 10
    } elsewhen (started && sample) {
        byteCounter := byteCounter - 1
    }

    serializer.io.load := start
    serializer.io.shift := started && sample
    serializer.io.input := Vec(
        False,
        io.data.payload(0 downto 0).asBool,
        io.data.payload(1 downto 1).asBool,
        io.data.payload(2 downto 2).asBool,
        io.data.payload(3 downto 3).asBool,
        io.data.payload(4 downto 4).asBool,
        io.data.payload(5 downto 5).asBool,
        io.data.payload(6 downto 6).asBool,
        io.data.payload(7 downto 7).asBool,
    )

    io.data.ready := ~started
    io.serial := serializer.io.output(0)
}

object TransmitterVerilog {
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
            Transmitter(
                UARTParameters()
            )
        )
    }
}

object TransmitterSimulation {
    def test(
        dut: Transmitter
    ) = {
        def driver(
            dut: Transmitter,
            queue: Queue[Int]
        ) = {
            while (true) {
                dut.io.data.valid.randomize()
                dut.io.data.payload.randomize()
                dut.clockDomain.waitSampling()
                
                if (dut.io.data.valid.toBoolean && dut.io.data.ready.toBoolean) {
                    queue.enqueue(dut.io.data.payload.toInt)
                }
            }
        }

        def monitor(
            dut: Transmitter,
            queue: Queue[Int]
        ) = {
            val baudPeriod = dut.parameters.clockFrequency / dut.parameters.baudRate

            waitUntil(dut.io.serial.toBoolean == true)

            while (true) {
                waitUntil(dut.io.serial.toBoolean == false)
                dut.clockDomain.waitSampling(baudPeriod / 2)

                assert(dut.io.serial.toBoolean == false)
                dut.clockDomain.waitSampling(baudPeriod)

                var buffer = 0
                for (bit <- 0 to 7) {
                    buffer = buffer | (dut.io.serial.toBoolean.toInt << bit)
                    dut.clockDomain.waitSampling(baudPeriod)
                }

                assert(dut.io.serial.toBoolean == true)
                assert(buffer == queue.dequeue())
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
                                    Transmitter(
                                        UARTParameters(
                                            baudRate = 7372800
                                        )
                                    )
                                )

        compiled.doSim(dut => test(dut))
    }
}