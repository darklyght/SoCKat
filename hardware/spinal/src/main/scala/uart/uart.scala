package sockat.uart

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue

case class UARTSerial (
) extends Bundle {
    val transmit = out UInt(1 bits)
    val receive = in UInt(1 bits)
}

case class UARTData (
) extends Bundle with IMasterSlave {
    val transmit = Stream(UInt(8 bits))
    val receive = Stream(UInt(8 bits))

    override def asMaster() = {
        master(transmit)
        slave(receive)
    }
}

case class UART (
    parameters: UARTParameters,
) extends Component {
    val io = new Bundle {
        val data = slave(UARTData())
        val serial = UARTSerial()
    }

    val transmitter = Transmitter(parameters)
    val receiver = Receiver(parameters)

    io.data.transmit <> transmitter.io.data
    io.data.receive <> receiver.io.data.toStream

    io.serial.transmit <> transmitter.io.serial
    io.serial.receive <> receiver.io.serial
}

object UARTVerilog {
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
            UART(UARTParameters())
        )
    }
}

object UARTSimulation {
    def test(
        dut: UART
    ) = {
        def driver(
            dut: UART,
            queue: Queue[Int]
        ) = {
            while (true) {
                dut.io.serial.receive #= dut.io.serial.transmit.toInt

                dut.io.data.transmit.valid.randomize()
                dut.io.data.transmit.payload.randomize()
                dut.clockDomain.waitSampling()

                if (dut.io.data.transmit.valid.toBoolean && dut.io.data.transmit.ready.toBoolean) {
                    queue.enqueue(dut.io.data.transmit.payload.toInt)
                }
            }
        }

        def monitor(
            dut: UART,
            queue: Queue[Int]
        ) = {
            while (true) {
                dut.io.data.receive.ready #= true

                dut.clockDomain.waitSampling()

                if (dut.io.data.receive.valid.toBoolean && dut.io.data.receive.ready.toBoolean) {
                    assert(dut.io.data.receive.payload.toInt == queue.dequeue())
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
                                    UART(
                                        UARTParameters(
                                            baudRate = 7372800
                                        )
                                    )
                                )
        
        compiled.doSim(dut => test(dut))
    }
}