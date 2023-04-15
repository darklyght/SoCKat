package sockat.uart

import spinal.core._
import spinal.lib.{Stream, Flow, IMasterSlave, master, slave, StreamFifoCC}

case class UARTSerial (
) extends Bundle {
    val transmit = out UInt(1 bits)
    val receive = in UInt(1 bits)
}

case class UARTData (
) extends Bundle with IMasterSlave {
    val transmit = Stream(UInt(8 bits))
    val receive = Stream(UInt(8 bits))

    override def asMaster(): Unit = {
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

object UARTV {
    def main(
        args: Array[String]
    ) = {
        val UARTV = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/uart/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            ),
        ).generate(
            UART(UARTParameters()),
        )
    }
}