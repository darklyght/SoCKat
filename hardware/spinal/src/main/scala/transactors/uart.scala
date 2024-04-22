package sockat.transactors

import spinal.core._

import spinal.lib._
import sockat.uart._

case class UARTVPIParameters (
    name: String = "uart",
)

case class UARTVPI (
    parameters: UARTVPIParameters
) extends BlackBox {
    addGeneric("NAME", parameters.name)

    val io = new Bundle {
        val clk = in Bool()
        val data = master(UARTData())
    }

    private def renameIO() = {
        io.flatten.foreach(bt => {
            if (bt.getName().contains("data_")) bt.setName(bt.getName().replace("data_", ""))
            if (bt.getName().contains("_payload")) bt.setName(bt.getName().replace("_payload", "_data"))
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())

    addRTLPath("../sim/transactors/uart/hdl/UARTVPI.v")
    addRTLPath("../sim/transactors/uart/src/uart.c")
    addRTLPath("../sim/transactors/uart/src/receive.c")
    addRTLPath("../sim/transactors/uart/src/transmit.c")
}

case class UARTTransactorParameters (
    vpiParameters: UARTVPIParameters,
    uartParameters: UARTParameters
)

case class UARTTransactor (
    parameters: UARTTransactorParameters,
    uartClockDomain: ClockDomain
) extends Component {
    val io = new Bundle {
        val serial = UARTSerial()
    }

    val vpi = UARTVPI(parameters.vpiParameters)
    val uart = UART(
        parameters = parameters.uartParameters,
        uartClockDomain = uartClockDomain
    )

    vpi.io.clk := ClockDomain.current.readClockWire
    uart.io.data <> vpi.io.data
    io.serial <> uart.io.serial
}