package sockat.top

import spinal.core._

import sockat.transactors.{
    Clock,
    ClockParameters,
    Reset,
    ResetParameters,
    UARTTransactor,
    UARTTransactorParameters,
    UARTVPI,
    UARTVPIParameters
}
import sockat.uart.{
    UART,
    UARTParameters,
    UARTSerial
}

case class Simulation (
) extends Component {
    val topClock = Clock(
        parameters = ClockParameters(
            period = 10,
            phase = 0,
        )
    )

    val uartClock = Clock(
        parameters = ClockParameters(
            period = 8.477,
            phase = 0,
        )
    )

    val reset = Reset(
        parameters = ResetParameters(
            cycles = 100,
        )
    )

    reset.io.clk := topClock.io.clk

    val topClockDomain = ClockDomain(
        clock = topClock.io.clk,
        reset = reset.io.reset,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH,
        ),
    )

    val uartClockDomain = ClockDomain(
        clock = uartClock.io.clk,
        reset = reset.io.reset,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH,
        ),
    )

    val topClockArea = new ClockingArea(topClockDomain) {
        val top = Top(
            parameters = TopParameters(
                clockFrequency = 100000000,
                uartParameters = UARTParameters(
                    clockFrequency = 117964800,
                    baudRate = 7372800,
                )
            ),
        )
    }

    val uartClockArea = new ClockingArea(uartClockDomain) {
        val uartTransactor = UARTTransactor(
            parameters = UARTTransactorParameters(
                vpiParameters = UARTVPIParameters(
                    name = "uart0",
                ),
                uartParameters = UARTParameters(
                    clockFrequency = 117964800,
                    baudRate = 7372800,
                ),
            ),
        )
    }

    topClockArea.top.io.uart.transmit <> uartClockArea.uartTransactor.io.serial.receive
    topClockArea.top.io.uart.receive <> uartClockArea.uartTransactor.io.serial.transmit
}

object SimulationV {
    def main(
        args: Array[String]
    ) = {
        val SimulationV = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/top/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            ),
        ).generate(
            Simulation(),
        )
    }
}