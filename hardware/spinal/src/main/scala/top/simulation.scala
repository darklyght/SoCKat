package sockat.top

import spinal.core._

import sockat.transactors._
import sockat.uart._

case class Simulation (
) extends Component {
    val topClock = SimulationClock(
        parameters = SimulationClockParameters(
            period = 10,
            phase = 0
        )
    )

    val uartClock = SimulationClock(
        parameters = SimulationClockParameters(
            period = 8.477,
            phase = 0
        )
    )

    val reset = SimulationReset(
        parameters = SimulationResetParameters(
            cycles = 100
        )
    )

    reset.io.clk := topClock.io.clk

    val topClockDomain = ClockDomain(
        clock = topClock.io.clk,
        reset = reset.io.reset,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val uartClockDomain = ClockDomain(
        clock = uartClock.io.clk,
        reset = reset.io.reset,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val topClockArea = new ClockingArea(topClockDomain) {
        val top = Top(
            parameters = TopParameters(
                clockFrequency = 100000000,
                uartParameters = UARTParameters(
                    clockFrequency = 117964800,
                    baudRate = 7372800
                )
            )
        )
    }

    val uartClockArea = new ClockingArea(uartClockDomain) {
        val uartTransactor = UARTTransactor(
            parameters = UARTTransactorParameters(
                vpiParameters = UARTVPIParameters(
                    name = "uart0"
                ),
                uartParameters = UARTParameters(
                    clockFrequency = 117964800,
                    baudRate = 7372800
                )
            )
        )
    }

    topClockArea.top.io.uart.transmit <> uartClockArea.uartTransactor.io.serial.receive
    topClockArea.top.io.uart.receive <> uartClockArea.uartTransactor.io.serial.transmit
}

object SimulationVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/top/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            Simulation()
        )
    }
}