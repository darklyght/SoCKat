package sockat.top

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import sockat.primitives._
import sockat.transactors._
import sockat.uart._
import sockat.utilities._

case class Top (
    parameters: TopParameters,
    topClockDomain: ClockDomain
) extends Component {
    val io = new Bundle {
        val uart = UARTSerial()
    }

    val topClockArea = new ClockingArea(topClockDomain) {
        val uartClock = MMCME2_ADV(
            parameters = MMCME2_ADVParameters(
                clkIn1Period = 1e9 / parameters.clockFrequency,
                divClkDivide = 9,
                clkFbOutMultF = 62.375,
                clkOut0DivideF = 5.875
            )
        )

        uartClock.noPhaseShift()
        uartClock.noDynamicReconfiguration()
        uartClock.io.clkIn1 := ClockDomain.current.readClockWire
        uartClock.io.clkIn2 := False
        uartClock.io.clkInSel := 1
        uartClock.io.rst := ~ClockDomain.current.readResetWire
        uartClock.io.pwrDwn := False
        uartClock.io.clkFbIn := uartClock.io.clkFbOut

        val uartResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
        uartResetSynchronizer.io.clock := uartClock.io.clkOut0
        uartResetSynchronizer.io.async := ~ClockDomain.current.readResetWire || ~uartClock.io.locked

        val uartClockDomain = ClockDomain(
            clock = uartClock.io.clkOut0,
            reset = uartResetSynchronizer.io.sync,
            clockEnable = True,
            frequency = FixedFrequency(HertzNumber(uartClock.getFrequencyMultiplier(0) * parameters.clockFrequency)),
            config = ClockDomainConfig(
                clockEdge = RISING,
                resetKind = ASYNC,
                resetActiveLevel = HIGH,
                clockEnableActiveLevel = HIGH
            )
        )

        val uart = UART(
            parameters = parameters.uartParameters,
            uartClockDomain = uartClockDomain
        )

        uart.io.data.transmit <-/< uart.io.data.receive

        io.uart <> uart.io.serial
    }
}

case class TopSimulationModel (
    parameters: TopParameters
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

    val uartResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    uartResetSynchronizer.io.clock := uartClock.io.clk
    uartResetSynchronizer.io.async := ~reset.io.reset

    val topClockDomain = ClockDomain(
        clock = topClock.io.clk,
        reset = reset.io.reset,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = LOW,
            clockEnableActiveLevel = HIGH
        )
    )

    val uartClockDomain = ClockDomain(
        clock = uartClock.io.clk,
        reset = uartResetSynchronizer.io.sync,
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
            parameters = parameters,
            topClockDomain = topClockDomain
        )

        val uartTransactor = UARTTransactor(
            parameters = UARTTransactorParameters(
                vpiParameters = UARTVPIParameters(
                    name = "uart0"
                ),
                uartParameters = UARTParameters(
                    clockFrequency = 117964800,
                    baudRate = 7372800
                )
            ),
            uartClockDomain = uartClockDomain
        )
    }

    topClockArea.top.io.uart.transmit <> topClockArea.uartTransactor.io.serial.receive
    topClockArea.top.io.uart.receive <> topClockArea.uartTransactor.io.serial.transmit
}

object TopVerilog {
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
            Top(
                parameters = TopParameters(),
                topClockDomain = ClockDomain.external(
                    name = "topClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = LOW,
                        clockEnableActiveLevel = HIGH
                    )
                )
            )
        )
    }
}

object TopSimulation {
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
            TopSimulationModel(
                TopParameters(
                    clockFrequency = 100000000,
                    uartParameters = UARTParameters(
                        clockFrequency = 117964800,
                        baudRate = 7372800
                    )
                )
            )
        )
    }
}