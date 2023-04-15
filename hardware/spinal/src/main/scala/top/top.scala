package sockat.top

import spinal.core._

import sockat.primitives.{MMCME2_ADV, MMCME2_ADVParameters}
import sockat.uart.{UART, UARTParameters, UARTSerial}

case class Top (
    parameters: TopParameters,
) extends Component {
    val io = new Bundle {
        val uart = UARTSerial()
    }

    val uartClock = MMCME2_ADV(
        parameters = MMCME2_ADVParameters(
            clkIn1Period = 1e9 / parameters.clockFrequency,
            divClkDivide = 9,
            clkFbOutMultF = 62.375,
            clkOut0DivideF = 5.875,
        ),
    )

    uartClock.noPhaseShift()
    uartClock.noDynamicReconfiguration()
    uartClock.io.clkIn1 := ClockDomain.current.readClockWire
    uartClock.io.clkIn2 := False
    uartClock.io.clkInSel := 1
    uartClock.io.rst := ClockDomain.current.readResetWire
    uartClock.io.pwrDwn := False
    uartClock.io.clkFbIn := uartClock.io.clkFbOut

    val reset = ClockDomain.current.readResetWire || ~uartClock.io.locked

    val uartClockDomain = ClockDomain(
        clock = uartClock.io.clkOut0,
        reset = reset,
        frequency = FixedFrequency(HertzNumber(uartClock.getFrequencyMultiplier(0) * parameters.clockFrequency)),
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH,
        ),
    )

    val uartClockArea = new ClockingArea(uartClockDomain) {
        val uart = UART(
            parameters = parameters.uartParameters,
        )

        uart.io.data.transmit <-/< uart.io.data.receive
    }

    io.uart <> uartClockArea.uart.io.serial
}

object TopV {
    def main(
        args: Array[String]
    ) = {
        val TopV = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/top/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            ),
        ).generate(
            Top(TopParameters()),
        )
    }
}