package sockat.transactors

import spinal.core._

case class ClockParameters (
    period: Double = 10,
    phase: Int = 0,
)

case class Clock (
    parameters: ClockParameters,
) extends BlackBox {
    addGeneric("PERIOD", parameters.period)
    addGeneric("PHASE", parameters.phase)

    val io = new Bundle {
        val clk = out Bool()
    }

    noIoPrefix()
}