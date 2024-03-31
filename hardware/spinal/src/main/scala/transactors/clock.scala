package sockat.transactors

import spinal.core._

case class SimulationClockParameters (
    period: Double = 10,
    phase: Int = 0
)

case class SimulationClock (
    parameters: SimulationClockParameters
) extends BlackBox {
    addGeneric("PERIOD", parameters.period)
    addGeneric("PHASE", parameters.phase)

    val io = new Bundle {
        val clk = out Bool()
    }

    noIoPrefix()
}