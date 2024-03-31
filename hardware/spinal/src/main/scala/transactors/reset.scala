package sockat.transactors

import spinal.core._

case class SimulationResetParameters (
    cycles: Int = 10
)

case class SimulationReset (
    parameters: SimulationResetParameters
) extends BlackBox {
    addGeneric("CYCLES", parameters.cycles)

    val io = new Bundle {
        val clk = in Bool()
        val reset = out Bool()
    }

    noIoPrefix()
}