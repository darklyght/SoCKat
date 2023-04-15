package sockat.transactors

import spinal.core._

case class ResetParameters (
    cycles: Int = 10,
)

case class Reset (
    parameters: ResetParameters,
) extends BlackBox {
    addGeneric("CYCLES", parameters.cycles)

    val io = new Bundle {
        val clk = in Bool()
        val reset = out Bool()
    }

    noIoPrefix()
}