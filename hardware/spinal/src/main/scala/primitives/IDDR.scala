package sockat.primitives

import spinal.core._
import spinal.lib._

case class IDDRParameters (
    ddrClkEdge: String = "OPPOSITE_EDGE",
    initQ1: Int = 0,
    initQ2: Int = 0,
    srType: String = "SYNC"
)

case class IDDR (
    parameters: IDDRParameters,
) extends BlackBox {
    addGeneric("DDR_CLK_EDGE", parameters.ddrClkEdge)
    addGeneric("INIT_Q1", parameters.initQ1)
    addGeneric("INIT_Q2", parameters.initQ2)
    addGeneric("SRYTPE", parameters.srType)

    val io = new Bundle {
        val q1 = out Bool()
        val q2 = out Bool()
        val c = in Bool()
        val ce = in Bool()
        val d = in Bool()
        val r = in Bool()
        val s = in Bool()
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/IDDR.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}