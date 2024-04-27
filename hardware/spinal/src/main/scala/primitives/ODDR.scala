package sockat.primitives

import spinal.core._
import spinal.lib._

case class ODDRParameters (
    ddrClkEdge: String = "OPPOSITE_EDGE",
    init: Int = 0,
    srType: String = "SYNC"
)

case class ODDR (
    parameters: ODDRParameters,
) extends BlackBox {
    addGeneric("DDR_CLK_EDGE", parameters.ddrClkEdge)
    addGeneric("INIT", parameters.init)
    addGeneric("SRTYPE", parameters.srType)

    val io = new Bundle {
        val q = out Bool()
        val c = in Bool()
        val ce = in Bool()
        val d1 = in Bool()
        val d2 = in Bool()
        val r = in Bool()
        val s = in Bool()
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/ODDR.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}