package sockat.primitives

import spinal.core._
import spinal.lib._

case class OBUFDSParameters (
    ioStandard: String = "DEFAULT",
    slew: String = "SLOW"
)

case class OBUFDS (
    parameters: OBUFDSParameters,
) extends BlackBox {
    addGeneric("IOSTANDARD", parameters.ioStandard)
    addGeneric("SLEW", parameters.slew)

    val io = new Bundle {
        val o = out UInt(1 bits)
        val ob = out UInt(1 bits)
        val i = in UInt(1 bits)
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/OBUFDS.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}