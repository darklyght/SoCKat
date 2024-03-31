package sockat.primitives

import spinal.core._
import spinal.lib._

case class OBUFParameters (
    drive: Int = 12,
    ioStandard: String = "DEFAULT",
    slew: String = "SLOW"
)

case class OBUF (
    parameters: OBUFParameters,
) extends BlackBox {
    addGeneric("DRIVE", parameters.drive)
    addGeneric("IOSTANDARD", parameters.ioStandard)
    addGeneric("SLEW", parameters.slew)

    val io = new Bundle {
        val o = out UInt(1 bits)
        val i = in UInt(1 bits)
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/OBUF.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}