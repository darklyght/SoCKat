package sockat.primitives

import spinal.core._
import spinal.lib._

case class IOBUFParameters (
    drive: Int = 12,
    ioStandard: String = "DEFAULT",
    slew: String = "SLOW"
)

case class IOBUF (
    parameters: IOBUFParameters,
) extends BlackBox {
    addGeneric("DRIVE", parameters.drive)
    addGeneric("IOSTANDARD", parameters.ioStandard)
    addGeneric("SLEW", parameters.slew)

    val io = new Bundle {
        val o = out Bool()
        val io = inout(Analog(Bool()))
        val i = in Bool()
        val t = in Bool()
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/IOBUF.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}