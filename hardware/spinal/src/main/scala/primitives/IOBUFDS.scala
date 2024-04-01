package sockat.primitives

import spinal.core._
import spinal.lib._

case class IOBUFDSParameters (
    diffTerm: String = "FALSE",
    ibufLowPwr: String = "TRUE",
    ioStandard: String = "DEFAULT",
    slew: String = "SLOW"
)

case class IOBUFDS (
    parameters: IOBUFDSParameters,
) extends BlackBox {
    addGeneric("DIFF_TERM", parameters.diffTerm)
    addGeneric("IBUF_LOW_PWR", parameters.ibufLowPwr)
    addGeneric("IOSTANDARD", parameters.ioStandard)
    addGeneric("SLEW", parameters.slew)

    val io = new Bundle {
        val o = out Bool()
        val io = inout(Analog(Bool()))
        val iob = inout(Analog(Bool()))
        val i = in Bool()
        val t = in Bool()
    }

    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/glbl.v")
    addRTLPath("../sim/lib/XilinxUnisimLibrary/verilog/src/unisims/IOBUFDS.v")

    private def renameIO() = {
        io.flatten.foreach(bt => {
            bt.setName(bt.getName().toUpperCase())
        })
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}