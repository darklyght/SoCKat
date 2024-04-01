package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.io._

import sockat.models._
import sockat.primitives._
import sockat.utilities._

case class DeviceCommand (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val ras_n = Bool()
    val cas_n = Bool()
    val we_n = Bool()
    val ba = UInt(parameters.device.BA_BITS bits)
    val addr = UInt(parameters.device.ADDR_BITS bits)

    override def asMaster() = {
        out(ras_n)
        out(cas_n)
        out(we_n)
        out(ba)
        out(addr)
    }
}

case class Device (
    parameters: DDR3Parameters
) extends Bundle {
    val rst_n = out Bool()
    val ck = out(differential(Bool()))
    val cke = out Bool()
    val cs_n = out Bool()
    val command = master(DeviceCommand(parameters))
    val odt = out Bool()
    val dq = inout(Analog(UInt(parameters.device.DQ_BITS bits)))
    val dm = inout(Analog(UInt(parameters.device.DM_BITS bits)))
    val dqs = inout(Analog(differential(UInt(parameters.device.DQS_BITS bits))))
}

case class DeviceInternal (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val rst_n = Bool()
    val ck = Bool()
    val cke = Bool()
    val cs_n = Bool()
    val command = DeviceCommand(parameters)
    val odt = Bool()
    val dq = TriStateArray(parameters.device.DQ_BITS bits)
    val dm = TriStateArray(parameters.device.DM_BITS bits)
    val dqs = TriStateArray(parameters.device.DQS_BITS bits)

    override def asMaster() = {
        out(rst_n)
        out(ck)
        out(cke)
        out(cs_n)
        master(command)
        out(odt)
        master(dq)
        master(dm)
        master(dqs)
    }
}

case class IOCells (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val device = Device(parameters)
        val internal = slave(DeviceInternal(parameters))
    }

    val rst_n = OBUF(OBUFParameters())
    val ck = OBUFDS(OBUFDSParameters())
    val cke = OBUF(OBUFParameters())
    val cs_n = OBUF(OBUFParameters())
    val ras_n = OBUF(OBUFParameters())
    val cas_n = OBUF(OBUFParameters())
    val we_n = OBUF(OBUFParameters())
    val odt = OBUF(OBUFParameters())
    val ba = Seq.fill(parameters.device.BA_BITS) {
        OBUF(OBUFParameters())
    }
    val addr = Seq.fill(parameters.device.ADDR_BITS) {
        OBUF(OBUFParameters())
    }
    val dq = Seq.fill(parameters.device.DQ_BITS) {
        IOBUF(IOBUFParameters())
    }
    val dm = Seq.fill(parameters.device.DM_BITS) {
        IOBUF(IOBUFParameters())
    }
    val dqs = Seq.fill(parameters.device.DQS_BITS) {
        IOBUFDS(IOBUFDSParameters())
    }

    rst_n.io.i := io.internal.rst_n
    io.device.rst_n := rst_n.io.o

    ck.io.i := io.internal.ck
    io.device.ck.p := ck.io.o
    io.device.ck.n := ck.io.ob

    cke.io.i := io.internal.cke
    io.device.cke := cke.io.o

    cs_n.io.i := io.internal.cs_n
    io.device.cs_n := cs_n.io.o

    ras_n.io.i := io.internal.command.ras_n
    io.device.command.ras_n := ras_n.io.o

    cas_n.io.i := io.internal.command.cas_n
    io.device.command.cas_n := cas_n.io.o

    we_n.io.i := io.internal.command.we_n
    io.device.command.we_n := we_n.io.o

    odt.io.i := io.internal.odt
    io.device.odt := odt.io.o

    ba.zipWithIndex.foreach({case (obuf, index) => {
        obuf.io.i := io.internal.command.ba(index)
        io.device.command.ba(index) := obuf.io.o
    }})

    addr.zipWithIndex.foreach({case (obuf, index) => {
        obuf.io.i := io.internal.command.addr(index)
        io.device.command.addr(index) := obuf.io.o
    }})

    dq.zipWithIndex.foreach({case (iobuf, index) => {
        iobuf.io.i := io.internal.dq(index).write
        iobuf.io.t := ~io.internal.dq(index).writeEnable
        io.internal.dq(index).read := iobuf.io.o
        io.device.dq(index) := iobuf.io.io
    }})

    dm.zipWithIndex.foreach({case (iobuf, index) => {
        iobuf.io.i := io.internal.dm(index).write
        iobuf.io.t := ~io.internal.dm(index).writeEnable
        io.internal.dm(index).read := iobuf.io.o
        io.device.dm(index) := iobuf.io.io
    }})

    dqs.zipWithIndex.foreach({case (iobufds, index) => {
        iobufds.io.i := io.internal.dqs(index).write
        iobufds.io.t := ~io.internal.dqs(index).writeEnable
        io.internal.dqs(index).read := iobufds.io.o
        io.device.dqs.p(index) := iobufds.io.io
        io.device.dqs.n(index) := iobufds.io.iob
    }})
}

case class IOCellsDDR3 (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val internal = slave(DeviceInternal(parameters))
    }

    val model = ddr3(parameters)
    val ioCells = IOCells(parameters)

    ioCells.io.internal <> io.internal

    model.io.rst_n := ioCells.io.device.rst_n
    model.io.ck := ioCells.io.device.ck.p
    model.io.ck_n := ioCells.io.device.ck.n
    model.io.cke := ioCells.io.device.cke
    model.io.cs_n := ioCells.io.device.cs_n
    model.io.ras_n := ioCells.io.device.command.ras_n
    model.io.cas_n := ioCells.io.device.command.cas_n
    model.io.we_n := ioCells.io.device.command.we_n
    model.io.dm_tdqs := ioCells.io.device.dm
    model.io.ba := ioCells.io.device.command.ba
    model.io.addr := ioCells.io.device.command.addr
    model.io.dq := ioCells.io.device.dq
    model.io.dqs := ioCells.io.device.dqs.p
    model.io.dqs_n := ioCells.io.device.dqs.n
    model.io.odt := ioCells.io.device.odt
}

object IOCellsVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/ddr3/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            IOCellsDDR3(DDR3Parameters())
        )
    }
}

object IOCellsSimulation {
    def test(
        dut: IOCellsDDR3
    ) = {
    }

    def main(
        args: Array[String]
    ) = {
        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .addSimulatorFlag("-D den4096Mb")
                                .addSimulatorFlag("-D sg187E")
                                .addSimulatorFlag("-D x16")
                                .addSimulatorFlag("-g2012")
                                .addSimulatorFlag("-s glbl")
                                .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                .compile(
                                    IOCellsDDR3(DDR3Parameters())
                                )

        compiled.doSim(dut => test(dut))
    }
}
