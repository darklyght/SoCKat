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
    val ras_n = UInt(1 bits)
    val cas_n = UInt(1 bits)
    val we_n = UInt(1 bits)
    val ba = UInt(parameters.baWidth bits)
    val addr = UInt(parameters.addrWidth bits)

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
    val rst_n = out UInt(1 bits)
    val ck = out(differential(UInt(1 bits)))
    val cke = out UInt(1 bits)
    val cs_n = out UInt(1 bits)
    val command = master(DeviceCommand(parameters))
    val odt = out UInt(1 bits)
    val dq = inout(Analog(UInt(parameters.dqWidth bits)))
    val dm = inout(Analog(UInt(parameters.dmWidth bits)))
    val dqs = inout(Analog(differential(UInt(parameters.dqsWidth bits))))
}

case class DeviceInternal (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val rst_n = UInt(1 bits)
    val ck = UInt(1 bits)
    val cke = UInt(1 bits)
    val cs_n = UInt(1 bits)
    val command = DeviceCommand(parameters)
    val odt = UInt(1 bits)
    val dq = TriStateArray(parameters.dqWidth bits)
    val dm = TriStateArray(parameters.dmWidth bits)
    val dqs = TriStateArray(parameters.dqsWidth bits)

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
    val ba = Seq.fill(parameters.baWidth) {
        OBUF(OBUFParameters())
    }
    val addr = Seq.fill(parameters.addrWidth) {
        OBUF(OBUFParameters())
    }
    val dq = Seq.fill(parameters.dqWidth) {
        IOBUF(IOBUFParameters())
    }
    val dm = Seq.fill(parameters.dmWidth) {
        IOBUF(IOBUFParameters())
    }
    val dqs = Seq.fill(parameters.dqsWidth) {
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
        obuf.io.i := io.internal.command.ba(index).asUInt
        io.device.command.ba(index) := obuf.io.o.asBool
    }})

    addr.zipWithIndex.foreach({case (obuf, index) => {
        obuf.io.i := io.internal.command.addr(index).asUInt
        io.device.command.addr(index) := obuf.io.o.asBool
    }})

    dq.zipWithIndex.foreach({case (iobuf, index) => {
        iobuf.io.i := io.internal.dq(index).write.asUInt
        iobuf.io.t := ~io.internal.dq(index).writeEnable.asUInt
        io.internal.dq(index).read := iobuf.io.o.asBool
        io.device.dq(index) := iobuf.io.io.asBool
    }})

    dm.zipWithIndex.foreach({case (iobuf, index) => {
        iobuf.io.i := io.internal.dm(index).write.asUInt
        iobuf.io.t := ~io.internal.dm(index).writeEnable.asUInt
        io.internal.dm(index).read := iobuf.io.o.asBool
        io.device.dm(index) := iobuf.io.io.asBool
    }})

    dqs.zipWithIndex.foreach({case (iobufds, index) => {
        iobufds.io.i := io.internal.dqs(index).write.asUInt
        iobufds.io.t := ~io.internal.dqs(index).writeEnable.asUInt
        io.internal.dqs(index).read := iobufds.io.o.asBool
        io.device.dqs.p(index) := iobufds.io.io.asBool
        io.device.dqs.n(index) := iobufds.io.iob.asBool
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

    model.io.rst_n := ioCells.io.device.rst_n.asBool
    model.io.ck := ioCells.io.device.ck.p.asBool
    model.io.ck_n := ioCells.io.device.ck.n.asBool
    model.io.cke := ioCells.io.device.cke.asBool
    model.io.cs_n := ioCells.io.device.cs_n.asBool
    model.io.ras_n := ioCells.io.device.command.ras_n.asBool
    model.io.cas_n := ioCells.io.device.command.cas_n.asBool
    model.io.we_n := ioCells.io.device.command.we_n.asBool
    model.io.dm_tdqs := ioCells.io.device.dm
    model.io.ba := ioCells.io.device.command.ba
    model.io.addr := ioCells.io.device.command.addr
    model.io.dq := ioCells.io.device.dq
    model.io.dqs := ioCells.io.device.dqs.p
    model.io.dqs_n := ioCells.io.device.dqs.n
    model.io.odt := ioCells.io.device.odt.asBool
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
