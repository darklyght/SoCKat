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
    val cs_n = Vec.fill(parameters.device.RANKS)(Bool())
    val ras_n = Bool()
    val cas_n = Bool()
    val we_n = Bool()
    val ba = UInt(parameters.device.BA_BITS bits)
    val addr = UInt(parameters.device.ADDR_BITS bits)

    override def asMaster() = {
        out(cs_n)
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
    val command = master(DeviceCommand(parameters))
    val odt = out Bool()
    val dq = inout(Vec.fill(parameters.dqParallel)(Analog(UInt(parameters.device.DQ_BITS bits))))
    val dm = inout(Vec.fill(parameters.dqParallel)(Analog(UInt(parameters.device.DM_BITS bits))))
    val dqs = inout(Analog(differential(UInt(parameters.device.DQS_BITS bits))))
}

case class DeviceInternal (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val rst_n = Bool()
    val ck = Bool()
    val cke = Bool()
    val command = DeviceCommand(parameters)
    val odt = Bool()
    val dq = Vec.fill(parameters.dqParallel)(TriStateArray(parameters.device.DQ_BITS bits))
    val dm = Vec.fill(parameters.dqParallel)(TriStateArray(parameters.device.DM_BITS bits))
    val dqs = TriStateArray(parameters.device.DQS_BITS bits)

    override def asMaster() = {
        out(rst_n)
        out(ck)
        out(cke)
        master(command)
        out(odt)
        dq.foreach(master(_))
        dm.foreach(master(_))
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
    val cs_n = Seq.fill(parameters.device.RANKS) {
        OBUF(OBUFParameters())
    }
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
    val dq = Seq.fill(parameters.dqParallel) {
        Seq.fill(parameters.device.DQ_BITS) {
            IOBUF(IOBUFParameters())
        }
    }
    val dm = Seq.fill(parameters.dqParallel) {
        Seq.fill(parameters.device.DM_BITS) {
            IOBUF(IOBUFParameters())
        }
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

    cs_n.zipWithIndex.foreach({case (obuf, i) => {
        obuf.io.i := io.internal.command.cs_n(i)
        io.device.command.cs_n(i) := obuf.io.o
    }})

    ras_n.io.i := io.internal.command.ras_n
    io.device.command.ras_n := ras_n.io.o

    cas_n.io.i := io.internal.command.cas_n
    io.device.command.cas_n := cas_n.io.o

    we_n.io.i := io.internal.command.we_n
    io.device.command.we_n := we_n.io.o

    odt.io.i := io.internal.odt
    io.device.odt := odt.io.o

    ba.zipWithIndex.foreach({case (obuf, i) => {
        obuf.io.i := io.internal.command.ba(i)
        io.device.command.ba(i) := obuf.io.o
    }})

    addr.zipWithIndex.foreach({case (obuf, i) => {
        obuf.io.i := io.internal.command.addr(i)
        io.device.command.addr(i) := obuf.io.o
    }})

    dq.zipWithIndex.foreach({case (chip, i) => {
        chip.zipWithIndex.foreach({case (iobuf, j) => {
            iobuf.io.i := io.internal.dq(i)(j).write
            iobuf.io.t := ~io.internal.dq(i)(j).writeEnable
            io.internal.dq(i)(j).read := iobuf.io.o
            io.device.dq(i)(j) := iobuf.io.io
        }})
    }})

    dm.zipWithIndex.foreach({case (chip, i) => {
        chip.zipWithIndex.foreach({case (iobuf, j) => {
            iobuf.io.i := io.internal.dm(i)(j).write
            iobuf.io.t := ~io.internal.dm(i)(j).writeEnable
            io.internal.dm(i)(j).read := iobuf.io.o
            io.device.dm(i)(j) := iobuf.io.io
        }})
    }})
    
    dqs.zipWithIndex.foreach({case (iobufds, index) => {
        iobufds.io.i := io.internal.dqs(index).write
        iobufds.io.t := ~io.internal.dqs(index).writeEnable
        io.internal.dqs(index).read := iobufds.io.o
        io.device.dqs.p(index) := iobufds.io.io
        io.device.dqs.n(index) := iobufds.io.iob
    }})
}

case class IOCellsSimulationModel (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val internal = slave(DeviceInternal(parameters))
    }

    val model = Seq.fill(parameters.device.RANKS) {
        Seq.fill(parameters.dqParallel) {
            ddr3(parameters)
        }
    }
    val ioCells = IOCells(parameters)

    ioCells.io.internal <> io.internal

    (0 until parameters.device.RANKS).foreach(i => {
        (0 until parameters.dqParallel).foreach(j => {
            model(i)(j).io.rst_n := ioCells.io.device.rst_n
            model(i)(j).io.ck := ioCells.io.device.ck.p
            model(i)(j).io.ck_n := ioCells.io.device.ck.n
            model(i)(j).io.cke := ioCells.io.device.cke
            model(i)(j).io.cs_n := ioCells.io.device.command.cs_n(i)
            model(i)(j).io.ras_n := ioCells.io.device.command.ras_n
            model(i)(j).io.cas_n := ioCells.io.device.command.cas_n
            model(i)(j).io.we_n := ioCells.io.device.command.we_n
            model(i)(j).io.dm_tdqs := ioCells.io.device.dm(j)
            model(i)(j).io.ba := ioCells.io.device.command.ba
            model(i)(j).io.addr := ioCells.io.device.command.addr
            model(i)(j).io.dq := ioCells.io.device.dq(j)
            model(i)(j).io.dqs := ioCells.io.device.dqs.p
            model(i)(j).io.dqs_n := ioCells.io.device.dqs.n
            model(i)(j).io.odt := ioCells.io.device.odt
        })
    })
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
            IOCellsSimulationModel(DDR3Parameters())
        )
    }
}

object IOCellsSimulation {
    def test(
        dut: IOCellsSimulationModel
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
                                    IOCellsSimulationModel(DDR3Parameters())
                                )

        compiled.doSim(dut => test(dut))
    }
}
