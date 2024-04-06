package sockat.ddr3

import spinal.core._
import spinal.lib._

import sockat.primitives._
import sockat.utilities._

case class PHYInterface (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val deviceReset = Bool()
    val ready = Bool()
    val read = ReadPathInterface(parameters)
    val write = WritePathInterface(parameters)
    val readPhaseUpdate = PhaseShiftInterface()
    val dqsPhaseUpdate = Stream(PhaseUpdateInterface())

    override def asMaster() = {
        out(deviceReset)
        in(ready)
        master(read)
        master(write)
        master(readPhaseUpdate)
        master(dqsPhaseUpdate)
    }
}

case class PHY (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val internal = slave(PHYInterface(parameters))
        val device = Device(parameters)
    }

    val clockGenerator = MMCME2_ADV(MMCME2_ADVParameters(
        clkIn1Period = (2 * parameters.tCKPeriod).toInt,
        clkFbOutMultF = 8,
        clkOut0DivideF = 2,
        clkOut1Divide = 2,
        clkOut1UseFinePs = "TRUE",
        clkOut2Divide = 2,
        clkOut2Phase = 270,
        divClkDivide = 2
    ))

    val dqsClockGenerator = ClockGenerator(ClockGeneratorParameters(
        mmcm = MMCME2_ADVParameters(
            clkIn1Period = (2 * parameters.tCKPeriod).toInt,
            clkFbOutMultF = 8,
            clkOut0DivideF = 2,
            clkOut1Divide = 2,
            divClkDivide = 2
        )
    ))

    val readClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut1,
        reset = ClockDomain.current.readResetWire,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val writeClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut2,
        reset = ClockDomain.current.readResetWire,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val readClockArea = new ClockingArea(readClockDomain) {
        val readPath = ReadPath(parameters)
    }

    val writeClockArea = new ClockingArea(writeClockDomain) {
        val writePath = WritePath(parameters)
    }

    val ioCells = IOCells(parameters)

    clockGenerator.noDynamicReconfiguration()

    clockGenerator.io.clkIn1 := ClockDomain.current.readClockWire
    clockGenerator.io.clkIn2 := False
    clockGenerator.io.clkInSel := 1
    clockGenerator.io.clkFbIn := clockGenerator.io.clkFbOut
    clockGenerator.io.rst := ClockDomain.current.readResetWire
    clockGenerator.io.pwrDwn := False
    clockGenerator.io.phaseShift <> io.internal.readPhaseUpdate

    dqsClockGenerator.io.phaseUpdate <> io.internal.dqsPhaseUpdate

    readClockArea.readPath.io.controllerClock := ClockDomain.current.readClockWire
    readClockArea.readPath.io.device.dq.read := ioCells.io.internal.dq.read
    readClockArea.readPath.io.device.dm.read := ioCells.io.internal.dm.read
    readClockArea.readPath.io.device.dqs.read := ioCells.io.internal.dqs.read
    readClockArea.readPath.io.internal <> io.internal.read

    writeClockArea.writePath.io.dqsClock := Vec((0 until parameters.device.DQS_BITS).map((index) => {
        dqsClockGenerator.io.clocks(index).p
    }))
    ioCells.io.internal.cke := writeClockArea.writePath.io.device.cke
    ioCells.io.internal.cs_n := writeClockArea.writePath.io.device.cs_n
    ioCells.io.internal.command := writeClockArea.writePath.io.device.command
    ioCells.io.internal.odt := writeClockArea.writePath.io.device.odt
    ioCells.io.internal.dq.write := writeClockArea.writePath.io.device.dq.write
    ioCells.io.internal.dq.writeEnable := writeClockArea.writePath.io.device.dq.writeEnable
    ioCells.io.internal.dm.write := writeClockArea.writePath.io.device.dm.write
    ioCells.io.internal.dm.writeEnable := writeClockArea.writePath.io.device.dm.writeEnable
    ioCells.io.internal.dqs.write := writeClockArea.writePath.io.device.dqs.write
    ioCells.io.internal.dqs.writeEnable := writeClockArea.writePath.io.device.dqs.writeEnable
    writeClockArea.writePath.io.device.dq.read := ioCells.io.internal.dq.read
    writeClockArea.writePath.io.internal <> io.internal.write

    ioCells.io.internal.ck := clockGenerator.io.clkOut0
    ioCells.io.internal.rst_n := ~io.internal.deviceReset
    ioCells.io.device <> io.device

    io.internal.ready := clockGenerator.io.locked
}

object PHYInterfaceVerilog {
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
            PHY(DDR3Parameters())
        )
    }
}