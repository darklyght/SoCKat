package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._

import sockat.models._

import scala.collection.mutable.Queue
import scala.util.Random

case class RankController (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val phy = master(PHYInterface(parameters))
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS,
                dataWidth = 128,
                idWidth = 8
            )
        ))
        val initializationDone = out Bool()
        val writeLevelingStart = in Bool()
    }

    val clockPeriod = parameters.controllerClockRatio * parameters.tCKPeriod

    val commandToggle = Reg(Bool()) init(False)
    val readToggle = Reg(Bool()) init(False)
    val writeToggle = Reg(Bool()) init(False)
    val writeLevelingToggle = Reg(Bool()) init(False)

    val readValidToggle = Reg(Bool())

    val counter = Reg(UInt(17 bits)) init(0)

    io.phy.deviceReset := False
    io.phy.read.readToggle := readToggle
    io.phy.write.cke := True
    io.phy.write.cs_n := False
    io.phy.write.odt := False
    io.phy.write.commandToggle := commandToggle
    io.phy.write.command.ras_n := True
    io.phy.write.command.cas_n := True
    io.phy.write.command.we_n := True
    io.phy.write.command.ba := 0
    io.phy.write.command.addr := 0
    io.phy.write.writeToggle := writeToggle
    io.phy.write.writeData := 0
    io.phy.write.writeMask := 0
    io.phy.write.writeLevelingEnable := False
    io.phy.write.writeLevelingToggle := writeLevelingToggle
    io.phy.readPhaseUpdate.clk := ClockDomain.current.readClockWire
    io.phy.readPhaseUpdate.en := False
    io.phy.readPhaseUpdate.incdec := False
    io.phy.dqsPhaseUpdate.payload.clock := 0
    io.phy.dqsPhaseUpdate.payload.increment := False
    io.phy.dqsPhaseUpdate.valid := False

    io.axiSlave.aw.ready := False
    io.axiSlave.w.ready := False
    io.axiSlave.b.valid := False
    io.axiSlave.b.payload.id := 0
    io.axiSlave.b.payload.resp := 0
    io.axiSlave.ar.ready := False
    io.axiSlave.r.valid := False
    io.axiSlave.r.payload.data := 0
    io.axiSlave.r.payload.id := 0
    io.axiSlave.r.payload.resp := 0
    io.axiSlave.r.payload.last := False

    io.initializationDone := True

    commandToggle := io.phy.write.commandToggle
    readToggle := io.phy.read.readToggle
    writeToggle := io.phy.write.writeToggle
    writeLevelingToggle := io.phy.write.writeLevelingToggle

    readValidToggle := io.phy.read.readValidToggle

    val mainFsm  = new StateMachine {
        val sWaitPhy = makeInstantEntry()
        val sInitialize = new StateFsm(fsm = initializationFsm())
        val sIdle = new State
        val sPrechargeAll = new State
        val sPrechargeAllWait = new State
        val sRefresh = new State
        val sPrecharge = new State
        val sPrechargeWait = new State
        val sActivate = new State
        val sActivateWait = new State
        val sRead = new State
        val sWrite = new State

        sWaitPhy
            .whenIsActive({
                io.phy.deviceReset := True
                io.phy.write.cke := False
                io.initializationDone := False
                when (io.phy.ready) {
                    goto(sInitialize)
                }
            })

        sInitialize
            .onEntry()
            .whenIsActive({
                io.initializationDone := False
            })
            .whenCompleted({
                goto(sIdle)
            })
    }

    def initializationFsm() = new StateMachine {
        val sResetDevice = new State with EntryPoint
        val sCKELow = new State
        val sTXPR = new State
        val sMR2 = new State
        val sMR3 = new State
        val sMR1 = new State
        val sMR0 = new State
        val sZQCL = new State
        val sPrechargeAll = new State
        val sReadCalibration = new StateFsm(fsm = readCalibrationFsm())
        val sWriteLeveling = new StateFsm(fsm = writeLevelingFsm())

        sResetDevice
            .onEntry({
                counter := (if (parameters.synthesis) setCounter(ps = PS(300000000), tCK = TCK(0)) else setCounter(ps = PS(300000), tCK = TCK(0)))
            })
            .whenIsActive({
                io.phy.deviceReset := True
                io.phy.write.cke := False
                counter := counter - 1
                when (counter === 0) {
                    goto(sCKELow)
                }
            })

        sCKELow
            .onEntry({
                counter := (if (parameters.synthesis) setCounter(ps = PS(300000000), tCK = TCK(0)) else setCounter(ps = PS(300000), tCK = TCK(0)))
                io.phy.deviceReset := True
            })
            .whenIsActive({
                io.phy.write.cke := False
                counter := counter - 1
                when (counter === 0) {
                    goto(sTXPR)
                }
            })

        sTXPR
            .onEntry({
                counter := setCounter(ps = parameters.device.TXPR, tCK = parameters.device.TXPR_TCK)
                io.phy.write.cke := True
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sMR2)
                }
            })
        
        sMR2
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TMRD)
                modeRegisterWrite(MR2(
                    casWriteLatency = CASWriteLatency(parameters.writeLatency),
                    autoSelfRefresh = AutoSelfRefresh("Disabled"),
                    selfRefreshTemperature = SelfRefreshTemperature("Normal"),
                    dynamicODT = DynamicODT("Disabled")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sMR3)
                }
            })

        sMR3
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TMRD)
                modeRegisterWrite(MR3())
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sMR1)
                }
            })

        sMR1
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TMRD)
                modeRegisterWrite(MR1(
                    dllEnable = DLLEnable("Enable"),
                    outputDriveStrength = OutputDriveStrength("RZQ/6"),
                    rTT = RTT("RZQ/6"),
                    additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                    writeLeveling = WriteLeveling("Disabled"),
                    tDQS = TDQS("Disabled"),
                    qOff = QOff("Enabled")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sMR0)
                }
            })

        sMR0
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TMRD)
                modeRegisterWrite(MR0(
                    burstLength = BurstLength("Fixed BL8"),
                    casLatency = CASLatency(parameters.readLatency),
                    readBurstType = ReadBurstType("Sequential"),
                    dllReset = DLLReset("Yes"),
                    writeRecovery = WriteRecovery(6),
                    prechargePD = PrechargePD("Off")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sZQCL)
                }
            })

        sZQCL
            .onEntry({
                counter := setCounter(ps = PS(640000), tCK = TCK(512))
                zqCalibration(longCalibration = true)
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sPrechargeAll)
                }
            })

        sPrechargeAll
            .onEntry({
                counter := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                precharge(0, allBanks = true)
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sReadCalibration)
                }
            })

        sReadCalibration
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
            })
            .whenIsActive()
            .whenCompleted({
                goto(sWriteLeveling)
            })

        sWriteLeveling
            .onEntry()
            .whenIsActive()
            .whenCompleted({
                exitFsm()
            })
    }

    def readCalibrationFsm() = new StateMachine {
        val readCalibrationPhase = Reg(UInt(2 bits)) init(0)
        val readCalibrationData = Cat((0 until parameters.burstLength).map(i => {
            io.phy.read.readData(i * parameters.device.DQ_BITS)
        })).asUInt
        val readCalibrationTarget = (0 until parameters.burstLength / 2).foldLeft(U(0))((acc, next) => {
            (acc ## Cat(U"1'b0", U"1'b1")).asUInt
        })

        val phaseCounter = Reg(UInt(8 bits)) init(0)

        val sSetMR3 = new State with EntryPoint
        val sRead = new State
        val sCheckData = new State
        val sUpdatePhase = new State
        val sNext = new State
        val sResetMR3 = new State

        sSetMR3
            .onEntry({
                modeRegisterWrite(MR3(
                    mprReadFunction = MPRReadFunction("Predefined pattern"),
                    mprEnable = MPREnable("Dataflow from MPR")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sRead)
                }
            })
        
        sRead
            .onEntry({
                read(U(0, parameters.device.BA_BITS bit), U(0, parameters.device.COL_BITS bit))
            })
            .whenIsActive({
                when (io.phy.read.readValidToggle ^ readValidToggle) {
                    goto(sCheckData)
                }
            })

        sCheckData
            .onEntry()
            .whenIsActive({
                when (readCalibrationPhase === 0) {
                    when (readCalibrationData =/= readCalibrationTarget) {
                        readCalibrationPhase := 1
                    }
                } .elsewhen (readCalibrationPhase === 1) {
                    when (readCalibrationData === readCalibrationTarget) {
                        readCalibrationPhase := 2
                    }
                } .elsewhen (readCalibrationPhase === 2) {
                    phaseCounter := phaseCounter + 1
                    when (readCalibrationData =/= readCalibrationTarget) {
                        readCalibrationPhase := 3
                        counter := (phaseCounter >> 1).resized
                    }
                }
                goto(sUpdatePhase)
            })

        sUpdatePhase
            .onEntry({
                io.phy.readPhaseUpdate.en := True
                when (readCalibrationPhase === 3) {
                    io.phy.readPhaseUpdate.incdec := True
                }
            })
            .whenIsActive({
                when (io.phy.readPhaseUpdate.done) {
                    goto(sNext)
                }
            })

        sNext
            .onEntry()
            .whenIsActive({
                when (readCalibrationPhase === 3) {
                    when (counter === 0) {
                        goto(sResetMR3)
                    } .otherwise {
                        counter := counter - 1
                        goto(sUpdatePhase)
                    }
                } .otherwise {
                    goto(sRead)
                }
            })

        sResetMR3
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
                modeRegisterWrite(MR3())
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    exitFsm()
                }
            })
    }

    def writeLevelingFsm() = new StateMachine {
        val writeLevelingData = Vec((0 until parameters.device.DQS_BITS).map(i => {
            io.phy.write.writeLevelingData(i * 8).asUInt
        }))
        val writeLevelingByte = Reg(UInt(log2Up(parameters.device.DQS_BITS) bits)) init(0)

        val sWaitStart = new State with EntryPoint
        val sSetMR1 = new State
        val sODTOn = new State
        val sEnableDQS = new State
        val sPulse = new State
        val sCheckData = new State
        val sIncrementDelay = new State
        val sIncrementDelayWait = new State
        val sNextByte = new State
        val sODTOff = new State
        val sResetMR1 = new State

        sWaitStart
            .onEntry()
            .whenIsActive({
                when (io.writeLevelingStart) {
                    goto(sSetMR1)
                }
            })

        sSetMR1
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
                modeRegisterWrite(MR1(
                    dllEnable = DLLEnable("Enable"),
                    outputDriveStrength = OutputDriveStrength("RZQ/6"),
                    rTT = RTT("RZQ/6"),
                    additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                    writeLeveling = WriteLeveling("Enabled"),
                    tDQS = TDQS("Disabled"),
                    qOff= QOff("Enabled")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sODTOn)
                }
            })

        sODTOn
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TWLDQSEN) - setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
            })
            .whenIsActive({
                io.phy.write.odt := True
                counter := counter - 1
                when (counter === 0) {
                    goto(sEnableDQS)
                }
            })

        sEnableDQS
            .onEntry({
                counter := setCounter(ps = PS(0), tCK = parameters.device.TWLMRD) - setCounter(ps = PS(0), tCK = parameters.device.TWLDQSEN)
            })
            .whenIsActive({
                io.phy.write.writeLevelingEnable := True
                io.phy.write.odt := True
                counter := counter - 1
                when (counter === 0) {
                    goto(sPulse)
                }
            })

        sPulse
            .onEntry({
                counter := setCounter(ps = parameters.device.TWLO, tCK = TCK(0)) + 1
                io.phy.write.writeLevelingToggle := ~writeLevelingToggle
            })
            .whenIsActive({
                io.phy.write.writeLevelingEnable := True
                io.phy.write.odt := True
                counter := counter - 1
                when (counter === 0) {
                    goto(sCheckData)
                }
            })

        sCheckData
            .onEntry()
            .whenIsActive({
                io.phy.write.writeLevelingEnable := True
                io.phy.write.odt := True
                when (writeLevelingData(writeLevelingByte) === 0) {
                    goto(sIncrementDelay)
                } .otherwise {
                    when (writeLevelingByte === parameters.device.DQS_BITS - 1) {
                        goto(sODTOff)
                    } .otherwise {
                        writeLevelingByte := writeLevelingByte + 1
                        goto(sPulse)
                    }
                }
            })

        sIncrementDelay
            .onEntry({
                io.phy.dqsPhaseUpdate.payload.clock := writeLevelingByte.resized
                io.phy.dqsPhaseUpdate.payload.increment := True
                io.phy.dqsPhaseUpdate.valid := True
            })
            .whenIsActive({
                io.phy.write.writeLevelingEnable := True
                io.phy.write.odt := True
                when (io.phy.dqsPhaseUpdate.ready) {
                    goto(sPulse)
                }
            })

        sODTOff
            .onEntry({
                counter := parameters.writeLatency + parameters.additiveLatency - 2 + setCounter(ps = PS(0), tCK = parameters.device.TAOF)
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sResetMR1)
                }
            })
        
        sResetMR1
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
                modeRegisterWrite(MR1(
                    dllEnable = DLLEnable("Enable"),
                    outputDriveStrength = OutputDriveStrength("RZQ/6"),
                    rTT = RTT("RZQ/6"),
                    additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                    writeLeveling = WriteLeveling("Disabled"),
                    tDQS = TDQS("Disabled"),
                    qOff = QOff("Enabled")
                ))
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    exitFsm()
                }
            })
    }

    def setCounter(
        ps: PS,
        tCK: TCK
    ) = {
        ps.clockCycles(clockPeriod) max tCK.clockCycles(parameters.controllerClockRatio)
    }

    def modeRegisterWrite(
        modeRegister: ModeRegister
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := modeRegister.index
        io.phy.write.command.addr := modeRegister.asUInt(parameters.device.ADDR_BITS - 1 downto 0)
    }

    def zqCalibration(
        longCalibration: Boolean = true
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.ras_n := True
        io.phy.write.command.cas_n := True
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := 0
        io.phy.write.command.addr := (if (longCalibration) 1 << 10 else 0)
    }

    def precharge(
        bank: UInt,
        allBanks: Boolean = false
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := True
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := (if (allBanks) 1 << 10 else 0)
    }

    def read(
        bank: UInt,
        address: UInt
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.ras_n := True
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := True
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := Cat(
            if (parameters.device.COL_BITS > 10) address(parameters.device.COL_BITS downto 10) else U"b0",
            U"1'b0",
            address(9 downto 0)
        ).asUInt.resized
        io.phy.read.readToggle := ~readToggle
    }
}

case class RankControllerDDR3 (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS,
                dataWidth = 128,
                idWidth = 8
            )
        ))
    }

    val model = ddr3(parameters)
    val rankController = RankController(parameters)
    val phy = PHY(parameters)

    rankController.io.axiSlave <> io.axiSlave
    rankController.io.writeLevelingStart := True
    phy.io.internal <> rankController.io.phy

    model.io.rst_n := phy.io.device.rst_n
    model.io.ck := phy.io.device.ck.p
    model.io.ck_n := phy.io.device.ck.n
    model.io.cke := phy.io.device.cke
    model.io.cs_n := phy.io.device.cs_n
    model.io.ras_n := phy.io.device.command.ras_n
    model.io.cas_n := phy.io.device.command.cas_n
    model.io.we_n := phy.io.device.command.we_n
    model.io.dm_tdqs := phy.io.device.dm
    model.io.ba := phy.io.device.command.ba
    model.io.addr := phy.io.device.command.addr
    model.io.dq := phy.io.device.dq
    model.io.dqs := phy.io.device.dqs.p
    model.io.dqs_n := phy.io.device.dqs.n
    model.io.odt := phy.io.device.odt
}

object RankControllerVerilog {
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
            RankController(DDR3Parameters())
        )
    }
}

object RankControllerSimulation {
    def test(
        dut: RankControllerDDR3
    ) = {
        dut.clockDomain.forkStimulus(frequency = HertzNumber(200000000))

        dut.clockDomain.waitSampling(8000)
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
                                    RankControllerDDR3(DDR3Parameters(
                                        synthesis = false
                                    ))
                                )

        compiled.doSim(dut => test(dut))
    }
}