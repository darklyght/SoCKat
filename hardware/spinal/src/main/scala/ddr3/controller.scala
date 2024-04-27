package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._

import sockat.models
import sockat.primitives._
import sockat.utilities._

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.util.Random

case class Controller (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val phy = master(PHYInterface(parameters))
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength),
                dataWidth = parameters.dqParallel * parameters.burstLength * parameters.device.DQ_BITS,
                idWidth = 8
            )
        ))
        val readPhaseUpdate = master(PhaseShiftInterface())
        val dqsPhaseUpdate = master(PhaseShiftInterface())
    }

    val clockPeriod = parameters.ckClockRatio * parameters.tCKPeriod

    val commandToggle = Reg(Bool()) init(False)
    val readToggle = Reg(Bool()) init(False)
    val writeToggle = Reg(Bool()) init(False)
    val writeLevelingToggle = Reg(Bool()) init(False)

    val readValidToggle = Reg(Bool()) init(False)

    val counter = Reg(UInt(17 bits)) init(0)

    io.phy.deviceReset := False
    io.phy.read.readToggle := readToggle
    io.phy.write.cke := True
    io.phy.write.command.cs_n.foreach(cs_n =>{
        cs_n := True
    })
    io.phy.write.odt := False
    io.phy.write.commandToggle := commandToggle
    io.phy.write.command.ras_n := True
    io.phy.write.command.cas_n := True
    io.phy.write.command.we_n := True
    io.phy.write.command.ba := 0
    io.phy.write.command.addr := 0
    io.phy.write.writeToggle := writeToggle
    io.phy.write.writeData.foreach(_ := 0)
    io.phy.write.writeMask.foreach(_ := 0)
    io.phy.write.writeLevelingEnable := False
    io.phy.write.writeLevelingToggle := writeLevelingToggle

    io.readPhaseUpdate.clk := ClockDomain.current.readClockWire
    io.readPhaseUpdate.en := False
    io.readPhaseUpdate.incdec := False
    io.dqsPhaseUpdate.clk := ClockDomain.current.readClockWire
    io.dqsPhaseUpdate.en := False
    io.dqsPhaseUpdate.incdec := True

    commandToggle := io.phy.write.commandToggle
    readToggle := io.phy.read.readToggle
    writeToggle := io.phy.write.writeToggle
    writeLevelingToggle := io.phy.write.writeLevelingToggle

    readValidToggle := io.phy.read.readValidToggle

    val device = Reg(UInt(parameters.device.RANKS bits)) init(1)

    val rank = (log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength) - 1 downto parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength))
    val bank = (parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength) - 1 downto parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength))
    val row = (parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength) - 1 downto parameters.device.COL_BITS - log2Up(parameters.burstLength))
    val column = (parameters.device.COL_BITS - log2Up(parameters.burstLength) - 1 downto 0)

    val bankPrecharged = Vec.fill(parameters.device.RANKS)(
        Vec.fill(Math.pow(2, parameters.device.BA_BITS).toInt)(
            Reg(Bool()) init(False)
        )
    )
    val allBanksPrecharged = bankPrecharged.map(device => {
        device.reduce(_ && _)
    }).reduce(_ && _)

    val rowActivated = Vec.fill(parameters.device.RANKS)(
        Vec.fill(Math.pow(2, parameters.device.BA_BITS).toInt)(
            Reg(UInt(parameters.device.ROW_BITS bits)) init(0)
        )
    )
    val rowActivatedValid = Vec.fill(parameters.device.RANKS)(
        Vec.fill(Math.pow(2, parameters.device.BA_BITS).toInt)(
            Reg(Bool()) init(False)
        )
    )

    val refiTimer = Reg(UInt(32 bits)) init(setCounter(ps = PS(7800000), tCK = TCK(0)))
    val refreshTimer = Reg(UInt(32 bits)) init(0)
    val prechargeTimer = Reg(UInt(32 bits)) init(0)
    val activateTimer = Reg(UInt(32 bits)) init(0)
    val readTimer = Reg(UInt(32 bits)) init(0)
    val writeTimer = Reg(UInt(32 bits)) init(0)

    val readRequest = Reg(Axi4Ar(io.axiSlave.config))
    readRequest.addr init(0)
    readRequest.id init(0)
    readRequest.region init(0)
    readRequest.len init(0)
    readRequest.size init(0)
    readRequest.burst init(0)
    readRequest.lock init(0)
    readRequest.cache init(0)
    readRequest.qos init(0)
    readRequest.prot init(0)
    val writeRequest = Reg(Axi4Aw(io.axiSlave.config))
    writeRequest.addr init(0)
    writeRequest.id init(0)
    writeRequest.region init(0)
    writeRequest.len init(0)
    writeRequest.size init(0)
    writeRequest.burst init(0)
    writeRequest.lock init(0)
    writeRequest.cache init(0)
    writeRequest.qos init(0)
    writeRequest.prot init(0)

    val refreshFIFO = FIFO(
        FIFOParameters(
            dataType = Bool(),
            addressWidth = 4
        )
    )
    refreshFIFO.io.enqueue.payload := True
    refreshFIFO.io.enqueue.valid := refiTimer === 0

    val arFIFO = FIFO(
        FIFOParameters(
            dataType = Axi4Ar(io.axiSlave.config),
            addressWidth = 2
        )
    )

    val rFIFO = FIFO(
        FIFOParameters(
            dataType = Axi4R(io.axiSlave.config),
            addressWidth = 10
        )
    )

    val awFIFO = FIFO(
        FIFOParameters(
            dataType = Axi4Aw(io.axiSlave.config),
            addressWidth = 2
        )
    )

    val wFIFO = FIFO(
        FIFOParameters(
            dataType = Axi4W(io.axiSlave.config),
            addressWidth = 10
        )
    )

    val bFIFO = FIFO(
        FIFOParameters(
            dataType = Axi4B(io.axiSlave.config),
            addressWidth = 2
        )
    )

    val readPriority = Reg(Bool()) init(True)
    val writePriority = Reg(Bool()) init(False)
    val addressRequests = OHMasking.roundRobin(Cat(arFIFO.io.dequeue.valid, awFIFO.io.dequeue.valid), Cat(readPriority, writePriority))
    val readGrant = addressRequests(1)
    val writeGrant = addressRequests(0)

    val readOutstanding = FIFO(
        FIFOParameters(
            dataType = Bool,
            addressWidth = 5
        )
    )

    readOutstanding.io.enqueue.valid := False
    readOutstanding.io.enqueue.payload := True
    readOutstanding.io.dequeue.ready := rFIFO.io.enqueue.valid

    val writeResponse = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = (parameters.writeLatency + parameters.additiveLatency) / parameters.ckClockRatio + 1,
            inputWidth = 1,
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    writeResponse.io.load := False
    writeResponse.io.shift := True
    writeResponse.io.input := Vec(True)

    val mainFsm = new StateMachine {
        val sWaitMMCM = makeInstantEntry()
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
        val sReadWait = new State
        val sWrite = new State
        val sWriteWait = new State

        when (refiTimer === 0) {
            refiTimer := setCounter(ps = PS(7800000), tCK = TCK(0))
        } .otherwise {
            refiTimer := refiTimer - 1;
        }

        when (refreshTimer > 0) {
            refreshTimer := refreshTimer - 1
        }

        when (prechargeTimer > 0) {
            prechargeTimer := prechargeTimer - 1
        }

        when (activateTimer > 0) {
            activateTimer := activateTimer - 1
        }

        when (readTimer > 0) {
            readTimer := readTimer - 1
        }

        when (writeTimer > 0) {
            writeTimer := writeTimer - 1
        }

        refreshFIFO.io.dequeue.ready := False

        sWaitMMCM
            .whenIsActive({
                io.phy.deviceReset := True
                io.phy.write.cke := False
                refiTimer := setCounter(ps = PS(7800000), tCK = TCK(0))
                goto(sInitialize)
            })

        sInitialize
            .onEntry()
            .whenIsActive({
                refiTimer := setCounter(ps = PS(7800000), tCK = TCK(0))
            })
            .whenCompleted({
                goto(sIdle)
            })

        sIdle
            .onEntry()
            .whenIsActive({
                when (refreshFIFO.io.dequeue.valid) {
                    when (allBanksPrecharged) {
                        goto(sRefresh)
                    } .otherwise {
                        when (prechargeTimer === 0) {
                            goto(sPrechargeAll)
                        }
                    }
                } .elsewhen (readGrant) {
                    readRequest := arFIFO.io.dequeue.payload
                    when (rowActivatedValid(arFIFO.io.dequeue.payload.addr(rank))(arFIFO.io.dequeue.payload.addr(bank)) && (rowActivated(arFIFO.io.dequeue.payload.addr(rank))(arFIFO.io.dequeue.payload.addr(bank)) === arFIFO.io.dequeue.payload.addr(row))) {
                        when (readTimer === 0 && rFIFO.io.enqueue.ready) {
                            goto(sRead)
                        }
                    } .elsewhen (bankPrecharged(arFIFO.io.dequeue.payload.addr(rank))(arFIFO.io.dequeue.payload.addr(bank))) {
                        when (activateTimer === 0) {
                            goto(sActivate)
                        }
                    } .otherwise {
                        when (prechargeTimer === 0) {
                            goto(sPrecharge)
                        }
                    }
                } .elsewhen (writeGrant) {
                    writeRequest := awFIFO.io.dequeue.payload
                    when (rowActivatedValid(awFIFO.io.dequeue.payload.addr(rank))(awFIFO.io.dequeue.payload.addr(bank)) && (rowActivated(awFIFO.io.dequeue.payload.addr(rank))(awFIFO.io.dequeue.payload.addr(bank)) === awFIFO.io.dequeue.payload.addr(row))) {
                        when (writeTimer === 0 && bFIFO.io.enqueue.ready) {
                            goto(sWrite)
                        }
                    } .elsewhen (bankPrecharged(awFIFO.io.dequeue.payload.addr(rank))(awFIFO.io.dequeue.payload.addr(bank))) {
                        when (activateTimer === 0) {
                            goto(sActivate)
                        }
                    } .otherwise {
                        when (prechargeTimer === 0) {
                            goto(sPrecharge)
                        }
                    }
                }
            })

        sPrechargeAll
            .onEntry({
                refreshTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                prechargeTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                activateTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                readTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                writeTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
            })
            .whenIsActive({
                precharge(
                    device = U(device.range -> true),
                    bank = 0,
                    allBanks = true
                )
                
                bankPrecharged.foreach(device => {
                    device.foreach(bank => {
                        bank := True
                    })
                })
                rowActivatedValid.foreach(device => {
                    device.foreach(bank => {
                        bank := False
                    })
                })

                goto(sPrechargeAllWait)
            })

        sPrechargeAllWait
            .onEntry()
            .whenIsActive({
                when (refreshTimer === 0) {
                    goto(sRefresh)
                }
            })

        sRefresh
            .onEntry({
                refreshTimer := setCounter(ps = parameters.device.TRFC_MIN, tCK = TCK(0))
                prechargeTimer := setCounter(ps = parameters.device.TRFC_MIN, tCK = TCK(0))
                activateTimer := setCounter(ps = parameters.device.TRFC_MIN, tCK = TCK(0))
                readTimer := setCounter(ps = parameters.device.TRFC_MIN, tCK = TCK(0))
                writeTimer := setCounter(ps = parameters.device.TRFC_MIN, tCK = TCK(0))
            })
            .whenIsActive({
                refreshFIFO.io.dequeue.ready := True
                refresh(device = U(device.range -> true))

                bankPrecharged.foreach(device => {
                    device.foreach(bank => {
                        bank := True
                    })
                })

                goto(sIdle)
            })

        sPrecharge
            .onEntry({
                refreshTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                prechargeTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                activateTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                readTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
                writeTimer := setCounter(ps = parameters.device.TRP, tCK = TCK(0))
            })
            .whenIsActive({
                val address = UInt(log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength) bits)
                when (readGrant) {
                    address := readRequest.addr
                } .otherwise {
                    address := writeRequest.addr
                }
                precharge(
                    device = 1 << address(rank),
                    bank = address(bank),
                    allBanks = false
                )

                bankPrecharged(address(rank))(address(bank)) := True
                rowActivatedValid(address(rank))(address(bank)) := False

                goto(sPrechargeWait)
            })

        sPrechargeWait
            .onEntry()
            .whenIsActive({
                when (activateTimer === 0) {
                    goto(sActivate)
                }
            })
        
        sActivate
            .onEntry({
                prechargeTimer := setCounter(ps = parameters.device.TRAS_MIN, tCK = TCK(0))
                activateTimer := setCounter(ps = parameters.device.TRRD, tCK = TCK(0))
                readTimer := (setCounter(ps = parameters.device.TRCD, tCK = TCK(0)) - parameters.additiveLatency) max 0
                writeTimer := (setCounter(ps = parameters.device.TRCD, tCK = TCK(0)) - parameters.additiveLatency) max 0
            })
            .whenIsActive({
                val address = UInt(log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength) bits)
                when (readGrant) {
                    address := readRequest.addr
                } .otherwise {
                    address := writeRequest.addr
                }
                activate(
                    device = 1 << address(rank),
                    bank = address(bank),
                    row = address(row)
                )

                bankPrecharged(address(rank))(address(bank)) := False
                rowActivatedValid(address(rank))(address(bank)) := True
                rowActivated(address(rank))(address(bank)) := address(row)

                goto(sActivateWait)
            })

        sActivateWait
            .onEntry()
            .whenIsActive({
                when (readGrant && rFIFO.io.enqueue.ready) {
                    goto(sRead)
                } .otherwise {
                    when (wFIFO.io.dequeue.valid && bFIFO.io.enqueue.ready) {
                        goto(sWrite)
                    }
                }
            })

        sRead
            .onEntry({
                prechargeTimer := setCounter(ps = parameters.device.TRTP, tCK = parameters.device.TRTP_TCK) + parameters.additiveLatency
                readTimer := setCounter(ps = PS(0), tCK = parameters.device.TCCD)
                writeTimer := parameters.readLatency + parameters.additiveLatency + setCounter(ps = PS(0), tCK = parameters.device.TCCD) + 2 - parameters.writeLatency
            })
            .whenIsActive({
                read(
                    device = 1 << readRequest.addr(rank),
                    bank = readRequest.addr(bank),
                    column = Cat(readRequest.addr(column), U(0, log2Up(parameters.burstLength) bits)).asUInt
                )

                readRequest.addr := readRequest.addr + 1

                goto(sReadWait)
            })

        sReadWait
            .onEntry()
            .whenIsActive({
                when (readRequest.len === 0) {
                    arFIFO.io.dequeue.ready := True

                    readPriority := False
                    writePriority := True

                    goto(sIdle)
                } .elsewhen (rowActivatedValid(readRequest.addr(rank))(readRequest.addr(bank)) && rowActivated(readRequest.addr(rank))(readRequest.addr(bank)) === readRequest.addr(row) && readTimer === 0 && rFIFO.io.enqueue.ready) {
                    readRequest.len := readRequest.len - 1
                    readOutstanding.io.enqueue.valid := True

                    goto(sRead)
                } .elsewhen (bankPrecharged(readRequest.addr(rank))(readRequest.addr(bank)) && activateTimer === 0) {
                    readRequest.len := readRequest.len - 1
                    readOutstanding.io.enqueue.valid := True
                    
                    goto(sActivate)
                } .elsewhen (prechargeTimer === 0) {
                    readRequest.len := readRequest.len - 1
                    readOutstanding.io.enqueue.valid := True

                    goto(sPrecharge)
                }
            })

        sWrite
            .onEntry({
                prechargeTimer := parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 + setCounter(ps = parameters.device.TWR, tCK = TCK(0))
                readTimer := parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 + setCounter(ps = parameters.device.TWTR, tCK = parameters.device.TWTR_TCK)
                writeTimer := setCounter(ps = PS(0), tCK = parameters.device.TCCD)
            })
            .whenIsActive({
                write(
                    device = 1 << writeRequest.addr(rank),
                    bank = writeRequest.addr(bank),
                    column = Cat(writeRequest.addr(column), U(0, log2Up(parameters.burstLength) bits)).asUInt,
                    data = wFIFO.io.dequeue.payload.data.asUInt,
                    mask = wFIFO.io.dequeue.payload.strb.asUInt
                )

                writeRequest.addr := writeRequest.addr + 1

                wFIFO.io.dequeue.ready := True

                goto(sWriteWait)
            })

        sWriteWait
            .onEntry()
            .whenIsActive({
                when (writeRequest.len === 0) {
                    awFIFO.io.dequeue.ready := True
                    writeResponse.io.load := True

                    readPriority := True
                    writePriority := False

                    goto(sIdle)
                } .elsewhen (rowActivatedValid(writeRequest.addr(rank))(writeRequest.addr(bank)) && rowActivated(writeRequest.addr(rank))(writeRequest.addr(bank)) === writeRequest.addr(row) && writeTimer === 0 && wFIFO.io.dequeue.valid && bFIFO.io.enqueue.ready) {
                    writeRequest.len := writeRequest.len - 1

                    goto(sWrite)
                } .elsewhen (bankPrecharged(writeRequest.addr(rank))(writeRequest.addr(bank)) && activateTimer === 0) {
                    writeRequest.len := writeRequest.len - 1

                    goto(sActivate)
                } .elsewhen (prechargeTimer === 0) {
                    writeRequest.len := writeRequest.len - 1

                    goto(sPrecharge)
                }
            })
    }

    arFIFO.io.enqueue <> io.axiSlave.ar
    arFIFO.io.dequeue.ready := False

    rFIFO.io.dequeue <> io.axiSlave.r
    rFIFO.io.enqueue.valid := !(mainFsm.isActive(mainFsm.sInitialize)) && (readValidToggle ^ io.phy.read.readValidToggle)
    rFIFO.io.enqueue.payload.data := io.phy.read.readData.asBits
    rFIFO.io.enqueue.payload.id := readRequest.id
    rFIFO.io.enqueue.payload.resp := 0
    rFIFO.io.enqueue.payload.last := ~readOutstanding.io.dequeue.valid

    awFIFO.io.enqueue <> io.axiSlave.aw
    awFIFO.io.dequeue.ready := False

    wFIFO.io.enqueue <> io.axiSlave.w
    wFIFO.io.dequeue.ready := False

    bFIFO.io.dequeue <> io.axiSlave.b
    bFIFO.io.enqueue.valid := writeResponse.io.output(0)
    bFIFO.io.enqueue.payload.id := writeRequest.id
    bFIFO.io.enqueue.payload.resp := 0

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
        val sNextRank = new State
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
                modeRegisterWrite(
                    device = U(device.range -> true),
                    modeRegister = MR2(
                        casWriteLatency = CASWriteLatency(parameters.writeLatency),
                        autoSelfRefresh = AutoSelfRefresh("Disabled"),
                        selfRefreshTemperature = SelfRefreshTemperature("Normal"),
                        dynamicODT = DynamicODT("Disabled")
                    )
                )
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
                modeRegisterWrite(
                    device = U(device.range -> true),
                    modeRegister = MR3()
                )
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
                modeRegisterWrite(
                    device = U(device.range -> true),
                    modeRegister = MR1(
                        dllEnable = DLLEnable("Enable"),
                        outputDriveStrength = OutputDriveStrength("RZQ/6"),
                        rTT = RTT("RZQ/6"),
                        additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                        writeLeveling = WriteLeveling("Disabled"),
                        tDQS = TDQS("Disabled"),
                        qOff = QOff("Enabled")
                    )
                )
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sMR0)
                }
            })

        sMR0
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
                modeRegisterWrite(
                    device = U(device.range -> true),
                    modeRegister = MR0(
                        burstLength = BurstLength("Fixed BL8"),
                        casLatency = CASLatency(parameters.readLatency),
                        readBurstType = ReadBurstType("Sequential"),
                        dllReset = DLLReset("Yes"),
                        writeRecovery = WriteRecovery(6),
                        prechargePD = PrechargePD("Off")
                    )
                )
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
                zqCalibration(
                    device = U(device.range -> true),
                    longCalibration = true
                )
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
                precharge(
                    device = U(device.range -> true),
                    bank = 0,
                    allBanks = true
                )
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

        sNextRank
            .onEntry({
                device := device |<< 1
            })
            .whenIsActive({
                goto(sWriteLeveling)
            })

        sWriteLeveling
            .onEntry()
            .whenIsActive()
            .whenCompleted({
                when (device(parameters.device.RANKS - 1) === True) {
                    exitFsm()
                } .otherwise {
                    goto(sNextRank)
                }
            })
    }

    def readCalibrationFsm() = new StateMachine {
        val readCalibrationPhase = Reg(UInt(2 bits)) init(0)
        val readCalibrationData = Cat((0 until parameters.dqParallel).flatMap(i => {
            (0 until parameters.burstLength).map(j => {
                io.phy.read.readData(i)(j * parameters.device.DQ_BITS)
            })
        })).asUInt
        val readCalibrationTarget = (0 until parameters.dqParallel * parameters.burstLength / 2).foldLeft(U(0))((acc, next) => {
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
                modeRegisterWrite(
                    device = U(1, parameters.device.RANKS bit),
                    MR3(
                        mprReadFunction = MPRReadFunction("Predefined pattern"),
                        mprEnable = MPREnable("Dataflow from MPR")
                    )
                )
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    goto(sRead)
                }
            })
        
        sRead
            .onEntry({
                read(
                    device = U(1, parameters.device.RANKS bit),
                    bank = U(0, parameters.device.BA_BITS bit),
                    column = U(0, parameters.device.COL_BITS bit)
                )
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
                io.readPhaseUpdate.en := True
                when (readCalibrationPhase === 3) {
                    io.readPhaseUpdate.incdec := True
                }
            })
            .whenIsActive({
                when (io.readPhaseUpdate.done) {
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
                modeRegisterWrite(
                    device = U(1, parameters.device.RANKS bit),
                    modeRegister = MR3()
                )
            })
            .whenIsActive({
                counter := counter - 1
                when (counter === 0) {
                    exitFsm()
                }
            })
    }

    def writeLevelingFsm() = new StateMachine {
        val writeLevelingData = (0 until parameters.device.DQS_BITS).map(i => {
            io.phy.write.writeLevelingData.reduce(_ & _)(i * 8).asUInt
        }).reduce(_ & _)

        val sSetMR1 = new State with EntryPoint
        val sODTOn = new State
        val sEnableDQS = new State
        val sPulse = new State
        val sCheckData = new State
        val sIncrementDelay = new State
        val sIncrementDelayWait = new State
        val sODTOff = new State
        val sResetMR1 = new State

        sSetMR1
            .onEntry({
                counter := setCounter(ps = parameters.device.TMOD, tCK = parameters.device.TMOD_TCK)
                modeRegisterWrite(
                    device = device,
                    MR1(
                        dllEnable = DLLEnable("Enable"),
                        outputDriveStrength = OutputDriveStrength("RZQ/6"),
                        rTT = RTT("RZQ/6"),
                        additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                        writeLeveling = WriteLeveling("Enabled"),
                        tDQS = TDQS("Disabled"),
                        qOff= QOff("Enabled")
                    )
                )
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
                when (writeLevelingData === 0) {
                    goto(sIncrementDelay)
                } .otherwise {
                    goto(sODTOff)
                }
            })

        sIncrementDelay
            .onEntry({
                io.dqsPhaseUpdate.en := True
            })
            .whenIsActive({
                io.phy.write.writeLevelingEnable := True
                io.phy.write.odt := True
                when (io.dqsPhaseUpdate.done) {
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
                modeRegisterWrite(
                    device = device,
                    modeRegister = MR1(
                        dllEnable = DLLEnable("Enable"),
                        outputDriveStrength = OutputDriveStrength("RZQ/6"),
                        rTT = RTT("RZQ/6"),
                        additiveLatency = AdditiveLatency(parameters.additiveLatencyString),
                        writeLeveling = WriteLeveling("Disabled"),
                        tDQS = TDQS("Disabled"),
                        qOff = QOff("Enabled")
                    )
                )
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
        ps.clockCycles(clockPeriod) max tCK.clockCycles(parameters.ckClockRatio)
    }

    def modeRegisterWrite(
        device: UInt,
        modeRegister: ModeRegister
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := modeRegister.index
        io.phy.write.command.addr := modeRegister.asUInt(parameters.device.ADDR_BITS - 1 downto 0)
    }

    def zqCalibration(
        device: UInt,
        longCalibration: Boolean = true
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := True
        io.phy.write.command.cas_n := True
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := 0
        io.phy.write.command.addr := (if (longCalibration) 1 << 10 else 0)
    }

    def refresh(
        device: UInt
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := True
        io.phy.write.command.ba := 0
        io.phy.write.command.addr := 0
    }

    def precharge(
        device: UInt,
        bank: UInt,
        allBanks: Boolean = false
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := True
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := (if (allBanks) 1 << 10 else 0)
    }

    def activate(
        device: UInt,
        bank: UInt,
        row: UInt
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := False
        io.phy.write.command.cas_n := True
        io.phy.write.command.we_n := True
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := row.resized
    }

    def read(
        device: UInt,
        bank: UInt,
        column: UInt
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := True
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := True
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := Cat(
            if (parameters.device.COL_BITS > 10) column(parameters.device.COL_BITS downto 10) else U"b0",
            U"1'b0",
            column(9 downto 0)
        ).asUInt.resized
        io.phy.read.readToggle := ~readToggle
    }

    def write(
        device: UInt,
        bank: UInt,
        column: UInt,
        data: UInt,
        mask: UInt
    ) = {
        io.phy.write.commandToggle := ~commandToggle
        io.phy.write.command.cs_n := ~device.asBools
        io.phy.write.command.ras_n := True
        io.phy.write.command.cas_n := False
        io.phy.write.command.we_n := False
        io.phy.write.command.ba := bank
        io.phy.write.command.addr := Cat(
            if (parameters.device.COL_BITS > 10) column(parameters.device.COL_BITS downto 10) else U"b0",
            U"1'b0",
            column(9 downto 0)
        ).asUInt.resized
        io.phy.write.writeToggle := ~writeToggle
        io.phy.write.writeData := data.resize(parameters.dqParallel * parameters.burstLength * parameters.device.DQ_BITS bits).subdivideIn(parameters.burstLength * parameters.device.DQ_BITS bits)
        io.phy.write.writeMask := mask.resize(parameters.dqParallel * parameters.burstLength * parameters.device.DM_BITS bits).subdivideIn(parameters.burstLength * parameters.device.DM_BITS bits)
    }
}

case class ControllerSimulationModel (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength),
                dataWidth = parameters.dqParallel * parameters.burstLength * parameters.device.DQ_BITS,
                idWidth = 8
            )
        ))
    }

    val model = Seq.fill(parameters.device.RANKS) {
        Seq.fill(parameters.dqParallel) {
            models.ddr3(parameters)
        }
    }

    val clockGenerator = MMCME2_ADV(MMCME2_ADVParameters(
        clkIn1Period = (parameters.ckClockRatio * parameters.tCKPeriod).toInt,
        clkFbOutMultF = 8,
        clkOut0DivideF = 2,
        clkOut1Divide = 2,
        clkOut1UseFinePs = "TRUE",
        clkOut2Divide = 2,
        clkOut2Phase = 270,
        divClkDivide = 2
    ))

    clockGenerator.noDynamicReconfiguration()

    clockGenerator.io.clkIn1 := ClockDomain.current.readClockWire
    clockGenerator.io.clkIn2 := False
    clockGenerator.io.clkInSel := 1
    clockGenerator.io.clkFbIn := clockGenerator.io.clkFbOut
    clockGenerator.io.rst := ClockDomain.current.readResetWire
    clockGenerator.io.pwrDwn := False

    val dqsClockGenerator = MMCME2_ADV(MMCME2_ADVParameters(
        clkIn1Period = (parameters.ckClockRatio * parameters.tCKPeriod).toInt,
        clkFbOutMultF = 8,
        clkOut0DivideF = 2,
        clkOut0UseFinePs = "TRUE",
        divClkDivide = 2
    ))

    dqsClockGenerator.noDynamicReconfiguration()

    dqsClockGenerator.io.clkIn1 := ClockDomain.current.readClockWire
    dqsClockGenerator.io.clkIn2 := False
    dqsClockGenerator.io.clkInSel := 1
    dqsClockGenerator.io.clkFbIn := dqsClockGenerator.io.clkFbOut
    dqsClockGenerator.io.rst := ClockDomain.current.readResetWire
    dqsClockGenerator.io.pwrDwn := False

    val ckResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    ckResetSynchronizer.io.clock := clockGenerator.io.clkOut0
    ckResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~clockGenerator.io.locked

    val readResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    readResetSynchronizer.io.clock := clockGenerator.io.clkOut1
    readResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~clockGenerator.io.locked

    val writeResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    writeResetSynchronizer.io.clock := clockGenerator.io.clkOut2
    writeResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~clockGenerator.io.locked

    val dqsResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    dqsResetSynchronizer.io.clock := dqsClockGenerator.io.clkOut0
    dqsResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~dqsClockGenerator.io.locked

    val ckClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut0,
        reset = ckResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )
    
    val readClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut1,
        reset = readResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val writeClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut2,
        reset = writeResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val dqsClockDomain = ClockDomain(
        clock = dqsClockGenerator.io.clkOut0,
        reset = dqsResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val controller = Controller(parameters)
    val phy = PHY(
        parameters = parameters,
        ckClockDomain = ckClockDomain,
        readClockDomain = readClockDomain,
        writeClockDomain = writeClockDomain,
        dqsClockDomain = dqsClockDomain
    )

    clockGenerator.io.phaseShift <> controller.io.readPhaseUpdate
    dqsClockGenerator.io.phaseShift <> controller.io.dqsPhaseUpdate

    controller.io.axiSlave <> io.axiSlave
    phy.io.internal <> controller.io.phy

    val ranks = 0 until parameters.device.RANKS
    val chips = 0 until parameters.dqParallel

    (0 until parameters.device.RANKS).foreach(i => {
        (0 until parameters.dqParallel).foreach(j => {
            model(i)(j).io.rst_n := phy.io.device.rst_n
            model(i)(j).io.ck := phy.io.device.ck.p
            model(i)(j).io.ck_n := phy.io.device.ck.n
            model(i)(j).io.cke := phy.io.device.cke
            model(i)(j).io.cs_n := phy.io.device.command.cs_n(i)
            model(i)(j).io.ras_n := phy.io.device.command.ras_n
            model(i)(j).io.cas_n := phy.io.device.command.cas_n
            model(i)(j).io.we_n := phy.io.device.command.we_n
            model(i)(j).io.dm_tdqs := phy.io.device.dm(j)
            model(i)(j).io.ba := phy.io.device.command.ba
            model(i)(j).io.addr := phy.io.device.command.addr
            model(i)(j).io.dq := phy.io.device.dq(j)
            model(i)(j).io.dqs := phy.io.device.dqs.p
            model(i)(j).io.dqs_n := phy.io.device.dqs.n
            model(i)(j).io.odt := phy.io.device.odt
        })
    })
}

object ControllerVerilog {
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
            Controller(DDR3Parameters())
        )
    }
}

object ControllerSimulation {
    def test(
        dut: ControllerSimulationModel
    ) = {
        def read(
            dut: ControllerSimulationModel,
            address: Queue[BigInt],
            length: Queue[BigInt],
            readDataAddress: Queue[BigInt]
        ) = {
            while (true) {
                if (!address.isEmpty && !length.isEmpty && dut.io.axiSlave.ar.ready.toBoolean) {
                    (0 until length(0).toInt).foreach(i => {
                        readDataAddress.enqueue(address(0) + i)
                    })

                    dut.io.axiSlave.ar.valid #= true
                    dut.io.axiSlave.ar.payload.addr #= address.dequeue()
                    dut.io.axiSlave.ar.payload.len #= length.dequeue() - 1
                }

                dut.clockDomain.waitFallingEdge()

                dut.io.axiSlave.ar.valid #= false
                dut.io.axiSlave.ar.payload.addr #= 0
                dut.io.axiSlave.ar.payload.len #= 0
            }
        }

        def readData(
            dut: ControllerSimulationModel,
            readDataAddress: Queue[BigInt],
            model: Map[BigInt, BigInt]
        ) = {
            while (true) {
                if (!readDataAddress.isEmpty && dut.io.axiSlave.r.valid.toBoolean) {
                    // println(readDataAddress)
                    if (model.contains(readDataAddress(0))) {
                        assert(
                            model.getOrElse(readDataAddress(0), BigInt(0)) == dut.io.axiSlave.r.payload.data.toBigInt,
                            f"${readDataAddress(0) * dut.parameters.burstLength}%x: ${model.getOrElse(readDataAddress(0), BigInt(0))}%X, ${dut.io.axiSlave.r.payload.data.toBigInt}%X"
                        )
                    }
                    readDataAddress.dequeue()
                }

                dut.clockDomain.waitFallingEdge()
            }
        }

        def write(
            dut: ControllerSimulationModel,
            address: Queue[BigInt],
            data: Queue[Seq[BigInt]],
            writeAddressData: Queue[Queue[(BigInt, BigInt)]]
        ) = {
            while(true) {
                if (!address.isEmpty && !data.isEmpty && dut.io.axiSlave.aw.ready.toBoolean) {
                    var current = address.dequeue()
                    var modelQueue = Queue[(BigInt, BigInt)]()

                    dut.io.axiSlave.aw.valid #= true
                    dut.io.axiSlave.aw.payload.addr #= current
                    dut.io.axiSlave.aw.payload.len #= data(0).length - 1

                    val items = data.dequeue()

                    items.zipWithIndex.foreach({case (item, index) => {
                        var count = 0
                        while (!dut.io.axiSlave.w.ready.toBoolean) {
                            dut.clockDomain.waitFallingEdge()

                            if (index == 0 && count == 0) {
                                dut.io.axiSlave.aw.valid #= false
                                dut.io.axiSlave.aw.payload.addr #= 0
                                dut.io.axiSlave.aw.payload.len #= 0
                            }

                            count = count + 1
                        }

                        dut.io.axiSlave.w.valid #= true
                        dut.io.axiSlave.w.payload.data #= item
                        dut.io.axiSlave.w.payload.strb #= 0
                        dut.io.axiSlave.w.payload.last #= index == (items.length - 1)

                        modelQueue.enqueue((current, item))
                        current = current + 1

                        dut.clockDomain.waitFallingEdge()

                        if (index == 0 && count == 0) {
                            dut.io.axiSlave.aw.valid #= false
                            dut.io.axiSlave.aw.payload.addr #= 0
                            dut.io.axiSlave.aw.payload.len #= 0
                        }

                        if (index == items.length - 1) {
                            dut.io.axiSlave.w.valid #= false
                            dut.io.axiSlave.w.payload.data #= 0
                            dut.io.axiSlave.w.payload.strb #= 0
                            dut.io.axiSlave.w.payload.last #= false

                            writeAddressData.enqueue(modelQueue)
                        }
                    }})
                } else {
                    dut.clockDomain.waitFallingEdge()
                }
            }
        }

        def writeResponse(
            dut: ControllerSimulationModel,
            writeAddressData: Queue[Queue[(BigInt, BigInt)]],
            model: Map[BigInt, BigInt]
        ) = {
            while (true) {
                if (!writeAddressData.isEmpty && dut.io.axiSlave.b.valid.toBoolean) {
                    val data = writeAddressData.dequeue()

                    data.foreach({case (address, value) => {
                        // printf("%x %x\n", address, value)
                        model.remove(address)
                        model.put(address, value)
                    }})
                }

                dut.clockDomain.waitFallingEdge()
            }
        }

        dut.clockDomain.forkStimulus(frequency = HertzNumber(200000000))

        val readAddress = Queue[BigInt]()
        val readLength = Queue[BigInt]()
        val readDataAddress = Queue[BigInt]()

        val writeAddress = Queue[BigInt]()
        val writeData = Queue[Seq[BigInt]]()
        val writeAddressData = Queue[Queue[(BigInt, BigInt)]]()
        val model = Map[BigInt, BigInt]()

        dut.io.axiSlave.aw.payload.id #= 0
        dut.io.axiSlave.b.ready #= true
        dut.io.axiSlave.ar.payload.id #= 0
        dut.io.axiSlave.r.ready #= true

        val writeThread = fork(write(dut, writeAddress, writeData, writeAddressData))
        val writeResponseThread = fork(writeResponse(dut, writeAddressData, model))
        val readAddressThread = fork(read(dut, readAddress, readLength, readDataAddress))
        val readDataThread = fork(readData(dut, readDataAddress, model))

        val rand = new scala.util.Random

        dut.clockDomain.waitSampling()

        (0 until 1000).foreach(i => {
            val address = BigInt(16, rand)
            val length = BigInt(8, rand) + 1

            val items = Seq.fill(length.toInt)(BigInt(dut.parameters.dqParallel * dut.parameters.burstLength * dut.parameters.device.DQ_BITS, rand))

            writeAddress.enqueue(address)
            writeData.enqueue(items)
        })

        // waitUntil(dut.io.axiSlave.b.valid.toBoolean)

        (0 until 1000).foreach(i => {
            val address = BigInt(16, rand)
            val length = BigInt(8, rand) + 1

            readAddress.enqueue(address)
            readLength.enqueue(length)
        })

        dut.clockDomain.waitSampling(200000)
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
                                .addSimulatorFlag("-gno-io-range-error")
                                .addSimulatorFlag("-s glbl")
                                .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                .compile(
                                    ControllerSimulationModel(
                                        parameters = DDR3Parameters(
                                            synthesis = false
                                        )
                                    )
                                )

        compiled.doSim(dut => test(dut))
    }
}