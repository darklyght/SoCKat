package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.io._

import sockat.primitives._
import sockat.utilities._

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class WritePathInterface (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val cke = Bool()
    val odt = Bool()
    val commandToggle = Bool()
    val command = DeviceCommand(parameters)
    val writeToggle = Bool()
    val writeData = Vec.fill(parameters.dqParallel)(UInt(parameters.burstLength * parameters.device.DQ_BITS bits))
    val writeMask = Vec.fill(parameters.dqParallel)(UInt(parameters.burstLength * parameters.device.DM_BITS bits))
    val writeLevelingEnable = Bool()
    val writeLevelingToggle = Bool()
    val writeLevelingData = Vec.fill(parameters.dqParallel)(UInt(parameters.device.DQ_BITS bits))

    override def asMaster() = {
        out(cke)
        out(odt)
        out(commandToggle)
        master(command)
        out(writeToggle)
        out(writeData)
        out(writeMask)
        out(writeLevelingEnable)
        out(writeLevelingToggle)
        in(writeLevelingData)
    }
}

case class WritePath (
    parameters: DDR3Parameters,
    dqsClockDomain: ClockDomain
) extends Component {
    val io = new Bundle {
        val device = master(DeviceInternal(parameters))
        val internal = slave(WritePathInterface(parameters))
    }

    io.device.rst_n := False
    io.device.ck := False

    val odth = if (parameters.burstLength == 4) parameters.device.ODTH4 else parameters.device.ODTH8

    val ckeSync = Vec.fill(3)(
        Reg(Bool()) init(False) addTag(crossClockDomain)
    )

    val cs_nSync = Vec.fill(parameters.device.RANKS)(
        Vec.fill(3)(
            Reg(Bool()) init(True) addTag(crossClockDomain)
        )
    )

    val odtSync = Vec.fill(3)(
        Reg(Bool()) init(False) addTag(crossClockDomain)
    )

    val commandToggleSync = Vec.fill(4)(
        Reg(Bool()) init(False) addTag(crossClockDomain)
    )

    val ras_nSync = Vec.fill(3)(
        Reg(Bool()) init(True) addTag(crossClockDomain)
    )

    val cas_nSync = Vec.fill(3)(
        Reg(Bool()) init(True) addTag(crossClockDomain)
    )

    val we_nSync = Vec.fill(3)(
        Reg(Bool()) init(True) addTag(crossClockDomain)
    )

    val baSync = Vec.fill(3)(
        Reg(UInt(parameters.device.BA_BITS bits)) init(0) addTag(crossClockDomain)
    )

    val addrSync = Vec.fill(3)(
        Reg(UInt(parameters.device.ADDR_BITS bits)) init(0) addTag(crossClockDomain)
    )

    val writeToggleSync = Vec.fill(4)(
        Reg(Bool()) init(False) addTag(crossClockDomain)
    )

    val writeDataSync = Vec.fill(3)(
        Vec.fill(parameters.dqParallel)(
            Reg(UInt(parameters.burstLength * parameters.device.DQ_BITS bits)) init(0) addTag(crossClockDomain)
        )
    )

    val writeMaskSync = Vec.fill(3)(
        Vec.fill(parameters.dqParallel)(
            Reg(UInt(parameters.burstLength * parameters.device.DM_BITS bits)) init(0) addTag(crossClockDomain)
        )
    )

    val dqOddrIn = Vec((0 until parameters.dqParallel).map(i => {
        Vec((0 until parameters.burstLength).map(j => writeDataSync(0)(i)((j + 1) * parameters.device.DQ_BITS - 1 downto j * parameters.device.DQ_BITS)))
    }))
    
    val dmOddrIn = Vec((0 until parameters.dqParallel).map(i => {
        Vec((0 until parameters.burstLength).map(j => writeMaskSync(0)(i)((j + 1) * parameters.device.DM_BITS - 1 downto j * parameters.device.DM_BITS)))
    }))

    ckeSync(2) := io.internal.cke
    ckeSync(1) := ckeSync(2)
    ckeSync(0) := ckeSync(1)

    cs_nSync.zipWithIndex.foreach({case (sync, index) => {
        sync(2) := io.internal.command.cs_n(index)
        sync(1) := sync(2)
        sync(0) := sync(1)
    }})

    odtSync(2) := io.internal.odt
    odtSync(1) := odtSync(2)
    odtSync(0) := odtSync(1)
    
    commandToggleSync(3) := io.internal.commandToggle
    commandToggleSync(2) := commandToggleSync(3)
    commandToggleSync(1) := commandToggleSync(2)
    commandToggleSync(0) := commandToggleSync(1)

    ras_nSync(2) := io.internal.command.ras_n
    ras_nSync(1) := ras_nSync(2)
    ras_nSync(0) := ~(commandToggleSync(2) ^ commandToggleSync(1)) | ras_nSync(1)

    cas_nSync(2) := io.internal.command.cas_n
    cas_nSync(1) := cas_nSync(2)
    cas_nSync(0) := ~(commandToggleSync(2) ^ commandToggleSync(1)) | cas_nSync(1)

    we_nSync(2) := io.internal.command.we_n
    we_nSync(1) := we_nSync(2)
    we_nSync(0) := ~(commandToggleSync(2) ^ commandToggleSync(1)) | we_nSync(1)

    baSync(2) := io.internal.command.ba
    baSync(1) := baSync(2)
    baSync(0) := baSync(1)

    addrSync(2) := io.internal.command.addr
    addrSync(1) := addrSync(2)
    addrSync(0) := addrSync(1)

    writeToggleSync(3) := io.internal.writeToggle
    writeToggleSync(2) := writeToggleSync(3)
    writeToggleSync(1) := writeToggleSync(2)
    writeToggleSync(0) := writeToggleSync(2) ^ writeToggleSync(1)

    writeDataSync(2) := io.internal.writeData
    writeDataSync(1) := writeDataSync(2)
    writeDataSync(0) := writeDataSync(1)

    writeMaskSync(2) := io.internal.writeMask
    writeMaskSync(1) := writeMaskSync(2)
    writeMaskSync(0) := writeMaskSync(1)

    val odtSerializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = odth.tCKCycles(parameters.tCKPeriod),
            inputWidth = odth.tCKCycles(parameters.tCKPeriod),
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    val dqEnableSerializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2,
            inputWidth = parameters.burstLength / 2,
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    val writeDataRiseSerializer = Seq.fill(parameters.dqParallel) {
        ShiftRegister(
            ShiftRegisterParameters(
                dataType = UInt(parameters.device.DQ_BITS bits),
                width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 - 1,
                inputWidth = parameters.burstLength / 2,
                resetFunction = (register: UInt) => {
                    register init(0)
                },
                defaultFunction = (register: UInt) => {
                    register := 0
                }
            )
        )
    }

    val writeDataFallSerializer = Seq.fill(parameters.dqParallel) {
        ShiftRegister(
            ShiftRegisterParameters(
                dataType = UInt(parameters.device.DQ_BITS bits),
                width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 - 1,
                inputWidth = parameters.burstLength / 2,
                resetFunction = (register: UInt) => {
                    register init(0)
                },
                defaultFunction = (register: UInt) => {
                    register := 0
                }
            )
        )
    }

    val writeMaskRiseSerializer = Seq.fill(parameters.dqParallel) {
        ShiftRegister(
            ShiftRegisterParameters(
                dataType = UInt(parameters.device.DM_BITS bits),
                width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 - 1,
                inputWidth = parameters.burstLength / 2,
                resetFunction = (register: UInt) => {
                    register init(0)
                },
                defaultFunction = (register: UInt) => {
                    register := 0
                }
            )
        )
    }

    val writeMaskFallSerializer = Seq.fill(parameters.dqParallel) {
        ShiftRegister(
            ShiftRegisterParameters(
                dataType = UInt(parameters.device.DM_BITS bits),
                width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 - 1,
                inputWidth = parameters.burstLength / 2,
                resetFunction = (register: UInt) => {
                    register init(0)
                },
                defaultFunction = (register: UInt) => {
                    register := 0
                }
            )
        )
    }

    val ckeOddr = ODDR(
        ODDRParameters(
            ddrClkEdge = "SAME_EDGE",
            init = 1
        )
    )

    val cs_nOddr = Seq.fill(parameters.device.RANKS) {
        ODDR(
            ODDRParameters(
                ddrClkEdge = "SAME_EDGE",
                init = 1
            )
        )
    }

    val odtOddr = ODDR(
        ODDRParameters(
            ddrClkEdge = "SAME_EDGE",
            init = 1
        )
    )

    val ras_nOddr = ODDR(
        ODDRParameters(
            ddrClkEdge = "SAME_EDGE",
            init = 1
        )
    )

    val cas_nOddr = ODDR(
        ODDRParameters(
            ddrClkEdge = "SAME_EDGE",
            init = 1
        )
    )

    val we_nOddr = ODDR(
        ODDRParameters(
            ddrClkEdge = "SAME_EDGE",
            init = 1
        )
    )

    val baOddr = Seq.fill(parameters.device.BA_BITS) {
        ODDR(
            ODDRParameters(
                ddrClkEdge = "SAME_EDGE",
                init = 1
            )
        )
    }

    val addrOddr = Seq.fill(parameters.device.ADDR_BITS) {
        ODDR(
            ODDRParameters(
                ddrClkEdge = "SAME_EDGE",
                init = 1
            )
        )
    }

    val dqOddr = Seq.fill(parameters.dqParallel) {
        Seq.fill(parameters.device.DQ_BITS) {
            ODDR(
                ODDRParameters(
                    ddrClkEdge = "SAME_EDGE",
                    init = 1
                )
            )
        }
    }

    val dmOddr = Seq.fill(parameters.dqParallel) {
        Seq.fill(parameters.device.DM_BITS) {
            ODDR(
                ODDRParameters(
                    ddrClkEdge = "SAME_EDGE",
                    init = 1
                )
            )
        }
    }

    odtSerializer.io.load := odtSync(0) | writeToggleSync(0)
    odtSerializer.io.shift := True
    odtSerializer.io.input := Vec(True, odth.tCKCycles(parameters.tCKPeriod))

    dqEnableSerializer.io.load := writeToggleSync(0)
    dqEnableSerializer.io.shift := True
    dqEnableSerializer.io.input := Vec(True, parameters.burstLength / 2)

    writeDataRiseSerializer.zipWithIndex.foreach({case (serializer, index) => {
        serializer.io.load := writeToggleSync(0)
        serializer.io.shift := True
        serializer.io.input := Vec(
            (parameters.burstLength / 2 - 1 to 0 by -1).map(i => {
                dqOddrIn(index)(i * 2 + 1)
            })
        )
    }})

    writeDataFallSerializer.zipWithIndex.foreach({case (serializer, index) => {
        serializer.io.load := writeToggleSync(0)
        serializer.io.shift := True
        serializer.io.input := Vec(
            (parameters.burstLength / 2 - 1 to 0 by -1).map(i => {
                dqOddrIn(index)(i * 2)
            })
        )
    }})

    writeMaskRiseSerializer.zipWithIndex.foreach({case (serializer, index) => {
        serializer.io.load := writeToggleSync(0)
        serializer.io.shift := True
        serializer.io.input := Vec(
            (parameters.burstLength / 2 - 1 to 0 by -1).map(i => {
                dmOddrIn(index)(i * 2 + 1)
            })
        )
    }})

    writeMaskFallSerializer.zipWithIndex.foreach({case (serializer, index) => {
        serializer.io.load := writeToggleSync(0)
        serializer.io.shift := True
        serializer.io.input := Vec(
            (parameters.burstLength / 2 - 1 to 0 by -1).map(i => {
                dmOddrIn(index)(i * 2)
            })
        )
    }})

    ckeOddr.io.c := ClockDomain.current.readClockWire
    ckeOddr.io.ce := True
    ckeOddr.io.d1 := ckeSync(0)
    ckeOddr.io.d2 := ckeSync(0)
    ckeOddr.io.r := ClockDomain.current.readResetWire
    ckeOddr.io.s := False
    io.device.cke := ckeOddr.io.q

    cs_nOddr.zipWithIndex.foreach({case (oddr, index) => {
        oddr.io.c := ClockDomain.current.readClockWire
        oddr.io.ce := True
        oddr.io.d1 := cs_nSync(index)(0)
        oddr.io.d2 := cs_nSync(index)(0)
        oddr.io.r := ClockDomain.current.readResetWire
        oddr.io.s := False
        io.device.command.cs_n(index) := oddr.io.q
    }})

    odtOddr.io.c := ClockDomain.current.readClockWire
    odtOddr.io.ce := True
    odtOddr.io.d1 := odtSerializer.io.output(0) | odtSync(0)
    odtOddr.io.d2 := odtSerializer.io.output(0) | odtSync(0)
    odtOddr.io.r := ClockDomain.current.readResetWire
    odtOddr.io.s := False
    io.device.odt := odtOddr.io.q

    ras_nOddr.io.c := ClockDomain.current.readClockWire
    ras_nOddr.io.ce := True
    ras_nOddr.io.d1 := ras_nSync(0)
    ras_nOddr.io.d2 := ras_nSync(0)
    ras_nOddr.io.r := False
    ras_nOddr.io.s := ClockDomain.current.readResetWire
    io.device.command.ras_n := ras_nOddr.io.q

    cas_nOddr.io.c := ClockDomain.current.readClockWire
    cas_nOddr.io.ce := True
    cas_nOddr.io.d1 := cas_nSync(0)
    cas_nOddr.io.d2 := cas_nSync(0)
    cas_nOddr.io.r := False
    cas_nOddr.io.s := ClockDomain.current.readResetWire
    io.device.command.cas_n := cas_nOddr.io.q

    we_nOddr.io.c := ClockDomain.current.readClockWire
    we_nOddr.io.ce := True
    we_nOddr.io.d1 := we_nSync(0)
    we_nOddr.io.d2 := we_nSync(0)
    we_nOddr.io.r := False
    we_nOddr.io.s := ClockDomain.current.readResetWire
    io.device.command.we_n := we_nOddr.io.q

    baOddr.zipWithIndex.foreach({case (oddr, index) => {
        oddr.io.c := ClockDomain.current.readClockWire
        oddr.io.ce := True
        oddr.io.d1 := baSync(0)(index)
        oddr.io.d2 := baSync(0)(index)
        oddr.io.r := ClockDomain.current.readResetWire
        oddr.io.s := False
        io.device.command.ba(index) := oddr.io.q
    }})

    addrOddr.zipWithIndex.foreach({case (oddr, index) => {
        oddr.io.c := ClockDomain.current.readClockWire
        oddr.io.ce := True
        oddr.io.d1 := addrSync(0)(index)
        oddr.io.d2 := addrSync(0)(index)
        oddr.io.r := ClockDomain.current.readResetWire
        oddr.io.s := False
        io.device.command.addr(index) := oddr.io.q
    }})

    dqOddr.zipWithIndex.foreach({case (chip, i) => {
        chip.zipWithIndex.foreach({case (oddr, j) => {
            oddr.io.c := ClockDomain.current.readClockWire
            oddr.io.ce := True
            oddr.io.d1 := writeDataRiseSerializer(i).io.output(0)(j)
            oddr.io.d2 := writeDataFallSerializer(i).io.output(0)(j)
            oddr.io.r := ClockDomain.current.readResetWire
            oddr.io.s := False
            io.device.dq(i)(j).write := oddr.io.q
            io.device.dq(i)(j).writeEnable := dqEnableSerializer.io.output(0)
            io.internal.writeLevelingData(i)(j) := io.device.dq(i)(j).read
        }})
    }})
    
    dmOddr.zipWithIndex.foreach({case (chip, i) => {
        chip.zipWithIndex.foreach({case (oddr, j) => {
            oddr.io.c := ClockDomain.current.readClockWire
            oddr.io.ce := True
            oddr.io.d1 := writeMaskRiseSerializer(i).io.output(0)(j)
            oddr.io.d2 := writeMaskFallSerializer(i).io.output(0)(j)
            oddr.io.r := ClockDomain.current.readResetWire
            oddr.io.s := False
            io.device.dm(i)(j).write := oddr.io.q
            io.device.dm(i)(j).writeEnable := dqEnableSerializer.io.output(0)
        }})
    }})

    val dqsClockArea = new ClockingArea(dqsClockDomain) {
        val writeToggleSync = Vec.fill(3)(
            Reg(Bool()) init(False) addTag(crossClockDomain)
        )

        val writeLevelingEnableSync = Vec.fill(4)(
            Reg(Bool()) init(False) addTag(crossClockDomain)
        )

        val writeLevelingToggleSync = Vec.fill(4)(
            Reg(Bool()) init(False) addTag(crossClockDomain)
        )

        writeToggleSync(2) := io.internal.writeToggle
        writeToggleSync(1) := writeToggleSync(2)
        writeToggleSync(0) := writeToggleSync(1)

        writeLevelingEnableSync(3) := io.internal.writeLevelingEnable
        writeLevelingEnableSync(2) := writeLevelingEnableSync(3)
        writeLevelingEnableSync(1) := writeLevelingEnableSync(2)
        writeLevelingEnableSync(0) := writeLevelingEnableSync(1)

        writeLevelingToggleSync(3) := io.internal.writeLevelingToggle
        writeLevelingToggleSync(2) := writeLevelingToggleSync(3)
        writeLevelingToggleSync(1) := writeLevelingToggleSync(2)
        writeLevelingToggleSync(0) := writeLevelingToggleSync(2) ^ writeLevelingToggleSync(1)

        val dqsRise = Mux(writeLevelingEnableSync(1), writeLevelingToggleSync(0), True)

        val dqsEnableSerializer = ShiftRegister(
            ShiftRegisterParameters(
                dataType = Bool(),
                width = parameters.writeLatency + parameters.additiveLatency + parameters.burstLength / 2 + 2,
                inputWidth = parameters.burstLength / 2 + 2,
                resetFunction = (register: Bool) => {
                    register init(False)
                },
                defaultFunction = (register: Bool) => {
                    register := False
                }
            )
        )

        val dqsOddr = ODDR(
            ODDRParameters(
                ddrClkEdge = "SAME_EDGE"
            )
        )

        dqsEnableSerializer.io.load := writeToggleSync(1) ^ writeToggleSync(0)
        dqsEnableSerializer.io.shift := True
        dqsEnableSerializer.io.input := Vec(True, parameters.burstLength / 2 + 2)

        dqsOddr.io.c := ClockDomain.current.readClockWire
        dqsOddr.io.ce := True
        dqsOddr.io.d1 := dqsRise
        dqsOddr.io.d2 := False
        dqsOddr.io.r := ClockDomain.current.readResetWire
        dqsOddr.io.s := False
        (0 until parameters.device.DQS_BITS).foreach(i => {
            io.device.dqs.write(i) := dqsOddr.io.q
            io.device.dqs.writeEnable(i) := (writeLevelingEnableSync(0) | dqsEnableSerializer.io.output(0))
        })
    }
}

object WritePathVerilog {
    def main(
        args: Array[String]
    ) = {
        val parameters = DDR3Parameters()

        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/ddr3/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            WritePath(
                parameters = parameters,
                dqsClockDomain = ClockDomain.external(
                    name = "dqsClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = HIGH,
                        clockEnableActiveLevel = HIGH
                    )
                )
            )
        )
    }
}

object WritePathSimulation {
    def test(
        dut: WritePath
    ) = {
        def driver(
            dut: WritePath,
            queue: Queue[Seq[BigInt]]
        ) = {
            val rand = new scala.util.Random

            while (true) {
                var buffer = Seq.fill(dut.parameters.dqParallel) {
                    BigInt(dut.parameters.burstLength * dut.parameters.device.DQ_BITS, rand)
                }
                var mask = Seq.fill(dut.parameters.dqParallel) {
                    BigInt(dut.parameters.burstLength * dut.parameters.device.DM_BITS, rand)
                }
                val value = buffer.zip(mask).map({case (data, bitmask) => {
                    (0 until dut.parameters.burstLength * dut.parameters.device.DM_BITS).map(i => {
                        if (bitmask.testBit(i)) data & (((BigInt(1) << 8) - 1) << (i * 8)) else BigInt(0)
                    }).foldLeft(BigInt(0))((acc, bit) => {
                        acc | bit
                    })
                }})
                
                if (queue.isEmpty) {
                    dut.io.internal.command.ras_n #= true
                    dut.io.internal.command.cas_n #= false
                    dut.io.internal.command.we_n #= false
                    dut.io.internal.commandToggle #= !dut.io.internal.commandToggle.toBoolean
                    dut.io.internal.writeToggle #= !dut.io.internal.writeToggle.toBoolean
                    dut.io.internal.writeData.zip(buffer).foreach({case (input, data) => {
                        input #= data
                    }})
                    dut.io.internal.writeMask.zip(mask).foreach({case (input, data) => {
                        input #= data
                    }})

                    queue.enqueue(value)
                }

                dut.clockDomain.waitSampling()
            }
        }

        def monitor(
            dut: WritePath,
            dqsClock: ClockDomain,
            queue: Queue[Seq[BigInt]]
        ) = {
            while (true) {
                if (!dut.io.device.command.we_n.toBoolean) {
                    dqsClock.waitRisingEdge(dut.parameters.writeLatency + dut.parameters.additiveLatency - 1)
                    sleep(1)
                    assert(dut.io.device.dqs.writeEnable.toInt == (1 << dut.parameters.device.DQS_BITS) - 1)
                    dqsClock.waitRisingEdge()

                    var buffer = ArrayBuffer.fill(dut.parameters.dqParallel) {
                        BigInt(0)
                    }
                    var value = Seq.fill(dut.parameters.dqParallel) {
                        BigInt(0)
                    }
                    for (i <- dut.parameters.burstLength / 2 - 1 to 0 by -1) {
                        dut.io.device.dq.foreach(dq => {
                            assert(dq.writeEnable.toBigInt == (BigInt(1) << dut.parameters.device.DQ_BITS) - 1)
                        })
                        dut.io.device.dm.foreach(dm => {
                            assert(dm.writeEnable.toBigInt == (BigInt(1) << dut.parameters.device.DM_BITS) - 1)
                        })
                        
                        value = (0 until dut.parameters.dqParallel).map(chip => {
                            (0 until dut.parameters.device.DM_BITS).map(bit => {
                                if (dut.io.device.dm(chip).write.toBigInt.testBit(bit)) dut.io.device.dq(chip).write.toBigInt & (((BigInt(1) << 8) - 1) << (bit * 8)) else BigInt(0)
                            }).foldLeft(BigInt(0))((acc, bit) => {
                                acc | bit
                            })
                        })
                        
                        (0 until dut.parameters.dqParallel).foreach(chip => {
                            buffer(chip) = buffer(chip) | (value(chip) << ((i * 2 + 1) * dut.parameters.device.DQ_BITS))
                        })

                        dqsClock.waitFallingEdge()

                        dut.io.device.dq.foreach(dq => {
                            assert(dq.writeEnable.toBigInt == (BigInt(1) << dut.parameters.device.DQ_BITS) - 1)
                        })
                        dut.io.device.dm.foreach(dm => {
                            assert(dm.writeEnable.toBigInt == (BigInt(1) << dut.parameters.device.DM_BITS) - 1)
                        })

                        value = (0 until dut.parameters.dqParallel).map(chip => {
                            (0 until dut.parameters.device.DM_BITS).map(bit => {
                                if (dut.io.device.dm(chip).write.toBigInt.testBit(bit)) dut.io.device.dq(chip).write.toBigInt & (((BigInt(1) << 8) - 1) << (bit * 8)) else BigInt(0)
                            }).foldLeft(BigInt(0))((acc, bit) => {
                                acc | bit
                            })
                        })

                        (0 until dut.parameters.dqParallel).foreach(chip => {
                            buffer(chip) = buffer(chip) | (value(chip) << ((i * 2) * dut.parameters.device.DQ_BITS))
                        })

                        dqsClock.waitRisingEdge()
                    }

                    assert(buffer == queue.dequeue())
                }
                dqsClock.waitSampling()
            }
        }

        val queue = Queue[Seq[BigInt]]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(400000000))
        dut.clockDomain.waitRisingEdge()
        sleep(625)
        dut.dqsClockDomain.forkStimulus(frequency = HertzNumber(400000000))
        
        dut.io.internal.command.ras_n #= true
        dut.io.internal.command.cas_n #= true
        dut.io.internal.command.we_n #= true
        dut.io.internal.commandToggle #= false
        dut.io.internal.writeToggle #= false
        dut.io.internal.writeData.foreach(_ #= 0)
        dut.io.internal.writeMask.foreach(_ #= 0)
        dut.io.internal.writeLevelingEnable #= false
        dut.io.internal.writeLevelingToggle #= false

        dut.clockDomain.waitSampling(200)

        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, dut.dqsClockDomain, queue))

        dut.clockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val burstLengths = Seq(4, 8)
        val writeLatencies = Seq(4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

        val tests = for {
            i <- burstLengths
            j <- writeLatencies
        } yield (i, j)

        tests.foreach({case (burstLength, writeLatency) => {
            println(burstLength, writeLatency)

            val parameters = DDR3Parameters(burstLength = burstLength, writeLatency = writeLatency)

            val compiled = SimConfig.withIVerilog
                                    .withFstWave
                                    .addSimulatorFlag("-D den4096Mb")
                                    .addSimulatorFlag("-D sg187E")
                                    .addSimulatorFlag("-D x16")
                                    .addSimulatorFlag("-g2012")
                                    .addSimulatorFlag("-s glbl")
                                    .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                    .compile(
                                        WritePath(
                                            parameters = parameters,
                                            dqsClockDomain = ClockDomain.external(
                                                name = "dqsClock",
                                                config = ClockDomainConfig(
                                                    clockEdge = RISING,
                                                    resetKind = ASYNC,
                                                    resetActiveLevel = HIGH,
                                                    clockEnableActiveLevel = HIGH
                                                )
                                            )
                                        )
                                    )
            
            compiled.doSim(dut => test(dut))
        }})
        
    }
}