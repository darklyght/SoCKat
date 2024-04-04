package sockat.utilities

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

import sockat.primitives._

case class ClockGeneratorParameters (
    mmcm: MMCME2_ADVParameters = MMCME2_ADVParameters()
)

case class PhaseUpdateInterface (
) extends Bundle with IMasterSlave {
    val clock = UInt(log2Up(7) bits)
    val increment = Bool()

    override def asMaster() = {
        out(clock)
        out(increment)
    }
}

case class ClockGenerator (
    parameters: ClockGeneratorParameters
) extends Component {
    val io = new Bundle {
        val clocks = out(Vec.fill(7)(differential(Bool())))
        val phaseUpdate = slave(Stream(PhaseUpdateInterface()))
    }

    val addresses = Vec(
        Vec(U"7'h08", U"7'h09"),
        Vec(U"7'h0A", U"7'h0B"),
        Vec(U"7'h0C", U"7'h0D"),
        Vec(U"7'h0E", U"7'h0F"),
        Vec(U"7'h10", U"7'h11"),
        Vec(U"7'h06", U"7'h07"),
        Vec(U"7'h12", U"7'h13")
    )

    val mmcm = MMCME2_ADV(parameters.mmcm)

    mmcm.io.clkIn1 := ClockDomain.current.readClockWire
    mmcm.io.clkIn2 := False
    mmcm.io.clkFbIn := mmcm.io.clkFbOut
    mmcm.io.clkInSel := 1
    mmcm.io.pwrDwn := False

    mmcm.io.dynamicReconfiguration.clk := ClockDomain.current.readClockWire

    mmcm.noPhaseShift()

    io.clocks(0).p := mmcm.io.clkOut0
    io.clocks(0).n := mmcm.io.clkOut0B
    io.clocks(1).p := mmcm.io.clkOut1
    io.clocks(1).n := mmcm.io.clkOut1B
    io.clocks(2).p := mmcm.io.clkOut2
    io.clocks(2).n := mmcm.io.clkOut2B
    io.clocks(3).p := mmcm.io.clkOut3
    io.clocks(3).n := mmcm.io.clkOut3B
    io.clocks(4).p := mmcm.io.clkOut4
    io.clocks(4).n := False
    io.clocks(5).p := mmcm.io.clkOut5
    io.clocks(5).n := False
    io.clocks(6).p := mmcm.io.clkOut6
    io.clocks(6).n := False

    val increment = Reg(Bool) init(False)
    val mmcmData = Reg(UInt(16 bits)) init(0)
    val mmcmPhase = Vec.fill(7)(
        Reg(UInt(9 bits)) init (0)
    )

    val fsm = new StateMachine {
        val clock = Reg(UInt(log2Up(7) bits)) init(0)
        val clockRegister = Reg(UInt(log2Up(2) bits)) init(0)

        mmcm.io.rst := True
        
        mmcm.io.dynamicReconfiguration.addr := 0
        mmcm.io.dynamicReconfiguration.i := 0
        mmcm.io.dynamicReconfiguration.we := False
        mmcm.io.dynamicReconfiguration.en := False
        io.phaseUpdate.ready := False

        val sReset = new State with EntryPoint
        val sInitialize = new State
        val sInitializeWait = new State
        val sWaitLocked = new State
        val sRead = new State
        val sReadWait = new State
        val sWrite = new State
        val sWriteWait = new State
        val sReady = new State
        
        sReset
            .onEntry()
            .whenIsActive({
                clock := 0
                clockRegister := 0
                mmcmPhase.foreach(register => {
                    register := 0
                })
                goto(sWaitLocked)
            })
            .onExit()

        sInitialize
            .onEntry()
            .whenIsActive({
                mmcm.io.dynamicReconfiguration.addr := addresses(clock)(clockRegister)
                mmcm.io.dynamicReconfiguration.en := True
            })
            .onExit()

        sInitializeWait
            .onEntry()
            .whenIsActive({
                mmcm.io.dynamicReconfiguration.en := True
                when (mmcm.io.dynamicReconfiguration.rdy) {
                    when (clockRegister === 0) {
                        mmcmPhase(clock)(2 downto 0) := mmcm.io.dynamicReconfiguration.o(15 downto 13)
                    }

                    when (clockRegister === 1) {
                        mmcmPhase(clock)(8 downto 3) := mmcm.io.dynamicReconfiguration.o(5 downto 0)
                    }
                
                    when (clockRegister === 1) {
                        when (clock === 6) {
                            clock := 0
                            clockRegister := 0
                            goto(sWaitLocked)
                        } otherwise {
                            clock := clock + 1
                            clockRegister := 0
                            goto(sInitialize)
                        }
                    } otherwise {
                        clockRegister := clockRegister + 1
                        goto(sInitialize)
                    }
                }
            })
            .onExit()

        sWaitLocked
            .onEntry()
            .whenIsActive({
                mmcm.io.rst := False
                when (mmcm.io.locked) {
                    goto(sReady)
                }
            })
            .onExit()

        sRead
            .onEntry()
            .whenIsActive({
                mmcm.io.dynamicReconfiguration.addr := addresses(clock)(clockRegister)
                mmcm.io.dynamicReconfiguration.en := True
                when (increment) {
                    mmcmPhase(clock) := mmcmPhase(clock) + 1
                } otherwise {
                    mmcmPhase(clock) := mmcmPhase(clock) - 1
                }
                goto(sReadWait)
            })
            .onExit()

        sReadWait
            .onEntry()
            .whenIsActive({
                when (mmcm.io.dynamicReconfiguration.rdy) {
                    mmcmData := mmcm.io.dynamicReconfiguration.o
                    goto(sWrite)
                }
            })
            .onExit()

        sWrite
            .onEntry()
            .whenIsActive({
                mmcm.io.dynamicReconfiguration.addr := addresses(clock)(clockRegister)
                mmcm.io.dynamicReconfiguration.en := True
                mmcm.io.dynamicReconfiguration.we := True
                when (clockRegister === 0) {
                    mmcm.io.dynamicReconfiguration.i := Cat(mmcmPhase(clock)(2 downto 0), mmcmData(12 downto 0)).asUInt
                }

                when (clockRegister === 1) {
                    mmcm.io.dynamicReconfiguration.i := Cat(mmcmData(15 downto 6), mmcmPhase(clock)(8 downto 3)).asUInt
                }
                goto(sWriteWait)
            })
            .onExit()

        sWriteWait
            .onEntry()
            .whenIsActive({
                when (mmcm.io.dynamicReconfiguration.rdy) {
                    when (clockRegister === 1) {
                        clockRegister := 0
                        goto(sWaitLocked)
                    } otherwise {
                        clockRegister := clockRegister + 1
                        goto(sRead)
                    }
                }
            })
            .onExit()

        sReady
            .onEntry()
            .whenIsActive({
                io.phaseUpdate.ready := True
                mmcm.io.rst := False
                when (io.phaseUpdate.valid) {
                    clock := io.phaseUpdate.clock
                    increment := io.phaseUpdate.increment
                    clockRegister := 0
                    goto(sRead)
                }
            })
            .onExit()
    }
}

object ClockGeneratorVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/utilities/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            ClockGenerator(ClockGeneratorParameters())
        )
    }
}

object ClockGeneratorSimulation {
    def test(
        dut: ClockGenerator
    ) = {
        dut.clockDomain.forkStimulus(frequency = HertzNumber(200000000))

        dut.io.phaseUpdate.payload.clock #= 0
        dut.io.phaseUpdate.payload.increment #= false
        dut.io.phaseUpdate.valid #= false

        dut.clockDomain.waitSampling(200)

        for (i <- 0 until 100) {
            waitUntil(dut.io.phaseUpdate.ready.toBoolean == true)

            dut.io.phaseUpdate.payload.clock #= 1
            dut.io.phaseUpdate.payload.increment #= true
            dut.io.phaseUpdate.valid #= true

            dut.clockDomain.waitSampling()

            dut.io.phaseUpdate.valid #= false

            dut.clockDomain.waitSampling(1000)
        }
    }

    def main(
        args: Array[String]
    ) = {
        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .addSimulatorFlag("-g2012")
                                .addSimulatorFlag("-s glbl")
                                .compile(
                                    ClockGenerator(ClockGeneratorParameters(
                                        mmcm = MMCME2_ADVParameters(
                                            clkIn1Period = 5,
                                            clkFbOutMultF = 8,
                                            clkOut0DivideF = 2,
                                            clkOut1Divide = 2,
                                            divClkDivide = 2
                                        )
                                    ))
                                )

        compiled.doSim(dut => test(dut))
    }
}