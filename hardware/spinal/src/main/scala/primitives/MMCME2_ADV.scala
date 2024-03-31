package sockat.primitives

import spinal.core._
import spinal.lib._

case class MMCME2_ADVParameters (
    bandwidth: String = "OPTIMIZED",
    clkOut1Divide: Int = 1,
    clkOut2Divide: Int = 1,
    clkOut3Divide: Int = 1,
    clkOut4Divide: Int = 1,
    clkOut5Divide: Int = 1,
    clkOut6Divide: Int = 1,
    clkOut0DivideF: Double = 1,
    clkOut0Phase: Double = 0.0,
    clkOut1Phase: Double = 0.0,
    clkOut2Phase: Double = 0.0,
    clkOut3Phase: Double = 0.0,
    clkOut4Phase: Double = 0.0,
    clkOut5Phase: Double = 0.0,
    clkOut6Phase: Double = 0.0,
    clkOut0DutyCycle: Double = 0.5,
    clkOut1DutyCycle: Double = 0.5,
    clkOut2DutyCycle: Double = 0.5,
    clkOut3DutyCycle: Double = 0.5,
    clkOut4DutyCycle: Double = 0.5,
    clkOut5DutyCycle: Double = 0.5,
    clkOut6DutyCycle: Double = 0.5,
    clkFbOutMultF: Double = 5,
    divClkDivide: Int = 1,
    clkFbOutPhase: Double = 0.0,
    refJitter1: Double = 0.010,
    refJitter2: Double = 0.010,
    clkIn1Period: Double = 0.000,
    clkIn2Period: Double = 0.000,
    clkFbOutUseFinePs: String = "FALSE",
    clkOut0UseFinePs: String = "FALSE",
    clkOut1UseFinePs: String = "FALSE",
    clkOut2UseFinePs: String = "FALSE",
    clkOut3UseFinePs: String = "FALSE",
    clkOut4UseFinePs: String = "FALSE",
    clkOut5UseFinePs: String = "FALSE",
    clkOut6UseFinePs: String = "FALSE",
    startupWait: String = "FALSE",
    clkOut4Cascade: String = "FALSE"
)

case class MMCME2_ADV (
    parameters: MMCME2_ADVParameters,
) extends BlackBox {
    addGeneric("BANDWIDTH", parameters.bandwidth)
    addGeneric("CLKOUT1_DIVIDE", parameters.clkOut1Divide)
    addGeneric("CLKOUT2_DIVIDE", parameters.clkOut2Divide)
    addGeneric("CLKOUT3_DIVIDE", parameters.clkOut3Divide)
    addGeneric("CLKOUT4_DIVIDE", parameters.clkOut4Divide)
    addGeneric("CLKOUT5_DIVIDE", parameters.clkOut5Divide)
    addGeneric("CLKOUT6_DIVIDE", parameters.clkOut6Divide)
    addGeneric("CLKOUT0_DIVIDE_F", parameters.clkOut0DivideF)
    addGeneric("CLKOUT0_PHASE", parameters.clkOut0Phase)
    addGeneric("CLKOUT1_PHASE", parameters.clkOut1Phase)
    addGeneric("CLKOUT2_PHASE", parameters.clkOut2Phase)
    addGeneric("CLKOUT3_PHASE", parameters.clkOut3Phase)
    addGeneric("CLKOUT4_PHASE", parameters.clkOut4Phase)
    addGeneric("CLKOUT5_PHASE", parameters.clkOut5Phase)
    addGeneric("CLKOUT6_PHASE", parameters.clkOut6Phase)
    addGeneric("CLKOUT0_DUTY_CYCLE", parameters.clkOut0DutyCycle)
    addGeneric("CLKOUT1_DUTY_CYCLE", parameters.clkOut1DutyCycle)
    addGeneric("CLKOUT2_DUTY_CYCLE", parameters.clkOut2DutyCycle)
    addGeneric("CLKOUT3_DUTY_CYCLE", parameters.clkOut3DutyCycle)
    addGeneric("CLKOUT4_DUTY_CYCLE", parameters.clkOut4DutyCycle)
    addGeneric("CLKOUT5_DUTY_CYCLE", parameters.clkOut5DutyCycle)
    addGeneric("CLKOUT6_DUTY_CYCLE", parameters.clkOut6DutyCycle)
    addGeneric("CLKFBOUT_MULT_F", parameters.clkFbOutMultF)
    addGeneric("DIVCLK_DIVIDE", parameters.divClkDivide)
    addGeneric("CLKFBOUT_PHASE", parameters.clkFbOutPhase)
    addGeneric("REF_JITTER1", parameters.refJitter1)
    addGeneric("REF_JITTER2", parameters.refJitter2)
    addGeneric("CLKIN1_PERIOD", parameters.clkIn1Period)
    addGeneric("CLKIN2_PERIOD", parameters.clkIn2Period)
    addGeneric("CLKFBOUT_USE_FINE_PS", parameters.clkFbOutUseFinePs)
    addGeneric("CLKOUT0_USE_FINE_PS", parameters.clkOut0UseFinePs)
    addGeneric("CLKOUT1_USE_FINE_PS", parameters.clkOut1UseFinePs)
    addGeneric("CLKOUT2_USE_FINE_PS", parameters.clkOut2UseFinePs)
    addGeneric("CLKOUT3_USE_FINE_PS", parameters.clkOut3UseFinePs)
    addGeneric("CLKOUT4_USE_FINE_PS", parameters.clkOut4UseFinePs)
    addGeneric("CLKOUT5_USE_FINE_PS", parameters.clkOut5UseFinePs)
    addGeneric("CLKOUT6_USE_FINE_PS", parameters.clkOut6UseFinePs)
    addGeneric("STARTUP_WAIT", parameters.startupWait)
    addGeneric("CLKOUT4_CASCADE", parameters.clkOut4Cascade)

    val io = new Bundle {
        val clkIn1 = in Bool()
        val clkIn2 = in Bool()
        val clkFbIn = in Bool()
        val clkInSel = in UInt(1 bits)
        val rst = in Bool()
        val pwrDwn  = in Bool()
        val dynamicReconfiguration = slave(DynamicReconfigurationInterface())
        val phaseShift = slave(PhaseShiftInterface())
        val clkOut0 = out Bool()
        val clkOut1 = out Bool()
        val clkOut2 = out Bool()
        val clkOut3 = out Bool()
        val clkOut4 = out Bool()
        val clkOut5 = out Bool()
        val clkOut6 = out Bool()
        val clkOut0B = out Bool()
        val clkOut1B = out Bool()
        val clkOut2B = out Bool()
        val clkOut3B = out Bool()
        val clkFbOut = out Bool()
        val clkFbOutB = out Bool()
        val clkInStopped = out Bool()
        val clkFbStopped = out Bool()
        val locked = out Bool()
    }

    private def renameIO() = {
        io.flatten.foreach(bt => {
            if (bt.getName().contains("dynamicReconfiguration_")) bt.setName(bt.getName().replace("dynamicReconfiguration_", "D"))
            if (bt.getName().contains("phaseShift_")) bt.setName(bt.getName().replace("phaseShift_", "PS"))
            bt.setName(bt.getName().toUpperCase())
        })
    }

    def getFrequencyMultiplier(
        clkOut: Int
    ): Double = {
        clkOut match {
            case 0 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut0DivideF
            case 1 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut1Divide
            case 2 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut2Divide
            case 3 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut3Divide
            case 4 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut4Divide
            case 5 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut5Divide
            case 6 => parameters.clkFbOutMultF / parameters.divClkDivide / parameters.clkOut6Divide
            case _ => 0
        }
    }

    def noPhaseShift() = {
        io.phaseShift.clk := False
        io.phaseShift.en := False
        io.phaseShift.incdec := False
    }

    def noDynamicReconfiguration() = {
        io.dynamicReconfiguration.addr := 0
        io.dynamicReconfiguration.i := 0
        io.dynamicReconfiguration.we := False
        io.dynamicReconfiguration.en := False
        io.dynamicReconfiguration.clk := False
    }

    noIoPrefix()

    addPrePopTask(() => renameIO())
}

case class PhaseShiftInterface(
) extends Bundle with IMasterSlave {
    val clk = Bool()
    val en = Bool()
    val incdec = Bool()
    val done = Bool()

    override def asMaster() = {
        out(clk)
        out(en)
        out(incdec)
        in(done)
    }
}

case class DynamicReconfigurationInterface(
) extends Bundle with IMasterSlave {
    val addr = in UInt(7 bits)
    val i = in UInt(16 bits)
    val we = in Bool()
    val en = in Bool()
    val clk = in Bool()
    val o = out UInt(16 bits)
    val rdy = out Bool()

    override def asMaster() = {
        out(addr)
        out(i)
        out(we)
        out(en)
        out(clk)
        in(o)
        in(rdy)
    }
}