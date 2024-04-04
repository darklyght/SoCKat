package sockat.ddr3

import spinal.core._
import spinal.lib._

case class MR0 (
    burstLength: BurstLength = BurstLength("Fixed BL8"),
    casLatency: CASLatency = CASLatency(6),
    readBurstType: ReadBurstType = ReadBurstType("Sequential"),
    dllReset: DLLReset = DLLReset("No"),
    writeRecovery: WriteRecovery = WriteRecovery(6),
    prechargePD: PrechargePD = PrechargePD("Off")
) extends ModeRegister {
    override val index = 0

    def value = Cat(
        U"1'b0",
        register,
        U"1'b0",
        prechargePD.asUInt,
        writeRecovery.asUInt,
        dllReset.asUInt,
        U"1'b0",
        casLatency.asUInt(3 downto 1),
        readBurstType.asUInt,
        casLatency.asUInt(0),
        burstLength.asUInt
    ).asUInt
}

case class MR1 (
    dllEnable: DLLEnable = DLLEnable("Enable"),
    outputDriveStrength: OutputDriveStrength = OutputDriveStrength("RZQ/6"),
    rTT: RTT = RTT("RZQ/6"),
    additiveLatency: AdditiveLatency = AdditiveLatency("Disabled"),
    writeLeveling: WriteLeveling = WriteLeveling("Disabled"),
    tDQS: TDQS = TDQS("Disabled"),
    qOff: QOff = QOff("Enabled")
) extends ModeRegister {
    override val index = 1

    def value = Cat(
        U"1'b0",
        register,
        U"1'b0",
        qOff.asUInt,
        tDQS.asUInt,
        U"1'b0",
        rTT.asUInt(2),
        U"1'b0",
        writeLeveling.asUInt,
        rTT.asUInt(1),
        outputDriveStrength.asUInt(1),
        additiveLatency.asUInt,
        rTT.asUInt(0),
        outputDriveStrength.asUInt(0),
        dllEnable.asUInt
    ).asUInt
}

case class MR2 (
    casWriteLatency: CASWriteLatency = CASWriteLatency(5),
    autoSelfRefresh: AutoSelfRefresh = AutoSelfRefresh("Disabled"),
    selfRefreshTemperature: SelfRefreshTemperature = SelfRefreshTemperature("Normal"),
    dynamicODT: DynamicODT = DynamicODT("Disabled")
) extends ModeRegister {
    override val index = 2

    def value = Cat(
        U"1'b0",
        register,
        U"3'b000",
        dynamicODT.asUInt,
        U"1'b0",
        selfRefreshTemperature.asUInt,
        autoSelfRefresh.asUInt,
        casWriteLatency.asUInt,
        U"3'b000"
    ).asUInt
}

case class MR3 (
    mprReadFunction: MPRReadFunction = MPRReadFunction("Predefined pattern"),
    mprEnable: MPREnable = MPREnable("Normal DRAM operation")
) extends ModeRegister {
    override val index = 3

    def value = Cat(
        U"1'b0",
        register,
        U"11'b00000000000",
        mprEnable.asUInt,
        mprReadFunction.asUInt
    )
}

case class BurstLength (
    value: String = "Fixed BL8"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Fixed BL8" => U"2'b00"
        case "On-the-fly" => U"2'b01"
        case "Fixed BC4" => U"2'b10"
    }
}

case class CASLatency (
    value: Int = 6
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case 5 => U"4'b0010"
        case 6 => U"4'b0100"
        case 7 => U"4'b0110"
        case 8 => U"4'b1000"
        case 9 => U"4'b1010"
        case 10 => U"4'b1100"
        case 11 => U"4'b1110"
        case 12 => U"4'b0001"
        case 13 => U"4'b0011"
        case 14 => U"4'b0101"
    }
}

case class ReadBurstType (
    value: String = "Sequential"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Sequential" => U"1'b0"
        case "Interleaved" => U"1'b1"
    }
}

case class DLLReset (
    value: String = "Yes"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "No" => U"1'b0"
        case "Yes" => U"1'b1"
    }
}

case class WriteRecovery (
    value: Int = 6
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case 16 => U"3'b000"
        case 5 => U"3'b001"
        case 6 => U"3'b010"
        case 7 => U"3'b011"
        case 8 => U"3'b100"
        case 10 => U"3'b101"
        case 12 => U"3'b110"
        case 14 => U"3'b111"
    }
}

case class PrechargePD (
    value: String = "Off"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Off" => U"1'b0"
        case "On" => U"1'b1"
    }
}

case class DLLEnable (
    value: String = "Enable"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Enable" => U"1'b0"
        case "Disable" => U"1'b1"
    }
}

case class OutputDriveStrength (
    value: String = "RZQ/6"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "RZQ/6" => U"2'b00"
        case "RZQ/7" => U"2'b01"
    }
}

case class RTT (
    value: String = "RZQ/6"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "RTT disabled" => U"3'b000"
        case "RZQ/4" => U"3'b001"
        case "RZQ/2" => U"3'b010"
        case "RZQ/6" => U"3'b011"
        case "RZQ/12" => U"3'b100"
        case "RZQ/8" => U"3'b101"
    }
}

case class AdditiveLatency (
    value: String = "Disabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Disabled" => U"2'b00"
        case "CL - 1" => U"2'b01"
        case "CL - 2" => U"2'b10"
    }
}

case class WriteLeveling (
    value: String = "Disabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Disabled" => U"1'b0"
        case "Enabled" => U"1'b1"
    }   
}

case class TDQS (
    value: String = "Disabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Disabled" => U"1'b0"
        case "Enabled" => U"1'b1"
    }
}

case class QOff (
    value: String = "Enabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Enabled" => U"1'b0"
        case "Disabled" => U"1'b1"
    }
}

case class CASWriteLatency (
    value: Int = 5
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case 5 => U"3'b000"
        case 6 => U"3'b001"
        case 7 => U"3'b010"
        case 8 => U"3'b011"
        case 9 => U"3'b100"
        case 10 => U"3'b101"
    }
}

case class AutoSelfRefresh (
    value: String = "Disabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Disabled" => U"1'b0"
        case "Enabled" => U"1'b1"
    }
}

case class SelfRefreshTemperature (
    value: String = "Normal"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Normal" => U"1'b0"
        case "Extended" => U"1'b1"
    }
}

case class DynamicODT (
    value: String = "Disabled"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Disabled" => U"2'b00"
        case "RZQ/4" => U"2'b01"
        case "RZQ/2" => U"2'b10"
    }
}

case class MPRReadFunction (
    value: String = "Predefined pattern"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Predefined pattern" => U"2'b00"
    }
}

case class MPREnable (
    value: String = "Normal DRAM operation"
) extends ModeRegisterField(value) {
    override def asUInt = value match {
        case "Normal DRAM operation" => U"1'b0"
        case "Dataflow from MPR" => U"1'b1"
    }
}

abstract class ModeRegister (
) {
    val index = 0

    def register = {
        U(index, 2 bits)
    }
}

abstract class ModeRegisterField (
    value: Any
) {
    def asUInt: UInt
}