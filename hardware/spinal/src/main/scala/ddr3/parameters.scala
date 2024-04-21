package sockat.ddr3

import spinal.core._

case class DDR3Parameters (
    synthesis: Boolean = true,
    density: String = "den4096Mb",
    speedGrade: String = "sg187E",
    width: String = "x16",
    rank: String = "DUAL_RANK",
    dqBusWidth: Int = 32,
    tCKPeriod: Double = 2.5,
    controllerClockRatio: Int = 2,
    ckClockRatio: Int = 2,
    burstLength: Int = 8,
    writeLatency: Int = 5,
    readLatency: Int = 6
) {
    assert(ckClockRatio == 2)
    assert(burstLength == 4 || burstLength == 8)

    val device: DDR3DeviceParameters = DDR3DeviceParameters(
        density,
        speedGrade,
        width,
        rank
    )

    val additiveLatency = if (device.TRCD.tCKCycles(tCKPeriod) == 0) 0
                          else if (device.TRCD.tCKCycles(tCKPeriod) < readLatency - 1) readLatency - 2
                          else if (device.TRCD.tCKCycles(tCKPeriod) >= readLatency) readLatency - 1
                          else device.TRCD.tCKCycles(tCKPeriod)

    assert(additiveLatency == 0 || additiveLatency == readLatency - 1 || additiveLatency == readLatency - 2)

    val CLMinus1 = readLatency - 1
    val CLMinus2 = readLatency - 2
    val additiveLatencyString = additiveLatency match {
        case 0 => "Disabled"
        case CLMinus1 => "CL - 1"
        case CLMinus2 => "CL - 2"
    }

    assert(dqBusWidth % device.DQ_BITS == 0)

    val dqParallel = dqBusWidth / device.DQ_BITS
}

case class PS (
    value: Double
) {
    def tCKCycles(tCKPeriod: Double) = Math.ceil(value / (tCKPeriod * 1000.0)).toInt - 1

    def clockCycles(clockPeriod: Double) = Math.ceil(value / (clockPeriod * 1000.0)).toInt - 1
}

case class TCK (
    value: Double
) {
    def tCKCycles(tCKPeriod: Double) = Math.ceil(value).toInt - 1

    def clockCycles(clockMultiplier: Double) = Math.ceil(value / clockMultiplier).toInt - 1
}