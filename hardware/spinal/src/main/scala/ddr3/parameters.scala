package sockat.ddr3

import spinal.core._

case class DDR3Parameters (
    density: String = "den4096Mb",
    speedGrade: String = "sg187E",
    width: String = "x16",
    rank: String = "SINGLE_RANK",
    tCK: Int = 5,
    burstLength: Int = 8,
    writeLatency: Int = 5,
    readLatency: Int = 6
) {
    val device: DDR3DeviceParameters = DDR3DeviceParameters(
        density,
        speedGrade,
        width,
        rank
    )
}

case class ps (
    value: Double
) {
    def tCKCycles(tCK: Int) = Math.ceil(value / (tCK * 1000.0)).toInt
}

case class tCK (
    value: Double
) {
    def tCKCycles(tCK: Int) = Math.ceil(value).toInt
}