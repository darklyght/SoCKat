package sockat.ddr3

import spinal.core._

case class DDR3Parameters (
    density: String = "den4096Mb",
    speedGrade: String = "sg187E",
    width: String = "x16",
    rank: String = "SINGLE_RANK"
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
)

case class tCK (
    value: Double
)