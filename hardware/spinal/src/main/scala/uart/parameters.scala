package sockat.uart

import spinal.core._

case class UARTParameters (
    clockFrequency: Int = 117964800,
    baudRate: Int = 115200,
)