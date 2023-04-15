package sockat.top

import spinal.core._

import sockat.uart.UARTParameters

case class TopParameters (
    clockFrequency: Int = 100000000,
    uartParameters: UARTParameters = UARTParameters(
        clockFrequency = 117964800,
        baudRate = 115200,
    ),
)