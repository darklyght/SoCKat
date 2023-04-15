package sockat.uart

import spinal.core._
import spinal.lib.{Stream, Flow, master, slave}

import sockat.utilities.{Deserializer}

case class Receiver (
    parameters: UARTParameters,
) extends Component {
    val io = new Bundle {
        val data = master Flow(UInt(8 bits))
        val serial = in UInt(1 bits) addTag(crossClockDomain)
    }

    val bitCount = parameters.clockFrequency / parameters.baudRate
    val counterWidth = log2Up(bitCount.toInt)

    val bitCounter = Reg(UInt(counterWidth bits)) init(0)
    val byteCounter = Reg(UInt(4 bits)) init(0)

    val deserializer = Deserializer(
        dataType = UInt(1 bits),
        width = 8,
        outputWidth = 8,
        resetFunction = (register: UInt) => {
            register init(0)
        },
        defaultFunction = (register: UInt) => {
            register := 0
        }
    )
 
    val bit = bitCounter === bitCount - 1
    val sample = bitCounter === bitCount / 2
    val started = byteCounter =/= 0
    val start = ~started && io.serial === 0

    when (start || bit) {
        bitCounter := 0
    } otherwise {
        bitCounter := bitCounter + 1
    }

    when (start) {
        byteCounter := 10
    } elsewhen (bit && started) {
        byteCounter := byteCounter - 1
    }

    deserializer.io.load := sample && started
    deserializer.io.shift := sample && started
    deserializer.io.input := io.serial

    io.data.payload := deserializer.io.output.asBits.asUInt
    io.data.valid := byteCounter === 1 && sample && io.serial === 1
}

object ReceiverV {
    def main(
        args: Array[String]
    ) = {
        val ReceiverV = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/uart/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            ),
        ).generate(
            Receiver(UARTParameters()),
        )
    }
}