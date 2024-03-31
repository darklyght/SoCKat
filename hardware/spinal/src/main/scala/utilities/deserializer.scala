package sockat.utilities

import spinal.core._

case class Deserializer[T <: Data] (
    dataType: HardType[T],
    width: Int,
    outputWidth: Int,
    resetFunction: T => Unit,
    defaultFunction: T => Unit
) extends Component {
    val io = new Bundle {
        val load = in Bool()
        val shift = in Bool()
        val input = in(dataType())
        val output = out(Vec(dataType(), outputWidth))
    }

    val registers = Vec.fill(width)(
        Reg(dataType)
    )
    registers.foreach(resetFunction)

    registers.zipWithIndex.foreach({case (register, index) => {
        if (index == 0) {
            when (io.load) {
                register := io.input
            } otherwise {
                when (io.shift) {
                    defaultFunction(register)
                }
            }
        } else {
            when (io.shift) {
                register := registers(index - 1)
            }
        }
    }})

    io.output.zipWithIndex.foreach({case (output, index) => {
        output := registers(width - index - 1)
    }})
}