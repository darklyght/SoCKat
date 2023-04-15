package sockat.utilities

import spinal.core._

case class Serializer[T <: Data] (
    dataType: HardType[T],
    width: Int,
    inputWidth: Int,
    resetFunction: T => Unit,
    defaultFunction: T => Unit,
) extends Component {
    val io = new Bundle {
        val load = in Bool()
        val shift = in Bool()
        val input = in(Vec(dataType(), inputWidth))
        val output = out(dataType())
    }

    val registers = Vec.fill(width)(
        Reg(dataType)
    )
    registers.foreach(resetFunction)

    registers.zipWithIndex.foreach({case (register, index) => {
        when (io.load) {
            if (index >= width - inputWidth) {
                register := io.input(index - width + inputWidth)
            } else {
                register := registers(index + 1)
            }
        } otherwise {
            when (io.shift) {
                if (index == width - 1) {
                    defaultFunction(register)
                } else {
                    register := registers(index + 1)
                }
            }
        }
    }})

    io.output := registers(0)
}