package sockat.utilities

import spinal.core._

case class ShiftRegisterParameters[T <: Data] (
    dataType: HardType[T],
    width: Int = 1,
    inputWidth: Int = 1,
    outputWidth: Int = 1,
    step: Int = 1,
    mostSignificantFirst: Boolean = false,
    resetFunction: T => Unit,
    defaultFunction: T => Unit,
) {
    assert(inputWidth <= width)
    assert(outputWidth <= width)
    assert(step <= width)
}

case class ShiftRegister[T <: Data] (
    parameters: ShiftRegisterParameters[T]
) extends Component {
    val io = new Bundle {
        val load = in Bool()
        val shift = in Bool()
        val input = in(Vec(parameters.dataType(), parameters.inputWidth))
        val output = out(Vec(parameters.dataType(), parameters.outputWidth))
    }

    val registers = Vec.fill(parameters.width)(
        Reg(parameters.dataType)
    )
    registers.foreach(parameters.resetFunction)

    registers.zipWithIndex.foreach({case (register, index) => {
        when (io.shift) {
            if (index < parameters.step) {
                parameters.defaultFunction(register)
            } else {
                register := registers(index - parameters.step)
            }
        }

        when (io.load) {
            if (index < parameters.inputWidth) {
                if (parameters.mostSignificantFirst) {
                    register := io.input(index)
                } else {
                    register := io.input(parameters.inputWidth - 1 - index)
                }
            }
        }
    }})

    io.output.zipWithIndex.foreach({case (output, index) => {
        if (parameters.mostSignificantFirst) {
            output := registers(parameters.width - parameters.outputWidth + index)
        } else {
            output := registers(parameters.width - index - 1)
        }
    }})
}