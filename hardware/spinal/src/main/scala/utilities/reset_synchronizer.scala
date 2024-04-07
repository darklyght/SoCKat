package sockat.utilities

import spinal.core._
import spinal.lib._

case class ResetSynchronizerParameters (
    stages: Int = 3,
    resetPolarity: Polarity = HIGH
)

case class ResetSynchronizer (
    parameters: ResetSynchronizerParameters,
) extends Component {
    val io = new Bundle {
        val clock = in Bool()
        val async = in Bool()
        val sync = out Bool()
    }

    val crossingDomain = ClockDomain(
        clock = io.clock,
        reset = io.async,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = parameters.resetPolarity,
            clockEnableActiveLevel = HIGH
        )
    )

    val crossingArea = new ClockingArea(crossingDomain) {
        val registers = Vec.fill(parameters.stages)(
            Reg(Bool()) init(if (parameters.resetPolarity == HIGH) True else False) addTag(crossClockDomain)
        )

        registers(0) := (if (parameters.resetPolarity == HIGH) False else True)
        (1 until parameters.stages).map(i => {
            registers(i) := registers(i - 1)
        })

        io.sync := registers(parameters.stages - 1)
    }
}