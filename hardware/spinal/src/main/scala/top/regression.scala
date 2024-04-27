package sockat.top

import sockat.ddr3
import sockat.uart
import sockat.utilities

object Regression {
    def main(
        args: Array[String]
    ) = {
        utilities.FIFOSimulation.main(args)
        utilities.AsyncFIFOSimulation.main(args)
        utilities.ClockGeneratorSimulation.main(args)
        utilities.StreamResizerSimulation.main(args)

        uart.ReceiverSimulation.main(args)
        uart.TransmitterSimulation.main(args)
        uart.UARTSimulation.main(args)

        ddr3.ReadPathSimulation.main(args)
        ddr3.WritePathSimulation.main(args)
        ddr3.IOCellsSimulation.main(args)
        ddr3.ControllerSimulation.main(args)
        ddr3.DDR3Simulation.main(args)
    }
}