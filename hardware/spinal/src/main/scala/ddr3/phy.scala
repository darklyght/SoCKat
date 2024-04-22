package sockat.ddr3

import spinal.core._
import spinal.lib._

case class PHYInterface (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val deviceReset = Bool()
    val read = ReadPathInterface(parameters)
    val write = WritePathInterface(parameters)

    override def asMaster() = {
        out(deviceReset)
        master(read)
        master(write)
    }
}

case class PHY (
    parameters: DDR3Parameters,
    ckClockDomain: ClockDomain,
    readClockDomain: ClockDomain,
    writeClockDomain: ClockDomain,
    dqsClockDomain: ClockDomain
) extends Component {
    val io = new Bundle {
        val internal = slave(PHYInterface(parameters))
        val device = Device(parameters)
    }

    val controllerClockDomain = ClockDomain.current

    val readClockArea = new ClockingArea(readClockDomain) {
        val readPath = ReadPath(
            parameters = parameters,
            controllerClockDomain = controllerClockDomain
        )
    }

    val writeClockArea = new ClockingArea(writeClockDomain) {
        val writePath = WritePath(
            parameters = parameters, 
            dqsClockDomain = dqsClockDomain
        )
    }

    val ioCells = IOCells(parameters)

    readClockArea.readPath.io.device.dq.zip(ioCells.io.internal.dq).foreach({case (device, internal) => {
        device.read := internal.read
    }})
    readClockArea.readPath.io.device.dm.zip(ioCells.io.internal.dm).foreach({case (device, internal) => {
        device.read := internal.read
    }})
    readClockArea.readPath.io.device.dqs.read := ioCells.io.internal.dqs.read
    readClockArea.readPath.io.internal <> io.internal.read

    ioCells.io.internal.cke := writeClockArea.writePath.io.device.cke
    ioCells.io.internal.command := writeClockArea.writePath.io.device.command
    ioCells.io.internal.odt := writeClockArea.writePath.io.device.odt
    ioCells.io.internal.dq.zip(writeClockArea.writePath.io.device.dq).foreach({case (internal, device) => {
        internal.write := device.write
        internal.writeEnable := device.writeEnable
    }})
    ioCells.io.internal.dm.zip(writeClockArea.writePath.io.device.dm).foreach({case (internal, device) => {
        internal.write := device.write
        internal.writeEnable := device.writeEnable
    }})
    ioCells.io.internal.dqs.write := writeClockArea.writePath.io.device.dqs.write
    ioCells.io.internal.dqs.writeEnable := writeClockArea.writePath.io.device.dqs.writeEnable
    writeClockArea.writePath.io.device.dq.zip(ioCells.io.internal.dq).foreach({case (device, internal) => {
        device.read := internal.read
    }})
    writeClockArea.writePath.io.internal <> io.internal.write

    ioCells.io.internal.ck := ckClockDomain.readClockWire
    ioCells.io.internal.rst_n := ~io.internal.deviceReset
    ioCells.io.device <> io.device
}

object PHYVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/ddr3/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            PHY(
                parameters = DDR3Parameters(),
                ckClockDomain = ClockDomain.external(
                    name = "ckClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = HIGH,
                        clockEnableActiveLevel = HIGH
                    )
                ),
                readClockDomain = ClockDomain.external(
                    name = "readClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = HIGH,
                        clockEnableActiveLevel = HIGH
                    )
                ),
                writeClockDomain = ClockDomain.external(
                    name = "writeClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = HIGH,
                        clockEnableActiveLevel = HIGH
                    )
                ),
                dqsClockDomain = ClockDomain.external(
                    name = "dqsClock",
                    config = ClockDomainConfig(
                        clockEdge = RISING,
                        resetKind = ASYNC,
                        resetActiveLevel = HIGH,
                        clockEnableActiveLevel = HIGH
                    )
                )
            )
        )
    }
}