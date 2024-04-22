package sockat.uart

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import sockat.primitives._
import sockat.utilities._

import scala.collection.mutable.Queue

case class UARTSerial (
) extends Bundle {
    val transmit = out Bool()
    val receive = in Bool()
}

case class UARTData (
) extends Bundle with IMasterSlave {
    val transmit = Stream(UInt(8 bits))
    val receive = Stream(UInt(8 bits))

    override def asMaster() = {
        master(transmit)
        slave(receive)
    }
}

case class UART (
    parameters: UARTParameters,
    uartClockDomain: ClockDomain
) extends Component {
    val io = new Bundle {
        val data = slave(UARTData())
        val serial = UARTSerial()
    }

    val transmitFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = UInt(8 bits),
            addressWidth = 6,
            enqueueClockDomain = ClockDomain.current,
            dequeueClockDomain = uartClockDomain
        )
    )

    val receiveFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = UInt(8 bits),
            addressWidth = 6,
            enqueueClockDomain = uartClockDomain,
            dequeueClockDomain = ClockDomain.current
        )
    )

    val uartClockArea = new ClockingArea(uartClockDomain) {
        val transmitter = Transmitter(parameters)
        val receiver = Receiver(parameters)
    }

    transmitFIFO.io.enqueue <> io.data.transmit
    uartClockArea.transmitter.io.data <> transmitFIFO.io.dequeue
    receiveFIFO.io.enqueue <> uartClockArea.receiver.io.data.toStream
    io.data.receive <> receiveFIFO.io.dequeue

    io.serial.transmit <> uartClockArea.transmitter.io.serial
    io.serial.receive <> uartClockArea.receiver.io.serial
}

case class UARTSimulationModel(
    parameters: UARTParameters
) extends Component {
    val io = new Bundle {
        val data = slave(UARTData())
        val serial = UARTSerial()
    }

    val uartClock = MMCME2_ADV(
        parameters = MMCME2_ADVParameters(
            clkIn1Period = 1e9 / parameters.clockFrequency,
            divClkDivide = 9,
            clkFbOutMultF = 62.375,
            clkOut0DivideF = 5.875
        )
    )

    uartClock.noPhaseShift()
    uartClock.noDynamicReconfiguration()
    uartClock.io.clkIn1 := ClockDomain.current.readClockWire
    uartClock.io.clkIn2 := False
    uartClock.io.clkInSel := 1
    uartClock.io.rst := ClockDomain.current.readResetWire
    uartClock.io.pwrDwn := False
    uartClock.io.clkFbIn := uartClock.io.clkFbOut

    val uartResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    uartResetSynchronizer.io.clock := uartClock.io.clkOut0
    uartResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~uartClock.io.locked

    val uartClockDomain = ClockDomain(
        clock = uartClock.io.clkOut0,
        reset = uartResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val uart = UART(
        parameters = parameters,
        uartClockDomain = uartClockDomain
    )

    io.data <> uart.io.data
    io.serial <> uart.io.serial
}

object UARTVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/uart/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            UART(
                parameters = UARTParameters(),
                uartClockDomain = ClockDomain.external(
                    name = "uartClock",
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

object UARTSimulation {
    def test(
        dut: UARTSimulationModel
    ) = {
        def driver(
            dut: UARTSimulationModel,
            queue: Queue[Int]
        ) = {
            while (true) {
                dut.io.serial.receive #= dut.io.serial.transmit.toBoolean

                dut.io.data.transmit.valid.randomize()
                dut.io.data.transmit.payload.randomize()
                dut.clockDomain.waitSampling()

                if (dut.io.data.transmit.valid.toBoolean && dut.io.data.transmit.ready.toBoolean) {
                    queue.enqueue(dut.io.data.transmit.payload.toInt)
                }
            }
        }

        def monitor(
            dut: UARTSimulationModel,
            queue: Queue[Int]
        ) = {
            while (true) {
                dut.io.data.receive.ready #= true

                dut.clockDomain.waitSampling()

                if (dut.io.data.receive.valid.toBoolean && dut.io.data.receive.ready.toBoolean) {
                    assert(dut.io.data.receive.payload.toInt == queue.dequeue())
                }
            }
        }

        val queue = Queue[Int]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(dut.parameters.clockFrequency))
        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, queue))

        dut.clockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .addSimulatorFlag("-s glbl")
                                .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                .compile(
                                    UARTSimulationModel(
                                        UARTParameters(
                                            baudRate = 7372800
                                        )
                                    )
                                )
        
        compiled.doSim(dut => test(dut))
    }
}