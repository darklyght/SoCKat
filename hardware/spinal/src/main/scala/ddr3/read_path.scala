package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.io._

import sockat.primitives._
import sockat.utilities._

import scala.collection.mutable.Queue
import scala.util.Random

case class ReadPathInterface (
    parameters: DDR3Parameters
) extends Bundle with IMasterSlave {
    val controllerClock = Bool()
    val readToggle = Bool()
    val readData = UInt(parameters.burstLength * parameters.device.DQ_BITS bits)
    val readValidToggle = Bool()

    override def asMaster() = {
        out(controllerClock)
        out(readToggle)
        in(readData)
        in(readValidToggle)
    }
}

case class ReadPath (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val device = master(DeviceInternal(parameters))
        val internal = slave(ReadPathInterface(parameters))
    }

    io.device.rst_n := False
    io.device.ck := False
    io.device.cke := False
    io.device.cs_n := True
    io.device.command.ras_n := True
    io.device.command.cas_n := True
    io.device.command.we_n := True
    io.device.command.ba := 0
    io.device.command.addr := 0
    io.device.odt := False
    io.device.dq.write := 0
    io.device.dq.writeEnable := 0
    io.device.dm.write := 0
    io.device.dm.writeEnable := 0
    io.device.dqs.write := 0
    io.device.dqs.writeEnable := 0

    val dqIddrOut = Vec.fill(parameters.burstLength)(
        Reg(UInt(parameters.device.DQ_BITS bits))
    )
    
    val readToggleSync = Vec.fill(4)(
        Reg(Bool()) addTag(crossClockDomain)
    )

    val readValid = Reg(Bool()) init(False)

    readToggleSync(3) := io.internal.readToggle
    readToggleSync(2) := readToggleSync(3)
    readToggleSync(1) := readToggleSync(2)
    readToggleSync(0) := readToggleSync(2) ^ readToggleSync(1)

    val readValidSerializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = parameters.readLatency + parameters.burstLength / 2 + 3,
            inputWidth = 1,
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    val loadSerializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = parameters.readLatency + parameters.burstLength / 2 + 2,
            inputWidth = parameters.burstLength / 2,
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    val shiftSerializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = Bool(),
            width = parameters.readLatency + parameters.burstLength / 2 + 2,
            inputWidth = parameters.burstLength / 2 - 1,
            resetFunction = (register: Bool) => {
                register init(False)
            },
            defaultFunction = (register: Bool) => {
                register := False
            }
        )
    )

    val readDataRiseDeserializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = UInt(parameters.device.DQ_BITS bits),
            width = parameters.burstLength / 2,
            outputWidth = parameters.burstLength / 2,
            resetFunction = (register: UInt) => {
                register init(0)
            },
            defaultFunction = (register: UInt) => {
                register := 0
            }
        )
    )

    val readDataFallDeserializer = ShiftRegister(
        ShiftRegisterParameters(
            dataType = UInt(parameters.device.DQ_BITS bits),
            width = parameters.burstLength / 2,
            outputWidth = parameters.burstLength / 2,
            resetFunction = (register: UInt) => {
                register init(0)
            },
            defaultFunction = (register: UInt) => {
                register := 0
            }
        )
    )

    val dqIddr = Seq.fill(parameters.device.DQ_BITS) {
        IDDR(
            IDDRParameters(
                ddrClkEdge = "SAME_EDGE_PIPELINED"
            )
        )
    }

    readValidSerializer.io.load := readToggleSync(0)
    readValidSerializer.io.shift := True
    readValidSerializer.io.input := Vec(True)

    loadSerializer.io.load := readToggleSync(0)
    loadSerializer.io.shift := True
    loadSerializer.io.input := Vec(True, parameters.burstLength / 2)

    shiftSerializer.io.load := readToggleSync(0)
    shiftSerializer.io.shift := True
    shiftSerializer.io.input := Vec(True, parameters.burstLength / 2 - 1)

    readDataRiseDeserializer.io.load := loadSerializer.io.output(0)
    readDataRiseDeserializer.io.shift := shiftSerializer.io.output(0)
    readDataRiseDeserializer.io.input := Vec(
        Cat(dqIddr.map((iddr) => {iddr.io.q1})
    ).asUInt)

    readDataFallDeserializer.io.load := loadSerializer.io.output(0)
    readDataFallDeserializer.io.shift := shiftSerializer.io.output(0)
    readDataFallDeserializer.io.input := Vec(
        Cat(dqIddr.map((iddr) => {iddr.io.q2})
    ).asUInt)

    dqIddr.zipWithIndex.foreach({case (iddr, index) => {
        iddr.io.c := ClockDomain.current.readClockWire
        iddr.io.ce := True
        iddr.io.d := io.device.dq(index).read
        iddr.io.r := ClockDomain.current.readResetWire
        iddr.io.s := False
    }})

    readValid := readValidSerializer.io.output(0) ^ readValid

    when (readValid) {
        dqIddrOut := Vec(
            (readDataRiseDeserializer.io.output zip readDataFallDeserializer.io.output).flatMap({case (rise, fall) => {
                Seq(rise, fall)
            }})
        )
    }

    val controllerClockDomain = ClockDomain(
        clock = io.internal.controllerClock,
        reset = ClockDomain.current.readResetWire,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val controllerClockArea = new ClockingArea(controllerClockDomain) {
        val readValidToggle = Vec.fill(4)(
            Reg(Bool()) addTag(crossClockDomain)
        )

        val readData = Vec.fill(3)(
            Reg(UInt(parameters.burstLength * parameters.device.DQ_BITS bits)) addTag(crossClockDomain)
        )

        readValidToggle(3) := readValid
        readValidToggle(2) := readValidToggle(3)
        readValidToggle(1) := readValidToggle(2)
        readValidToggle(0) := readValidToggle(1)

        readData(2) := Cat(dqIddrOut.reverse).asUInt
        readData(1) := readData(2)
        when (readValidToggle(2) ^ readValidToggle(1)) {
            readData(0) := readData(1)
        }
    }

    io.internal.readData := controllerClockArea.readData(0)
    io.internal.readValidToggle := controllerClockArea.readValidToggle(0)
}

object ReadPathVerilog {
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
            ReadPath(DDR3Parameters())
        )
    }
}

object ReadPathSimulation {
    def test(
        dut: ReadPath
    ) = {
        def driver(
            dut: ReadPath,
            driveQueue: Queue[BigInt],
            verifyQueue: Queue[BigInt]
        ) = {
            while (true) {
                val readToggle = dut.io.internal.readToggle.toBoolean
                waitUntil(dut.io.internal.readToggle.toBoolean != readToggle)
                dut.clockDomain.waitRisingEdge(4)
                dut.clockDomain.waitRisingEdge(dut.parameters.readLatency)
                dut.clockDomain.waitFallingEdge()
                sleep(625)

                val buffer = driveQueue(0)
                for (i <- dut.parameters.burstLength / 2 - 1 to 0 by -1) {
                    dut.io.device.dq.read #= (buffer >> ((i * 2 + 1) * dut.parameters.device.DQ_BITS)) & ((1 << dut.parameters.device.DQ_BITS) - 1)
                    dut.clockDomain.waitRisingEdge()
                    sleep(625)
                    dut.io.device.dq.read #= (buffer >> ((i * 2) * dut.parameters.device.DQ_BITS)) & ((1 << dut.parameters.device.DQ_BITS) - 1)
                    dut.clockDomain.waitFallingEdge()
                    sleep(625)
                }

                verifyQueue.enqueue(driveQueue.dequeue())
            }
        }

        def monitor(
            dut: ReadPath,
            controllerClock: ClockDomain,
            queue: Queue[BigInt]
        ) = {
            while (true) {
                controllerClock.waitRisingEdge()

                val readValidToggle = dut.io.internal.readValidToggle.toBoolean

                if (dut.io.internal.readValidToggle.toBoolean != readValidToggle) {
                    assert(dut.io.internal.readData == queue.dequeue())
                }
            }
        }

        def readToggle(
            dut: ReadPath,
            controllerClock: ClockDomain,
            queue: Queue[BigInt]
        ) = {
            while (true) {
                val rand = new scala.util.Random
                var buffer = BigInt(dut.parameters.burstLength * dut.parameters.device.DQ_BITS, rand)

                controllerClock.waitRisingEdge()

                if (queue.isEmpty) {
                    dut.io.internal.readToggle #= !dut.io.internal.readToggle.toBoolean
                    queue.enqueue(buffer)
                }
            }
        }

        val driveQueue = Queue[BigInt]()
        val verifyQueue = Queue[BigInt]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(400000000))
        dut.clockDomain.waitRisingEdge()
        sleep(1875)
        val controllerClock = ClockDomain(dut.io.internal.controllerClock)
        controllerClock.forkStimulus(frequency = HertzNumber(200000000))

        dut.io.device.dq.read #= 0
        dut.io.internal.readToggle #= false

        dut.clockDomain.waitSampling(200)

        val driverThread = fork(driver(dut, driveQueue, verifyQueue))
        val monitorThread = fork(monitor(dut, controllerClock, verifyQueue))
        val readToggleThread = fork(readToggle(dut, controllerClock, driveQueue))

        dut.clockDomain.waitSampling(10000)
    }

    def main(
        args: Array[String]
    ) = {
        val burstLengths = Seq(4, 6, 8)
        val readLatencies = Seq(4, 5, 6 ,7 ,8, 9, 10, 11, 12, 13)

        val tests = for {
            i <- burstLengths
            j <- readLatencies
        } yield (i, j)

        tests.foreach({case (burstLength, readLatency) => {
            println(burstLength, readLatency)

            val compiled = SimConfig.withIVerilog
                                    .withFstWave
                                    .addSimulatorFlag("-D den4096Mb")
                                    .addSimulatorFlag("-D sg187E")
                                    .addSimulatorFlag("-D x16")
                                    .addSimulatorFlag("-g2012")
                                    .addSimulatorFlag("-s glbl")
                                    .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                    .compile(
                                        ReadPath(DDR3Parameters(burstLength = burstLength, readLatency = readLatency))
                                    )
            
            compiled.doSim(dut => test(dut))
        }})
        
    }
}