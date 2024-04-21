package sockat.ddr3

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._

import sockat.models
import sockat.primitives._
import sockat.utilities._

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.util.Random

case class DDR3 (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val device = Device(parameters)
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength),
                dataWidth = parameters.dqParallel * parameters.burstLength * parameters.device.DQ_BITS,
                idWidth = 8
            )
        ))
    }

    val clockGenerator = MMCME2_ADV(MMCME2_ADVParameters(
        clkIn1Period = (parameters.controllerClockRatio * parameters.ckClockRatio * parameters.tCKPeriod).toInt,
        clkFbOutMultF = 8,
        clkOut0DivideF = 4,
        divClkDivide = 1
    ))

    clockGenerator.noPhaseShift()
    clockGenerator.noDynamicReconfiguration()
    clockGenerator.io.clkIn1 := ClockDomain.current.readClockWire
    clockGenerator.io.clkIn2 := False
    clockGenerator.io.clkInSel := 1
    clockGenerator.io.clkFbIn := clockGenerator.io.clkFbOut
    clockGenerator.io.rst := ClockDomain.current.readResetWire
    clockGenerator.io.pwrDwn := False

    val controllerResetSynchronizer = ResetSynchronizer(ResetSynchronizerParameters())
    controllerResetSynchronizer.io.clock := clockGenerator.io.clkOut0
    controllerResetSynchronizer.io.async := ClockDomain.current.readResetWire || ~clockGenerator.io.locked

    val systemClockDomain = ClockDomain.current

    val controllerClockDomain = ClockDomain(
        clock = clockGenerator.io.clkOut0,
        reset = controllerResetSynchronizer.io.sync,
        clockEnable = True,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = HIGH,
            clockEnableActiveLevel = HIGH
        )
    )

    val controllerClockArea = new ClockingArea(controllerClockDomain) {
        val controller = Controller(parameters)
        val phy = PHY(parameters)
    }

    val arFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = Axi4Ar(io.axiSlave.config),
            addressWidth = 2,
            enqueueClockDomain = systemClockDomain,
            dequeueClockDomain = controllerClockDomain
        )
    )

    val rFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = Axi4R(io.axiSlave.config),
            addressWidth = 2,
            enqueueClockDomain = controllerClockDomain,
            dequeueClockDomain = systemClockDomain
        )
    )

    val awFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = Axi4Aw(io.axiSlave.config),
            addressWidth = 2,
            enqueueClockDomain = systemClockDomain,
            dequeueClockDomain = controllerClockDomain
        )
    )

    val wFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = Axi4W(io.axiSlave.config),
            addressWidth = 2,
            enqueueClockDomain = systemClockDomain,
            dequeueClockDomain = controllerClockDomain
        )
    )

    val bFIFO = AsyncFIFO(
        AsyncFIFOParameters(
            dataType = Axi4B(io.axiSlave.config),
            addressWidth = 2,
            enqueueClockDomain = controllerClockDomain,
            dequeueClockDomain = systemClockDomain
        )
    )

    arFIFO.io.enqueue <> io.axiSlave.ar
    controllerClockArea.controller.io.axiSlave.ar <> arFIFO.io.dequeue

    rFIFO.io.enqueue <> controllerClockArea.controller.io.axiSlave.r
    io.axiSlave.r <> rFIFO.io.dequeue

    awFIFO.io.enqueue <> io.axiSlave.aw
    controllerClockArea.controller.io.axiSlave.aw <> awFIFO.io.dequeue

    wFIFO.io.enqueue <> io.axiSlave.w
    controllerClockArea.controller.io.axiSlave.w <> wFIFO.io.dequeue

    bFIFO.io.enqueue <> controllerClockArea.controller.io.axiSlave.b
    io.axiSlave.b <> bFIFO.io.dequeue

    controllerClockArea.phy.io.internal <> controllerClockArea.controller.io.phy
    
    io.device <> controllerClockArea.phy.io.device
}

case class DDR3DDR3 (
    parameters: DDR3Parameters
) extends Component {
    val io = new Bundle {
        val axiSlave = slave(Axi4(
            Axi4Config(
                addressWidth = log2Up(parameters.device.RANKS) + parameters.device.BA_BITS + parameters.device.ROW_BITS + parameters.device.COL_BITS - log2Up(parameters.burstLength),
                dataWidth = parameters.dqParallel * parameters.burstLength * parameters.device.DQ_BITS,
                idWidth = 8
            )
        ))
    }

    val model = Seq.fill(parameters.device.RANKS) {
        Seq.fill(parameters.dqParallel) {
            models.ddr3(parameters)
        }
    }
    val ddr3 = DDR3(parameters)

    ddr3.io.axiSlave <> io.axiSlave

    val ranks = 0 until parameters.device.RANKS
    val chips = 0 until parameters.dqParallel

    (0 until parameters.device.RANKS).foreach(i => {
        (0 until parameters.dqParallel).foreach(j => {
            model(i)(j).io.rst_n := ddr3.io.device.rst_n
            model(i)(j).io.ck := ddr3.io.device.ck.p
            model(i)(j).io.ck_n := ddr3.io.device.ck.n
            model(i)(j).io.cke := ddr3.io.device.cke
            model(i)(j).io.cs_n := ddr3.io.device.command.cs_n(i)
            model(i)(j).io.ras_n := ddr3.io.device.command.ras_n
            model(i)(j).io.cas_n := ddr3.io.device.command.cas_n
            model(i)(j).io.we_n := ddr3.io.device.command.we_n
            model(i)(j).io.dm_tdqs := ddr3.io.device.dm(j)
            model(i)(j).io.ba := ddr3.io.device.command.ba
            model(i)(j).io.addr := ddr3.io.device.command.addr
            model(i)(j).io.dq := ddr3.io.device.dq(j)
            model(i)(j).io.dqs := ddr3.io.device.dqs.p
            model(i)(j).io.dqs_n := ddr3.io.device.dqs.n
            model(i)(j).io.odt := ddr3.io.device.odt
        })
    })
}

object DDR3Verilog {
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
            DDR3(DDR3Parameters())
        )
    }
}

object DDR3Simulation {
    def test(
        dut: DDR3DDR3
    ) = {
        def read(
            dut: DDR3DDR3,
            address: Queue[BigInt],
            length: Queue[BigInt],
            readDataAddress: Queue[BigInt]
        ) = {
            while (true) {
                if (!address.isEmpty && !length.isEmpty && dut.io.axiSlave.ar.ready.toBoolean) {
                    (0 until length(0).toInt).foreach(i => {
                        readDataAddress.enqueue(address(0) + i)
                    })

                    dut.io.axiSlave.ar.valid #= true
                    dut.io.axiSlave.ar.payload.addr #= address.dequeue()
                    dut.io.axiSlave.ar.payload.len #= length.dequeue() - 1
                }

                dut.clockDomain.waitFallingEdge()

                dut.io.axiSlave.ar.valid #= false
                dut.io.axiSlave.ar.payload.addr #= 0
                dut.io.axiSlave.ar.payload.len #= 0
            }
        }

        def readData(
            dut: DDR3DDR3,
            readDataAddress: Queue[BigInt],
            model: Map[BigInt, BigInt]
        ) = {
            while (true) {
                if (!readDataAddress.isEmpty && dut.io.axiSlave.r.valid.toBoolean) {
                    println(readDataAddress)
                    if (model.contains(readDataAddress(0))) {
                        assert(
                            model.getOrElse(readDataAddress(0), BigInt(0)) == dut.io.axiSlave.r.payload.data.toBigInt,
                            f"${readDataAddress(0) * dut.parameters.burstLength}%x: ${model.getOrElse(readDataAddress(0), BigInt(0))}%X, ${dut.io.axiSlave.r.payload.data.toBigInt}%X"
                        )
                    }
                    readDataAddress.dequeue()
                }

                dut.clockDomain.waitFallingEdge()
            }
        }

        def write(
            dut: DDR3DDR3,
            address: Queue[BigInt],
            data: Queue[Seq[BigInt]],
            writeAddressData: Queue[Queue[(BigInt, BigInt)]]
        ) = {
            while(true) {
                if (!address.isEmpty && !data.isEmpty && dut.io.axiSlave.aw.ready.toBoolean) {
                    var current = address.dequeue()
                    var modelQueue = Queue[(BigInt, BigInt)]()

                    dut.io.axiSlave.aw.valid #= true
                    dut.io.axiSlave.aw.payload.addr #= current
                    dut.io.axiSlave.aw.payload.len #= data(0).length - 1

                    val items = data.dequeue()

                    items.zipWithIndex.foreach({case (item, index) => {
                        var count = 0
                        while (!dut.io.axiSlave.w.ready.toBoolean) {
                            dut.clockDomain.waitFallingEdge()

                            if (index == 0 && count == 0) {
                                dut.io.axiSlave.aw.valid #= false
                                dut.io.axiSlave.aw.payload.addr #= 0
                                dut.io.axiSlave.aw.payload.len #= 0
                            }

                            count = count + 1
                        }

                        dut.io.axiSlave.w.valid #= true
                        dut.io.axiSlave.w.payload.data #= item
                        dut.io.axiSlave.w.payload.strb #= 0
                        dut.io.axiSlave.w.payload.last #= index == (items.length - 1)

                        modelQueue.enqueue((current, item))
                        current = current + 1

                        dut.clockDomain.waitFallingEdge()

                        if (index == 0 && count == 0) {
                            dut.io.axiSlave.aw.valid #= false
                            dut.io.axiSlave.aw.payload.addr #= 0
                            dut.io.axiSlave.aw.payload.len #= 0
                        }

                        if (index == items.length - 1) {
                            dut.io.axiSlave.w.valid #= false
                            dut.io.axiSlave.w.payload.data #= 0
                            dut.io.axiSlave.w.payload.strb #= 0
                            dut.io.axiSlave.w.payload.last #= false

                            writeAddressData.enqueue(modelQueue)
                        }
                    }})
                } else {
                    dut.clockDomain.waitFallingEdge()
                }
            }
        }

        def writeResponse(
            dut: DDR3DDR3,
            writeAddressData: Queue[Queue[(BigInt, BigInt)]],
            model: Map[BigInt, BigInt]
        ) = {
            while (true) {
                if (!writeAddressData.isEmpty && dut.io.axiSlave.b.valid.toBoolean) {
                    val data = writeAddressData.dequeue()

                    data.foreach({case (address, value) => {
                        printf("%x %x\n", address, value)
                        model.remove(address)
                        model.put(address, value)
                    }})
                }

                dut.clockDomain.waitFallingEdge()
            }
        }

        dut.clockDomain.forkStimulus(frequency = HertzNumber(100000000))

        val readAddress = Queue[BigInt]()
        val readLength = Queue[BigInt]()
        val readDataAddress = Queue[BigInt]()

        val writeAddress = Queue[BigInt]()
        val writeData = Queue[Seq[BigInt]]()
        val writeAddressData = Queue[Queue[(BigInt, BigInt)]]()
        val model = Map[BigInt, BigInt]()

        dut.io.axiSlave.aw.payload.id #= 0
        dut.io.axiSlave.b.ready #= true
        dut.io.axiSlave.ar.payload.id #= 0
        dut.io.axiSlave.r.ready #= true

        val writeThread = fork(write(dut, writeAddress, writeData, writeAddressData))
        val writeResponseThread = fork(writeResponse(dut, writeAddressData, model))
        val readAddressThread = fork(read(dut, readAddress, readLength, readDataAddress))
        val readDataThread = fork(readData(dut, readDataAddress, model))

        val rand = new scala.util.Random

        dut.clockDomain.waitSampling()

        (0 until 1000).foreach(i => {
            val address = BigInt(10, rand)
            val length = BigInt(8, rand) + 1

            val items = Seq.fill(length.toInt)(BigInt(dut.parameters.dqParallel * dut.parameters.burstLength * dut.parameters.device.DQ_BITS, rand))

            writeAddress.enqueue(address)
            writeData.enqueue(items)
        })

        // waitUntil(dut.io.axiSlave.b.valid.toBoolean)

        (0 until 1000).foreach(i => {
            val address = BigInt(10, rand)
            val length = BigInt(8, rand) + 1

            readAddress.enqueue(address)
            readLength.enqueue(length)
        })

        dut.clockDomain.waitSampling(200000)
    }

    def main(
        args: Array[String]
    ) = {
        val compiled = SimConfig.withIVerilog
                                .withFstWave
                                .addSimulatorFlag("-D den4096Mb")
                                .addSimulatorFlag("-D sg187E")
                                .addSimulatorFlag("-D x16")
                                .addSimulatorFlag("-g2012")
                                .addSimulatorFlag("-s glbl")
                                .addIncludeDir("../sim/lib/DDR3_SDRAM_Verilog_Model")
                                .compile(
                                    DDR3DDR3(DDR3Parameters(
                                        synthesis = false
                                    ))
                                )

        compiled.doSim(dut => test(dut))
    }
}