package sockat.utilities

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.Queue
import scala.util.Random

case class StreamResizerParameters[T <: Data] (
    inputDataType: HardType[T],
    outputDataType: HardType[T],
    lsbFirst: Boolean = true
)

case class StreamResizer[T <: Data] (
    parameters: StreamResizerParameters[T]
) extends Component {
    val io = new Bundle {
        val input = slave(Stream(parameters.inputDataType().asBits))
        val output = master(Stream(parameters.outputDataType().asBits))
    }
    
    val inputWidth = io.input.payload.getWidth
    val outputWidth = io.output.payload.getWidth

    if (inputWidth == outputWidth) {
        io.output <> io.input
    } else if (inputWidth > outputWidth) {
        val multiple = Math.ceil(inputWidth.toDouble / outputWidth).toInt
        val fifo = FIFO(FIFOParameters(
            dataType = parameters.inputDataType().asBits,
            addressWidth = 0
        ))

        val counter = Reg(UInt(log2Up(multiple) bits)) init(0)

        val outputs = if (parameters.lsbFirst) fifo.io.dequeue.payload.resize(multiple * outputWidth).subdivideIn(outputWidth bits) else fifo.io.dequeue.payload.resizeLeft(multiple * outputWidth).subdivideIn(outputWidth bits).reverse
        
        when (io.output.fire) {
            when (counter === multiple - 1) {
                counter := 0
            } .otherwise {
                counter := counter + 1
            }
        }

        fifo.io.enqueue <> io.input
        io.output.valid := fifo.io.dequeue.valid
        io.output.payload := outputs(counter)
        fifo.io.dequeue.ready := io.output.ready && counter === (multiple - 1)
    } else {
        val multiple = Math.ceil(outputWidth.toDouble / inputWidth).toInt

        val register = Vec.fill(multiple)(
            Reg(Bits(inputWidth bits))
        )

        val output = (0 until register.size - 1).map(i => {
            register(i)
        })

        val counter = Reg(UInt(log2Up(multiple) bits)) init(0)

        when (io.input.fire) {
            when (counter === multiple - 1) {
                counter := 0
            } .otherwise {
                register(counter) := io.input.payload
                counter := counter + 1
            }
        }

        io.input.ready := counter < (multiple - 1) || (counter === multiple - 1 && io.output.ready)
        io.output.valid := counter === multiple - 1 && io.input.valid
        io.output.payload := (if (parameters.lsbFirst) Cat(io.input.payload, output.reverse.reduce(_ ## _)).resize(outputWidth) else Cat(output.reduce(_ ## _), io.input.payload).resizeLeft(outputWidth))
    }
}

object StreamResizerVerilog {
    def main(
        args: Array[String]
    ) = {
        val compiled = SpinalConfig(
            mode = Verilog,
            targetDirectory = "../src/hdl/utilities/",
            dumpWave = DumpWaveConfig(
                depth = 0,
                vcdPath = "wave.fst"
            )
        ).generate(
            StreamResizer(StreamResizerParameters(
                inputDataType = UInt(8 bits),
                outputDataType = UInt(13 bits)
            ))
        )
    }
}

object StreamResizerSimulation {
    def test(
        dut: StreamResizer[UInt]
    ) = {
        def driver(
            dut: StreamResizer[UInt],
            queue: Queue[BigInt]
        ) = {
            val rand = new scala.util.Random
            val inputWidth = dut.io.input.payload.getWidth
            val outputWidth = dut.io.output.payload.getWidth

            while (true) {
                dut.io.input.valid.randomize()
                dut.io.input.payload.randomize()
                dut.clockDomain.waitSampling()

                if (dut.io.input.valid.toBoolean && dut.io.input.ready.toBoolean) {
                    if (inputWidth > outputWidth) {
                        val extraBits = outputWidth - (inputWidth % outputWidth)
                        (0 until Math.ceil(inputWidth.toDouble / outputWidth).toInt).foreach(i => {
                            queue.enqueue(
                                if (dut.parameters.lsbFirst) {
                                    (dut.io.input.payload.toBigInt >> (i * outputWidth)) & ((BigInt(1) << outputWidth) - 1)
                                } else {
                                    val data = dut.io.input.payload.toBigInt << extraBits
                                    (data >> ((Math.ceil(inputWidth.toDouble / outputWidth).toInt - 1 - i) * outputWidth)) & ((BigInt(1) << outputWidth) - 1)
                                }
                            )
                        })
                    } else {
                        queue.enqueue(dut.io.input.payload.toBigInt)
                    }
                }
            }
        }

        def monitor(
            dut: StreamResizer[UInt],
            queue: Queue[BigInt]
        ) = {
            val inputWidth = dut.io.input.payload.getWidth
            val outputWidth = dut.io.output.payload.getWidth

            while (true) {
                dut.io.output.ready.randomize()
                dut.clockDomain.waitSampling()

                if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean) {
                    if (outputWidth > inputWidth) {
                        val extraBits = inputWidth - (outputWidth % inputWidth)
                        (0 until Math.ceil(outputWidth.toDouble / inputWidth).toInt).foreach(i => {
                            val output = if (dut.parameters.lsbFirst) {
                                (dut.io.output.payload.toBigInt >> (i * inputWidth)) & ((BigInt(1) << inputWidth) - 1)
                            } else {
                                val data = dut.io.output.payload.toBigInt << extraBits
                                (data >> ((Math.ceil(outputWidth.toDouble / inputWidth).toInt - 1 - i) * inputWidth)) & ((BigInt(1) << inputWidth) - 1)
                            }
                            val input = if (dut.parameters.lsbFirst) {
                                queue.dequeue() & ((BigInt(1) << (outputWidth - (i * inputWidth))) - 1)
                            } else {
                                if (i == Math.ceil(outputWidth.toDouble / inputWidth).toInt - 1) {
                                    queue.dequeue() >> extraBits << extraBits
                                } else {
                                    queue.dequeue()
                                }
                            }
                            assert(input == output)
                        })
                    } else {
                        assert(queue.dequeue() == dut.io.output.payload.toBigInt)
                    }
                }
            }
        }

        val queue = Queue[BigInt]()

        dut.clockDomain.forkStimulus(frequency = HertzNumber(100000000))
        dut.io.input.valid #= false
        dut.io.output.ready #= false

        val driverThread = fork(driver(dut, queue))
        val monitorThread = fork(monitor(dut, queue))

        dut.clockDomain.waitSampling(20000)
    }

    def main(
        args: Array[String]
    ) = {
        val inputDataWidths = Seq(4, 7, 8)
        val outputDataWidths = Seq(5, 7, 17)
        val lsbFirsts = Seq(true, false)

        val tests = for {
            i <- inputDataWidths
            j <- outputDataWidths
            k <- lsbFirsts
        } yield (i, j, k)

        tests.foreach({case (inputDataWidth, outputDataWidth, lsbFirst) => {
            println(inputDataWidth, outputDataWidth, lsbFirst)

            val compiled = SimConfig.withIVerilog
                                    .withFstWave
                                    .compile(
                                        StreamResizer(StreamResizerParameters(
                                            inputDataType = UInt(inputDataWidth bits),
                                            outputDataType = UInt(outputDataWidth bits),
                                            lsbFirst = lsbFirst
                                        ))
                                    )

            compiled.doSim(dut => test(dut))
        }})
    }
}