package sockat.models

import spinal.core._
import spinal.lib._
import spinal.lib.io._

import sockat.ddr3._

case class ddr3 (
    parameters: DDR3Parameters
) extends BlackBox {
    val io = new Bundle {
        val rst_n = in Bool()
        val ck = in Bool()
        val ck_n = in Bool()
        val cke = in Bool()
        val cs_n = in Bool()
        val ras_n = in Bool()
        val cas_n = in Bool()
        val we_n = in Bool()
        val dm_tdqs = inout(Analog(UInt(parameters.dmWidth bits)))
        val ba = in UInt(parameters.baWidth bits)
        val addr = in UInt(parameters.addrWidth bits)
        val dq = inout(Analog(UInt(parameters.dqWidth bits)))
        val dqs = inout(Analog(UInt(parameters.dqsWidth bits)))
        val dqs_n = inout(Analog(UInt(parameters.dqsWidth bits)))
        val tdqs_n = inout(Analog(UInt(parameters.dqsWidth bits)))
        val odt = in Bool()
    }

    addRTLPath("../sim/lib/DDR3_SDRAM_Verilog_Model/ddr3.v")

    noIoPrefix()
}