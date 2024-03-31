package sockat.ddr3

import spinal.core._

case class DDR3Parameters (
    baWidth: Int = 3,
    addrWidth: Int = 15,
    dqWidth: Int = 16,
    dmWidth: Int = 2,
    dqsWidth: Int = 2
)