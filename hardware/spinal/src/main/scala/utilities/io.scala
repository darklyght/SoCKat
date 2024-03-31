package sockat.utilities

import spinal.core._

case class differential[T <: Data] (
    dataType: HardType[T]
) extends Bundle {
    val p = dataType()
    val n = dataType()
}