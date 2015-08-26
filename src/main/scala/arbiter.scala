// See LICENSE for license details.

package ChiselComponents
import Chisel._

/** Base class for all stable arbiters
  */
abstract class StableLockingArbiterLike[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends Module {
  require(isPow2(count))
  def grant: Seq[Bool]
  val io = new ArbiterIO(gen, n)
  val locked  = Reg(init=Bool(false))
  val lockIdx = Reg(init=UInt(n-1))
  val chosen = Wire(UInt(width = log2Up(n)))

  io.chosen := chosen
  for ((g, i) <- grant.zipWithIndex)
    io.in(i).ready := Mux(locked, lockIdx === UInt(i), g) && io.out.ready
  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits

  if(count == 1){
    when(io.out.valid) {
      locked := !io.out.ready
      when(!locked) {
        lockIdx := chosen
      }
    }
  } else {
    val cnt = Reg(init=UInt(0, width = log2Up(count)))
    val cnt_next = cnt + UInt(1)
    when(io.out.valid) {
      when(!locked) {
        lockIdx := chosen
        locked := !io.out.ready
      }
      when(io.out.ready) {
        when(needsLock.map(_(io.out.bits)).getOrElse(Bool(true))) {
          cnt := cnt_next
          locked := !(cnt_next === UInt(0))
        }.otherwise{
          locked := Bool(false)
        }
      }
    }
  }
}

/** Stable locking arbiter
  * Once an input is allocated, no higher input can preempt it until it is fired
  */
class StableLockingArbiter[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) 
    extends StableLockingArbiterLike[T](gen, n, count, needsLock) {
  def grant: Seq[Bool] = ArbiterCtrl(io.in.map(_.valid))

  var choose = UInt(n-1)
  for (i <- n-2 to 0 by -1) {
    choose = Mux(io.in(i).valid, UInt(i), choose)
  }
  chosen := Mux(locked, lockIdx, choose)
}

class StableArbiter[T <: Data](gen: T, n: Int) extends StableLockingArbiter[T](gen, n, 1)

/** Stable Round-Robin locking arbiter
  * Once an input is allocated, no higher input can preempt it until it is fired
  */
class StableLockingRRArbiter[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None)
    extends StableLockingArbiterLike[T](gen, n, count, needsLock) {
  lazy val last_grant = Reg(init=UInt(0, log2Up(n)))
  override def grant: Seq[Bool] = {
    val ctrl = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UInt(i) > last_grant) ++ io.in.map(_.valid))
    (0 until n).map(i => ctrl(i) && UInt(i) > last_grant || ctrl(i + n))
  }

  when (io.out.fire()) { last_grant := chosen }

  var choose = UInt(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, UInt(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UInt(i) > last_grant, UInt(i), choose)
  chosen := Mux(locked, lockIdx, choose)
}

class StableRRArbiter[T <: Data](gen:T, n: Int) extends StableLockingRRArbiter[T](gen, n, 1)
