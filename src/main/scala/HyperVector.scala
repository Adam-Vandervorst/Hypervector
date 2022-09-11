package hv

import be.adamv.macroloop.collection.SizedVector
import be.adamv.macroloop.{ConstantTuple, stripCast}
import scala.compiletime.constValue
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.%


object HyperVectors:
  opaque type HyperVector = SizedVector[HyperVectorSize, Boolean]

  object HyperVector:
    val active_fraction: Float = 1f/8f
    val size: HyperVectorSize = hyperVectorSize
    val average_active: Int = (active_fraction*size).toInt

    val zero: HyperVector = SizedVector.fill(false)
    val one: HyperVector = SizedVector.fill(true)

    def random: HyperVector =
      SizedVector.tabulate(_ => math.abs(rand.nextInt()) < active_fraction*Int.MaxValue)

    def randomHalf: HyperVector =
      SizedVector.tabulate(_ => rand.nextBoolean())

    def fromRaw(sv: SizedVector[HyperVectorSize, Boolean]): HyperVector = sv

    inline def majority[Tup <: Tuple](inline hvs: Tup): HyperVector =
      inline if constValue[Tuple.Size[Tup] % 2 == 0] then
        majority(ConstantTuple.prepend(hvs)(random))
      else
        SizedVector.tabulate{ i =>
          var p = 0
          var n = 0
          ConstantTuple.forEachBoundedUnrolled(hvs)((hv: HyperVectors.HyperVector) =>
            if hv(i) then p += 1
            else n += 1
          )
          p > n
        }

    extension (xs: HyperVector)
      def raw: SizedVector[HyperVectorSize, Boolean] = xs

      inline def apply(i: Int): Boolean = xs(i)

      infix def xor(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ ^ _)

      infix def or(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ | _)

      infix def and(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ & _)

      infix def differences(ys: HyperVector): Int =
        xs.inner(ys, (x, y) => if x != y then 1 else 0, _ + _, 0)

      inline def related(ys: HyperVector): Boolean =
        math.abs((xs differences ys).toFloat/average_active - 1f) < .25

      inline def unrelated(ys: HyperVector): Boolean =
        !(xs related ys)

      infix def jaccard(ys: HyperVector): Float =
        1f - (xs and ys).active.toFloat/(xs or ys).active.toFloat

      infix def cosine(ys: HyperVector): Float =
        (xs and ys).active.toFloat/(xs.active + ys.active).toFloat

      def flipped: HyperVector =
        xs.map(!_)

      def active: Int =
        xs.data.count(identity)
export HyperVectors.*
