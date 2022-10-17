package hv

import be.adamv.macroloop.collection.SizedVector
import be.adamv.macroloop.{ConstantTuple, stripCast}
import scala.compiletime.constValue
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.{%, /}


object HyperVectors:
  opaque type HyperVector = SizedVector[HyperVectorSize/32, Int]

  object HyperVector:
    val active_fraction: Float = 1f/8f
    val size: HyperVectorSize = hyperVectorSize
    val average_active: Int = (active_fraction*size).toInt

    val zero: HyperVector = SizedVector.fill(0)
    val one: HyperVector = SizedVector.fill(~0)

    def random: HyperVector =
      SizedVector.tabulate(_ => rand.nextInt() & rand.nextInt() & rand.nextInt())

    def randomHalf: HyperVector =
      SizedVector.tabulate(_ => rand.nextInt())

    def fromRaw(sv: SizedVector[HyperVectorSize/32, Int]): HyperVector = sv

    inline def majority[Tup <: Tuple](inline hvs: Tup): HyperVector =
      inline if constValue[Tuple.Size[Tup] % 2 == 0] then
        val r = random // random should only be computed once
        majority(ConstantTuple.prepend(hvs)(r))
      else
        SizedVector.tabulate{ i =>
          val counts: Array[Int] = Array.fill(32)(0)
          ConstantTuple.forEachBoundedUnrolled(hvs)((hv: HyperVectors.HyperVector) =>
            val hvi: Int = hv.raw(i)
            var k = 0
            while k < 32 do
              if ((hvi >>> k) & 1) != 0 then counts(k) += 1
              k += 1
          )

          var res = 0
          var k = 0
          while k < 32 do
            if counts(k) > constValue[Tuple.Size[Tup]/2] then
              res |= 1 << k
            k += 1
          res
        }

    extension (xs: HyperVector)
      def raw: SizedVector[HyperVectorSize/32, Int] = xs

      inline def at(i: Int): Boolean = ((xs(i/32) >> (i % 32)) & 1) == 1

      infix def xor(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ ^ _)

      infix def or(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ | _)

      infix def and(ys: HyperVector): HyperVector =
        xs.elementwise(ys, _ & _)

      infix def differences(ys: HyperVector): Int =
        xs.inner(ys, (x, y) => Integer.bitCount(x ^ y), _ + _, 0)

      inline def related(ys: HyperVector): Boolean =
        math.abs((xs differences ys).toFloat/average_active - 1f) < .25

      inline def unrelated(ys: HyperVector): Boolean =
        !(xs related ys)

      infix def jaccard(ys: HyperVector): Float =
        1f - (xs and ys).active.toFloat/(xs or ys).active.toFloat

      infix def cosine(ys: HyperVector): Float =
        (xs and ys).active.toFloat/(xs.active + ys.active).toFloat

      def flipped: HyperVector =
        xs.map(~ _)

      def active: Int =
        xs.data.map(Integer.bitCount).sum
export HyperVectors.*
