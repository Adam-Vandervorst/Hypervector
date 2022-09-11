package hv

import be.adamv.macroloop.collection.SizedVector
import be.adamv.macroloop.SizedArrayIndex


object Permutations:
  opaque type Permutation = SizedVector[HyperVectorSize, Int]

  object Permutation:
    val ident: Permutation =
      SizedVector.tabulate(identity)

    def random: Permutation =
      SizedVector.wrap(rand.shuffle(ident.data).toArray)

    def shift(k: Int): Permutation =
      SizedVector.tabulate(i => (i + k) % hyperVectorSize)

    extension (p: Permutation)
      def apply(hv: HyperVector): HyperVector =
        HyperVector.fromRaw(SizedVector.tabulate(i => hv(p(i))))

      def inverse: Permutation =
        val inv: Array[Int] = SizedArrayIndex.ofSize[HyperVectorSize, Int]
        var i = 0
        p.forEach { j =>
          inv(j) = i
          i += 1
        }
        SizedVector.wrap(inv)

      def composed(q: Permutation): Permutation =
        SizedVector.tabulate(i => q(p(i)))
export Permutations.*