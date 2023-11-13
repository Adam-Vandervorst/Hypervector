package hv

import be.adamv.macroloop.collection.SizedVector
import be.adamv.macroloop.SizedArrayIndex


object Permutations:
  opaque type Permutation = SizedVector[HyperVectorSize, Int]

  object Permutation:
    val ident: Permutation =
      SizedVector.tabulate(identity)

    def random: Permutation =
      val a = ident.data.clone()
      var i = hyperVectorSize - 1
      while i > 0 do
        val j = rand.nextInt(i + 1)
        val x = a(j)
        a(j) = a(i)
        a(i) = x
        i -= 1
      SizedVector.wrap(a)

    def shift(k: Int): Permutation =
      SizedVector.tabulate(i => (i + k) % hyperVectorSize)

    def bijection(f: Int => Int): Permutation =
      SizedVector.tabulate(f)

    def fromRaw(sv: SizedVector[HyperVectorSize, Int]): Permutation = sv

    extension (p: Permutation)
      inline def raw: SizedVector[HyperVectorSize, Int] = p

      def apply(hv: HyperVector): HyperVector =
        val sv = Array.fill[Int](256)(0)
        var i = 0
        while i < 256 do
          var k = 0
          while k < 32 do
            val target = p(i*32 + k)
            if hv.at(i*32 + k) then
              sv(target/32) |= 1 << (target % 32)
            k += 1
          i += 1
        HyperVectors.HyperVector.fromRaw(SizedVector.from(sv))

      def inverse: Permutation =
        val inv: Array[Int] = SizedArrayIndex.ofSize[HyperVectorSize, Int]
        var i = 0
        p.forEach { j =>
          inv(j) = i
          i += 1
        }
        SizedVector.wrap(inv)

      def composed(q: Permutation): Permutation =
        SizedVector.tabulate(i => p(q(i)))
export Permutations.*
