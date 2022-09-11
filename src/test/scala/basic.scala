import munit.FunSuite

import hv.*

class BasicOps extends FunSuite:
  test("random different") {
    val x = HyperVector.random
    val y = HyperVector.random
    val z = HyperVector.random
    assert(x unrelated y)
    assert(y unrelated x)
    assert(y unrelated z)
    assert(z unrelated x)
    assert((x jaccard y) > .9)
    assert((x cosine y) < .1)
  }

  test("xor cancel") {
    val x = HyperVector.random
    val y = HyperVector.random
    val xy = x xor y
    assert((xy xor x) == y)
  }

  test("xor differences active") {
    val x = HyperVector.random
    val a = HyperVector.random
    val xa = x xor a
    assert((xa differences x) == a.active)
  }

  test("xor preserves distances") {
    val x = HyperVector.random
    val y = HyperVector.random
    val a = HyperVector.random
    val xa = x xor a
    val ya = y xor a
    assert((x differences y) == (xa differences ya))
  }

//  test("xor unrelated") {
//    // does this even make sense?
//    val x = HyperVector.random
//    val y = HyperVector.random
//    val z = HyperVector.random
//    val xy = x xor y
//    val yz = y xor z
//    val zx = z xor x
//    val xyz = x xor y xor z
//    println(x unrelated xy)
//    println(zx unrelated xy)
//    println(y unrelated xyz)
//    println(yz unrelated xyz)
//  }

  test("majority uneven related") {
    val x = HyperVector.random
    val y = HyperVector.random
    val z = HyperVector.random
    val s = HyperVector.majority(x, y, z)
    assert(x related s)
    assert(y related s)
    assert(z related s)
  }

  test("majority pair related") {
    val x = HyperVector.random
    val y = HyperVector.random
    val s = HyperVector.majority(x, y)
    assert(x related s)
    assert(y related s)
  }

  test("flipped none") {
    val x = HyperVector.random
    assert(HyperVector.zero.flipped == HyperVector.one)
    assert((x differences x.flipped) == hyperVectorSize)
  }

  test("random permutation dissimilar") {
    val P = Permutation.random
    val x = HyperVector.random
    val Px = P(x)
    assert(x unrelated Px)
  }

  test("permutation shift dissimilar") {
    val S1 = Permutation.shift(1)
    val S100 = Permutation.shift(100)
    val x = HyperVector.random
    assert(x unrelated S1(x))
    assert(x unrelated S100(x))
  }

  test("permutation composed compose") {
    val P = Permutation.random
    val Q = Permutation.random
    val x = HyperVector.random
    val PQ = P composed Q
    val compx = PQ(x)
    val acompx = (P.apply compose Q.apply)(x)
    assert(compx == acompx)
  }

  test("permutations composed assoc") {
    val P = Permutation.random
    val Q = Permutation.random
    val R = Permutation.random
    assert(((P composed Q) composed R) == (P composed (Q composed R)))
  }

  test("permutation identity") {
    val P = Permutation.random
    assert(P == (Permutation.ident composed P))
    assert(P == (P composed Permutation.ident))
  }

  test("permutation inverse") {
    val P = Permutation.random
    assert(P.inverse.inverse == P)
    val x = HyperVector.random
    assert(P.inverse(P(x)) == x)
    assert(P(P.inverse(x)) == x)
  }