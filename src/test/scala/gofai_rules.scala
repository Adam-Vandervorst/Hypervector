import munit.FunSuite

import hv.*

class GrandmotherExample extends FunSuite:
  // Let's try and encode some rules, and do some rule-based computing
  // If x is the mother of y and y is the father of z then x is the grandmother of z

  // relation utility
  val rel_subject = Permutation.shift(1)
  val rel_object = rel_subject.inverse

  // relations
  val mother_of = HyperVector.random
  val father_of = HyperVector.random
  val grandmother_of = HyperVector.random

  // extractors
  val mother_of_mother = rel_subject(mother_of)
  val mother_of_child = rel_object(mother_of)
  val father_of_father = rel_subject(father_of)
  val father_of_child = rel_object(father_of)
  val grandmother_of_grandmother = rel_subject(grandmother_of)
  val grandmother_of_child = rel_object(grandmother_of)

  def apply_rel(rel: HyperVector)(x: HyperVector, y: HyperVector): HyperVector =
    val sx = rel_subject(rel) xor x
    val sy = rel_object(rel) xor y
    HyperVector.majority(sx, sy)

  // symbols
  val person_x1 = HyperVector.random
  val person_y1 = HyperVector.random
  val person_z1 = HyperVector.random

  val person_x2 = HyperVector.random
  val person_y2 = HyperVector.random
  val person_z2 = HyperVector.random

  val person_x3 = HyperVector.random
  val person_y3 = HyperVector.random
  val person_z3 = HyperVector.random

  // our rule, read `xor` as "implied by" and `HyperVector.majority` as "and"
  // note this is applied to multiple "datapoints" ...
  val mxy1 = apply_rel(mother_of)(person_x1, person_y1)
  val fyz1 = apply_rel(father_of)(person_y1, person_z1)
  val gxz1 = apply_rel(grandmother_of)(person_x1, person_z1)
  val grandmother_rule1 = gxz1 xor HyperVector.majority(mxy1, fyz1)

  val mxy2 = apply_rel(mother_of)(person_x2, person_y2)
  val fyz2 = apply_rel(father_of)(person_y2, person_z2)
  val gxz2 = apply_rel(grandmother_of)(person_x2, person_z2)
  val grandmother_rule2 = gxz2 xor HyperVector.majority(mxy2, fyz2)

  val mxy3 = apply_rel(mother_of)(person_x2, person_y2)
  val fyz3 = apply_rel(father_of)(person_y2, person_z2)
  val gxz3 = apply_rel(grandmother_of)(person_x2, person_z2)
  val grandmother_rule3 = gxz3 xor HyperVector.majority(mxy3, fyz3)
  // ... and averaged out for higher accuracy
  val grandmother_rule = HyperVector.majority(grandmother_rule1, grandmother_rule2, grandmother_rule3)

  test("apply grandmother_rule") {
    val anna = HyperVector.random
    val bill = HyperVector.random
    val cid = HyperVector.random
    val anna_mother_of_bill = apply_rel(mother_of)(anna, bill)
    val bill_father_of_cid = apply_rel(father_of)(bill, cid)
    val calculated_anna_grandmother_of_cid = grandmother_rule xor HyperVector.majority(anna_mother_of_bill, bill_father_of_cid)
    val actual_anna_grandmother_of_cid = apply_rel(grandmother_of)(anna, cid)

    assert(calculated_anna_grandmother_of_cid related actual_anna_grandmother_of_cid)
  }