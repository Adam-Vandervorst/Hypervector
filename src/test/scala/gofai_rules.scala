import munit.FunSuite

import be.adamv.macroloop.ConstantTuple
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

  // our rule, read `xor` as "implied by" and `HyperVector.majority` as "and"
  // note this is applied to multiple "datapoints" ...
  val (rule1: HyperVector, rule2: HyperVector, rule3: HyperVector) = ConstantTuple.fillUnrolled(3){
    val person_x = HyperVector.random
    val person_y = HyperVector.random
    val person_z = HyperVector.random

    val mxy = apply_rel(mother_of)(person_x, person_y)
    val fyz = apply_rel(father_of)(person_y, person_z)
    val gxz = apply_rel(grandmother_of)(person_x, person_z)

    gxz xor HyperVector.majority(mxy, fyz)
  }
  // ... and averaged out for higher accuracy
  val grandmother_rule = HyperVector.majority(rule1, rule2, rule3)

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