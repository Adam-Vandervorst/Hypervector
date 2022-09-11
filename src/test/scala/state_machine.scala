import munit.FunSuite

import hv.*

class TurnstileExample extends FunSuite:
  // Implementing the Turnstile state machine with Hypervectors
  // The state machine: https://en.wikipedia.org/wiki/Finite-state_machine#Example:_coin-operated_turnstile

  // state
  val locked = HyperVector.random
  val unlocked = HyperVector.random
  // input symbols
  val token = HyperVector.random
  val push = HyperVector.random
  // next state permutation
  val PNext = Permutation.random
  // inverse for querying the next state
  val QNext = PNext.inverse

  val transition = HyperVector.majority(
    (push xor locked xor PNext(locked)),
    (token xor locked xor PNext(unlocked)),
    (push xor unlocked xor PNext(locked)),
    (token xor unlocked xor PNext(unlocked))
  )

  // note this doesn't exactly give the right state
  def next(state: HyperVector, input: HyperVector): HyperVector =
    QNext(transition xor input xor state)

  // so we make a noisy lookup table
  val stateMemory = AutoMemory(locked, unlocked)

  test("stateMemory transitions") {
    assert(stateMemory.query(next(locked, push)) == locked)
    assert(stateMemory.query(next(locked, token)) == unlocked)
    assert(stateMemory.query(next(unlocked, push)) == locked)
    assert(stateMemory.query(next(unlocked, token)) == unlocked)
  }
