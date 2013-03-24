package jp.seraphr.traverse.data

import org.scalatest.FunSuite

class StateTest extends FunSuite {
  test("count with State") {
    val count = (i: Int) => State((s: Int) => (s + 1, i))
    val tResult = count(10).flatMap(count).flatMap(count).runState(0)

    assert(tResult === (3, 10))
  }
}