package jp.seraphr.traverse.data

import org.scalatest.FunSuite

class StateTest extends FunSuite {
  test("count with State") {
    val count = (i: Int) => State((s: Int) => (s + 1, i))
    val tAdd3 = count(10).flatMap(count).flatMap(count);
    val tResult = tAdd3.runState(0)

    assert(tResult === (3, 10))

    val tAdd5 = for {
      i <- tAdd3
      _ <- count(i)
      _ <- count(i)
    } yield 3

    assert(tAdd5.runState(0) === (5, 3))
  }
}