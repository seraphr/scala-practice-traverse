package jp.seraphr.traverse.operation
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.matchers.ShouldMatchers
import jp.seraphr.traverse.typeclass.TraversableInstances
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import org.scalacheck.Prop
import jp.seraphr.traverse.typeclass.MonoidInstances
import jp.seraphr.traverse.typeclass.Monoid

class OperationsTest extends FunSuite with Checkers with ShouldMatchers {
  test("countElements with List") {
    import TraversableInstances.ListTraversable

    def testList[T](implicit ev: Arbitrary[List[T]]): Unit = {
      check { (aList: List[T]) =>
        {
          assert(Operations.countElements(aList) === aList.size)
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[Double]
    testList[Char]
    testList[(String, Int)]
  }

  test("sumElements with List") {
    import TraversableInstances.ListTraversable

    check { (aList: List[Int]) =>
      {
        assert(Operations.sumElements(aList) === aList.sum)
        true
      }
    }
  }

  test("append Elements with List") {
    import TraversableInstances.ListTraversable
    import MonoidInstances._

    def testList[T](implicit ev: Arbitrary[List[T]], ev2: Monoid[T]): Unit = {
      check { aList: List[T] =>
        {
          val tMonoid = ev2
          assert(Operations.appendElements(aList) === aList.fold(ev2.zero)(ev2.append(_, _)))
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[(String, Int)]
  }
}