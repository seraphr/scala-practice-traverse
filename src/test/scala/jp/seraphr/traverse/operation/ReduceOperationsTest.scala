package jp.seraphr.traverse.operation
import org.scalatest.prop.Checkers
import org.scalatest.FunSuite
import org.scalacheck.Arbitrary
import jp.seraphr.traverse.typeclass.TraversableInstances
import jp.seraphr.traverse.typeclass.MonoidInstances
import jp.seraphr.traverse.typeclass.Monoid

class ReduceOperationsTest extends FunSuite with Checkers {
  test("countElements with List") {
    import TraversableInstances.ListTraversable

    def testList[T](implicit ev: Arbitrary[List[T]]): Unit = {
      check { (aList: List[T]) =>
        {
          assert(ReduceOperations.countElements(aList) === aList.size)
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

  test("append Elements with List") {
    import TraversableInstances.ListTraversable
    import MonoidInstances._

    def testList[T](implicit ev: Arbitrary[List[T]], ev2: Monoid[T]): Unit = {
      check { aList: List[T] =>
        {
          val tMonoid = ev2
          assert(ReduceOperations.appendElements(aList) === aList.fold(ev2.zero)(ev2.append(_, _)))
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[List[Int]]
    testList[(String, Int)]
  }

  test("countAndAppendElements with List") {
    import TraversableInstances.ListTraversable
    import MonoidInstances._

    def testList[T](implicit ev: Arbitrary[List[T]], ev2: Monoid[T]): Unit = {
      check { aList: List[T] =>
        {
          val tMonoid = ev2
          val (tCount, tAppend) = ReduceOperations.countAndAppendElements(aList)
          assert(tCount === aList.size)
          assert(tAppend === aList.fold(ev2.zero)(ev2.append(_, _)))
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[List[Int]]
    testList[(String, Int)]
  }
}