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

  test("count Elements with List of List") {
    import TraversableInstances.ListTraversable
    import TraversableInstances.nestedTraversable

    val tListList = List(List(1, 2), List(3, 4, 5), List(6, 7))

    // こういう型注釈しないと、Traversableの中身がTraversableという場合には対応できない
    // 型注釈なしでは、3が返る。　残念
    assert(Operations.countElements[({type LL[E] = List[List[E]]})#LL, Int](tListList) === 7)
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

  test("sumAndSumElements with List") {
    import TraversableInstances.ListTraversable

    check { (aList: List[Int]) =>
      {
        assert(Operations.sumAndSumElements(aList) === (aList.sum, aList.sum))
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
          val (tCount, tAppend) = Operations.countAndAppendElements(aList)
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

  test("content with List") {
    import TraversableInstances.ListTraversable

    def testList[T](implicit ev: Arbitrary[List[T]]): Unit = {
      check { aList: List[T] =>
        {
          assert(Operations.content(aList) === aList)
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[List[Int]]
    testList[(String, Int)]
  }

  test("map with List") {
    import TraversableInstances.ListTraversable

    def testList[T](implicit ev: Arbitrary[List[T]]): Unit = {
      check { aList: List[T] =>
        {
          assert(Operations.map((_: T).toString)(aList) === aList.map(_.toString()))
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

  test("map with MonoidList") {
    import TraversableInstances.ListTraversable
    import MonoidInstances._

    def testList[T](implicit ev: Arbitrary[List[T]], ev2: Monoid[T]): Unit = {
      check { aList: List[T] =>
        {
          val tMonoid = ev2
          assert(Operations.map((e: T) => tMonoid.append(e, e))(aList) === aList.map((e: T) => tMonoid.append(e, e)))
          true
        }
      }
    }

    testList[String]
    testList[Int]
    testList[(String, Int)]
  }

  test("shape with List") {
    import TraversableInstances.ListTraversable

    def testList[T](implicit ev: Arbitrary[List[T]]): Unit = {
      check { aList: List[T] =>
        {
          assert(Operations.shape(aList) === aList.map(_ => ()))
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
}