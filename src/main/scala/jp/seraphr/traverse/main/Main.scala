package jp.seraphr.traverse.main
import jp.seraphr.traverse.operation.Operations
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.typeclass.TraversableInstances
import jp.seraphr.traverse.typeclass.MonoidInstances

object Main {

  def main(args: Array[String]): Unit = {
    import TraversableInstances.ListTraversable
    import ApplicativeInstances.ListApplicative
    import MonoidInstances._

    val tList = List(1, 2, 3, 4, 2, "hoge", 1.1, 1, 1)
    val tSize = Operations.countElements(tList)

    println("%s.size is %d and countElements is %d".format(tList, tList.size, tSize))

    val tIntList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val tSum = Operations.sumElements(tIntList)
    println("sum of %s is %d".format(tIntList, tSum))

    val tAppendInt = Operations.appendElements(tIntList)
    println("append of %s is %d".format(tIntList, tAppendInt))

    val tStringList = List("hoge", "_", "fuga", "_", "piyo")
    val tAppendString = Operations.appendElements(tStringList)
    println("append of %s, is %s".format(tStringList, tAppendString))

    val tListList = List(List(1, 2, 3), List(4, "hoge", 6), List(7, 8, 9))
    val tAppendList = Operations.appendElements(tListList)
    println("append of %s, is %s".format(tListList, tAppendList))
  }
}