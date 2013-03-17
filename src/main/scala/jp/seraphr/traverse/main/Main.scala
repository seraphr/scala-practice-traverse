package jp.seraphr.traverse.main
import jp.seraphr.traverse.operation.Operations
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.typeclass.TraversableInstances

object Main {

  def main(args: Array[String]): Unit = {
    import TraversableInstances.ListTraversable
    import ApplicativeInstances.ListApplicative

    val tList = List(1, 2, 3, 4, 2, "hoge", 1.1, 1, 1)
    val tSize = Operations.countElements(tList)

    println("%s.size is %d and countElements is %d".format(tList, tList.size, tSize))

    val tIntList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val tSum = Operations.sumElements(tIntList)
    println("sum of %s is %d".format(tIntList, tSum))
  }
}