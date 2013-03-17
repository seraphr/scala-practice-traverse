package jp.seraphr.traverse.main
import jp.seraphr.traverse.operation.Operations
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.typeclass.TraversableInstances

object Main {

  def main(args: Array[String]): Unit = {
    implicit val tListTraversable = TraversableInstances.ListTraversable
    implicit val tListApplicative = ApplicativeInstances.ListApplicative

    val tList = List(1, 2, 3, 4, 2, "hoge", 1.1, 1, 1)
    val tSize = Operations.countElements(tList)

    println("%s.size is %d and countElements is %d".format(tList, tList.size, tSize))
  }

}