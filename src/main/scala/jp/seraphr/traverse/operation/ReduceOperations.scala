package jp.seraphr.traverse.operation
import jp.seraphr.traverse.typeclass.Monoid
import jp.seraphr.traverse.typeclass.CanTraverse
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.data.Container
import jp.seraphr.traverse.typeclass.MonoidInstances
import jp.seraphr.traverse.data.Product
import jp.seraphr.traverse.typeclass.Applicative

/**
 * reduceによって、Operationsに定義されているいくつかの処理を再実装
 */
object ReduceOperations {
  def reduce[T[_]: CanTraverse, A, M: Monoid](aReducer: A => M)(aData: T[A]): M = {
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: A) => Container[M, Unit](aReducer(a))
    tTraversable.traverse[({ type C[E] = Container[M, E] })#C, A, Unit](f)(aData).value
  }

  def reduce[T[_]: CanTraverse, A, M1: Monoid, M2: Monoid](aReducer1: A => M1, aReducer2: A => M2)(aData: T[A]): (M1, M2) = {
    import MonoidInstances.monoidPairMonoid

    val tReducer = (a: A) => (aReducer1(a), aReducer2(a))
    reduce(tReducer)(aData)
  }

  def countReducer[A] = (_: A) => 1
  def appendReducer[A] = (a: A) => a

  def countElements[T[_]: CanTraverse, A](aData: T[A]): Int = {
    import MonoidInstances.IntMonoid

    reduce(countReducer[A])(aData)
  }

  def appendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): A = {
    reduce(appendReducer[A])(aData)
  }

  def countAndAppendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): (Int, A) = {
    import MonoidInstances.IntMonoid

    reduce(countReducer[A], appendReducer[A])(aData)
  }
}