package jp.seraphr.traverse.operation
import jp.seraphr.traverse.typeclass.Applicative
import jp.seraphr.traverse.typeclass.CanTraverse
import jp.seraphr.traverse.typeclass.CanTraverse
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.data.IntContainer
import jp.seraphr.traverse.data.Container
import jp.seraphr.traverse.typeclass.MonoidInstances
import jp.seraphr.traverse.typeclass.Monoid
import jp.seraphr.traverse.data.Ident

object Operations {
  def countElements[T[_]: CanTraverse, A](aData: T[A]): Int = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: A) => Container[Int, A](1)
    tTraversable.traverse[({ type C[E] = Container[Int, E] })#C, A, Any](f)(aData).value
  }

  def sumElements[T[_]: CanTraverse](aData: T[Int]): Int = {
    import ApplicativeInstances.IntContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: Int) => IntContainer(a)
    tTraversable.traverse(f)(aData).value
  }

  def appendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): A = {
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: A) => Container[A, A](a)
    tTraversable.traverse[({ type C[E] = Container[A, E] })#C, A, Any](f)(aData).value
  }

  def map[T[_]: CanTraverse, A, B](f: A => B)(aData: T[A]): T[B] = {
    import ApplicativeInstances.IdentApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    tTraversable.traverse((a: A) => Ident(f(a)))(aData).value
  }
}