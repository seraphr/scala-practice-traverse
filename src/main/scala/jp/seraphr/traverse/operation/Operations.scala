package jp.seraphr.traverse.operation
import jp.seraphr.traverse.typeclass.Applicative
import jp.seraphr.traverse.typeclass.CanTraversable
import jp.seraphr.traverse.typeclass.CanTraversable
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.data.IntContainer
import jp.seraphr.traverse.data.Container
import jp.seraphr.traverse.typeclass.MonoidInstances

object Operations {
  def countElements[F[_]: Applicative, T[_]: CanTraversable, A](aData: T[A]): Int = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraversable[T]]

    val f = (a: A) => Container[Int, A](1)
    tTraversable.traverse[({type C[E] = Container[Int, E]})#C, A, Any](f)(aData).value
  }

  def sumElements[F[_]: Applicative, T[_]: CanTraversable](aData: T[Int]): Int = {
    import ApplicativeInstances.IntContainerApplicative
    val tTraversable = implicitly[CanTraversable[T]]

    val f = (a: Int) => IntContainer(a)
    tTraversable.traverse(f)(aData).value
  }
}