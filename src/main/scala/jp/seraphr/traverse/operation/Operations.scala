package jp.seraphr.traverse.operation
import jp.seraphr.traverse.typeclass.Applicative
import jp.seraphr.traverse.typeclass.CanTraversable
import jp.seraphr.traverse.typeclass.CanTraversable
import jp.seraphr.traverse.typeclass.ApplicativeInstances
import jp.seraphr.traverse.data.IntContainer

object Operations {
  def countElements[F[_]: Applicative, T[_]: CanTraversable, A](aData: T[A]): Int = {
    implicit val IntContainerApplicative = ApplicativeInstances.IntContainerApplicative
    val tTraversable = implicitly[CanTraversable[T]]

    val f = (a: A) => IntContainer(1)

    tTraversable.traverse(f)(aData).value
  }
}