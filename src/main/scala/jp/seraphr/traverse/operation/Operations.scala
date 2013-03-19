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
import jp.seraphr.traverse.data.Product

object Operations {
  def countElements[T[_]: CanTraverse, A](aData: T[A]): Int = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: A) => Container[Int, Unit](1)
    tTraversable.traverse[({ type C[E] = Container[Int, E] })#C, A, Unit](f)(aData).value
  }

  def sumElements[T[_]: CanTraverse](aData: T[Int]): Int = {
    import ApplicativeInstances.IntContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: Int) => IntContainer(a)
    tTraversable.traverse(f)(aData).value
  }

  def sumAndSumElements[T[_]: CanTraverse](aData: T[Int]): (Int, Int) = {
    import ApplicativeInstances.IntContainerApplicative
    import ApplicativeInstances.applicativeProductAplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: Int) => Product[IntContainer, IntContainer, Unit](IntContainer(a), IntContainer(a))
    val (tLeft, tRight) = tTraversable.traverse[({type P[E] = Product[IntContainer, IntContainer, E]})#P, Int, Unit](f)(aData).tuple

    (tLeft.value, tRight.value)
  }

  def appendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): A = {
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = (a: A) => Container[A, Unit](a)
    tTraversable.traverse[({ type C[E] = Container[A, E] })#C, A, Unit](f)(aData).value
  }

  def countAndAppendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): (Int, A) = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative
    import ApplicativeInstances.applicativeProductAplicative

    implicit val tApplicative = applicativeProductAplicative[({ type C1[E1] = Container[Int, E1] })#C1, ({ type C2[E2] = Container[A, E2] })#C2]

    val tTraversable = implicitly[CanTraverse[T]]
    val f = (a: A) => Product[({ type C1[E1] = Container[Int, E1] })#C1, ({ type C2[E2] = Container[A, E2] })#C2, Unit](Container[Int, Unit](1), Container[A, Unit](a))

    val (tLeft, tRight) = tTraversable.traverse[({ type P[E] = Product[({ type C1[E1] = Container[Int, E1] })#C1, ({ type C2[E2] = Container[A, E2] })#C2, E] })#P, A, Unit](f)(aData).tuple

    (tLeft.value, tRight.value)
  }

  def map[T[_]: CanTraverse, A, B](f: A => B)(aData: T[A]): T[B] = {
    import ApplicativeInstances.IdentApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    tTraversable.traverse((a: A) => Ident(f(a)))(aData).value
  }
}