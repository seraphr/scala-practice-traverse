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
import jp.seraphr.traverse.data.Product

object Operations {
  def traverse[T[_]: CanTraverse, F1[_]: Applicative, A, B](aFunc1: A => F1[B])(aData: T[A]): F1[T[B]] = {
    val tTraversable = implicitly[CanTraverse[T]]
    tTraversable.traverse[F1, A, B](aFunc1)(aData)
  }

  def traverse[T[_]: CanTraverse, F1[_]: Applicative, F2[_]: Applicative, A, B](aFunc1: A => F1[B], aFunc2: A => F2[B])(aData: T[A]): (F1[T[B]], F2[T[B]]) = {
    import ApplicativeInstances.applicativeProductAplicative

    val tProductFunc = product(aFunc1, aFunc2)
    val tTraversable = implicitly[CanTraverse[T]]

    tTraversable.traverse[({ type P[E] = Product[F1, F2, E] })#P, A, B](tProductFunc)(aData).tuple
  }

  def product[F1[_]: Applicative, F2[_]: Applicative, A, B](aApp1: A => F1[B], aApp2: A => F2[B]): A => Product[F1, F2, B] = {
    (a: A) =>
      {
        Product(aApp1(a), aApp2(a))
      }
  }

  def sumFunc = (a: Int) => IntContainer(a)
  def countFunc[A] = (a: A) => Container[Int, Unit](1)
  def appendFunc[A] = (a: A) => Container[A, Unit](a)
  def mapFunc[A, B](f: A => B) = (a: A) => Ident(f(a))
  def contentFunc[A] = (a: A) => Container[List[A], Unit](List(a))

  def sumElements[T[_]: CanTraverse](aData: T[Int]): Int = {
    import ApplicativeInstances.IntContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = sumFunc
    tTraversable.traverse(f)(aData).value
  }

  def sumAndSumElements[T[_]: CanTraverse](aData: T[Int]): (Int, Int) = {
    import ApplicativeInstances.IntContainerApplicative

    val tSumFunc = sumFunc
    val (tLeft, tRight) = traverse(tSumFunc, tSumFunc)(aData)
    (tLeft.value, tRight.value)
  }

  def countElements[T[_]: CanTraverse, A](aData: T[A]): Int = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative

    val f = countFunc[A]
    traverse[T, ({ type C[E] = Container[Int, E] })#C, A, Unit](f)(aData).value
  }

  def appendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): A = {
    import ApplicativeInstances.monoidContainerApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    val f = appendFunc[A]
    tTraversable.traverse[({ type C[E] = Container[A, E] })#C, A, Unit](f)(aData).value
  }

  def countAndAppendElements[T[_]: CanTraverse, A: Monoid](aData: T[A]): (Int, A) = {
    import MonoidInstances.IntMonoid
    import ApplicativeInstances.monoidContainerApplicative

    val tCountFunc = countFunc[A]
    val tAppendFunc = appendFunc[A]
    val (tLeft, tRight) = traverse[T, ({ type C[E] = Container[Int, E] })#C, ({ type C[E] = Container[A, E] })#C, A, Unit](tCountFunc, tAppendFunc)(aData)

    (tLeft.value, tRight.value)
  }

  def content[T[_]: CanTraverse, A](aData: T[A]): List[A] = {
    import MonoidInstances.listMonoid
    import ApplicativeInstances.monoidContainerApplicative

    traverse[T, ({ type C[E] = Container[List[A], E] })#C, A, Unit](contentFunc[A])(aData).value
  }

  def map[T[_]: CanTraverse, A, B](f: A => B)(aData: T[A]): T[B] = {
    import ApplicativeInstances.IdentApplicative
    val tTraversable = implicitly[CanTraverse[T]]

    tTraversable.traverse(mapFunc(f))(aData).value
  }
}