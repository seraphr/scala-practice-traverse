package jp.seraphr.traverse.typeclass
import jp.seraphr.traverse.data.IntContainer

trait Functor[F[_]] {
  def fmap[A, B](f: A => B)(aFunctor: F[A]): F[B]
}

trait PointedFunctor[F[_]] extends Functor[F] {
  def point[A](a: => A): F[A]
}

trait Applic[F[_]] extends Functor[F] {
  def apply[A, B](f: F[A => B])(aFunctor: F[A]): F[B]
}

trait Applicative[F[_]] extends PointedFunctor[F] with Applic[F]

object ApplicativeInstances {
  object ListApplicative extends Applicative[List] {
    override def point[A](a: => A): List[A] = List(a)
    override def apply[A, B](f: List[A => B])(aFunctor: List[A]): List[B] = for {
      tFunc <- f
      tEl <- aFunctor
    } yield tFunc(tEl)

    override def fmap[A, B](f: A => B)(aFunctor: List[A]) = aFunctor.map(f)
  }

  object IntContainerApplicative extends Applicative[IntContainer] {
    override def point[A](a: => A): IntContainer[A] = IntContainer(0)
    override def apply[A, B](f: IntContainer[A => B])(aFunctor: IntContainer[A]): IntContainer[B] = {
      IntContainer[B](f.value + aFunctor.value)
    }

    override def fmap[A, B](f: A => B)(aFunctor: IntContainer[A]) = IntContainer[B](aFunctor.value)
  }
}