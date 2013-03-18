package jp.seraphr.traverse.typeclass
import jp.seraphr.traverse.data.IntContainer
import jp.seraphr.traverse.data.Container
import jp.seraphr.traverse.data.Ident

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
  implicit object ListApplicative extends Applicative[List] {
    override def point[A](a: => A): List[A] = List(a)
    override def apply[A, B](f: List[A => B])(aFunctor: List[A]): List[B] = for {
      tFunc <- f
      tEl <- aFunctor
    } yield tFunc(tEl)

    override def fmap[A, B](f: A => B)(aFunctor: List[A]) = aFunctor.map(f)
  }

  implicit object IntContainerApplicative extends Applicative[IntContainer] {
    override def point[A](a: => A): IntContainer[A] = IntContainer(0)
    override def apply[A, B](f: IntContainer[A => B])(aFunctor: IntContainer[A]): IntContainer[B] = {
      IntContainer[B](f.value + aFunctor.value)
    }

    override def fmap[A, B](f: A => B)(aFunctor: IntContainer[A]) = IntContainer[B](aFunctor.value)
  }

  implicit def monoidContainerApplicative[E: Monoid]: Applicative[({ type C[_A] = Container[E, _A] })#C] = new Applicative[({ type C[_A] = Container[E, _A] })#C] {
    override def point[A](a: => A) = Container[E, A](implicitly[Monoid[E]].zero)
    override def apply[A, B](f: Container[E, A => B])(aFunctor: Container[E, A]) = {
      Container[E, B](implicitly[Monoid[E]].append(f.value, aFunctor.value))
    }

    override def fmap[A, B](f: A => B)(aFunctor: Container[E, A]) = Container[E, B](aFunctor.value)
  }

  implicit object IdentApplicative extends Applicative[Ident] {
    override def point[A](a: => A) = Ident(a)
    override def apply[A, B](f: Ident[A => B])(aFunctor: Ident[A]): Ident[B] = {
      val tF = f.value
      val tValue = aFunctor.value

      point(tF(tValue))
    }
    override def fmap[A, B](f: A => B)(aFunctor: Ident[A]) = Ident(f(aFunctor.value))
  }
}
