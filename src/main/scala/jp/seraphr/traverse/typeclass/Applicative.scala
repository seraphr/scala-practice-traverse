package jp.seraphr.traverse.typeclass
import jp.seraphr.traverse.data.IntContainer
import jp.seraphr.traverse.data.Container
import jp.seraphr.traverse.data.Ident
import jp.seraphr.traverse.data.Product

import language.higherKinds

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

object Applicative {
  implicit class ApplyUtil[A, B, F[_]: Applicative](f: F[A => B]) {
    def <*>(aFunctor: F[A]): F[B] = implicitly[Applicative[F]].apply(f)(aFunctor)
  }

  implicit class PointApplyUtil[A, B, F[_]: Applicative](f: A => B){
    val tApp = implicitly[Applicative[F]]

    def <|>(aFunctor: F[A]): F[B] = tApp.apply(tApp.point(f))(aFunctor)
  }
}

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

  implicit def applicativeProductAplicative[F1[_]: Applicative, F2[_]: Applicative]: Applicative[({ type P[A] = Product[F1, F2, A] })#P] = new Applicative[({ type P[A] = Product[F1, F2, A] })#P] {
    private val mApp1 = implicitly[Applicative[F1]]
    private val mApp2 = implicitly[Applicative[F2]]

    override def point[A](a: => A) = Product(mApp1.point(a), mApp2.point(a))

    override def apply[A, B](f: Product[F1, F2, A => B])(aFunctor: Product[F1, F2, A]) = {
      val tLeftF = f.l
      val tRightF = f.r
      val tLeftFunctor = aFunctor.l
      val tRightFunctor = aFunctor.r

      val tLeftApplied = mApp1.apply(tLeftF)(tLeftFunctor)
      val tRightApplied = mApp2.apply(tRightF)(tRightFunctor)

      Product(tLeftApplied, tRightApplied)
    }

    override def fmap[A, B](f: A => B)(aFunctor: Product[F1, F2, A]): Product[F1, F2, B] = {
      val tLeftFunctor = aFunctor.l
      val tRightFunctor = aFunctor.r

      val tLeftMapped = mApp1.fmap(f)(tLeftFunctor)
      val tRightMapped = mApp2.fmap(f)(tRightFunctor)

      Product(tLeftMapped, tRightMapped)
    }
  }

  implicit object OptionIsApplicative extends Applicative[Option] {
    override def point[A](a: => A) = Option(a)
    override def apply[A, B](f: Option[A => B])(aFunctor: Option[A]): Option[B] = {
      for {
        tF <- f
        tValue <- aFunctor
      } yield tF(tValue)
    }
    override def fmap[A, B](f: A => B)(aFunctor: Option[A]) = aFunctor.map(f)
  }
}
