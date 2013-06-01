package jp.seraphr.traverse.typeclass

import language.higherKinds

trait CanTraverse[T[_]] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B])(aTraversable: T[A]): F[T[B]]
}

object TraversableInstances {
  implicit object ListTraversable extends CanTraverse[List] {
    def cons[A] = (aHead: A) => (aList: List[A]) => aHead :: aList

    override def traverse[F[_]: Applicative, A, B](f: A => F[B])(aTraversable: List[A]): F[List[B]] = {
      val tApp = implicitly[Applicative[F]]

      def point[A](a: A) = tApp.point(a)
      def apply[A, B](f: A => B)(a: F[A]) = tApp.apply(point(f))(a)

      aTraversable match {
        case h :: t => tApp.apply(apply(cons[B])(f(h)))(traverse(f)(t))
        case _ => point(Nil)
      }
    }
  }

  implicit def nestedTraversable[Outer[_]: CanTraverse, Inner[_]: CanTraverse] = new CanTraverse[({ type N[E] = Outer[Inner[E]] })#N] {
    override def traverse[F[_]: Applicative, A, B](f: A => F[B])(aTraversable: Outer[Inner[A]]): F[Outer[Inner[B]]] = {
      val tApp = implicitly[Applicative[F]]
      val tOuterTraversable = implicitly[CanTraverse[Outer]]
      val tInnerTraversable = implicitly[CanTraverse[Inner]]

      val tFunc = (aInner: Inner[A]) => {
          tInnerTraversable.traverse(f)(aInner)
      }

      tOuterTraversable.traverse(tFunc)(aTraversable)
    }
  }
}