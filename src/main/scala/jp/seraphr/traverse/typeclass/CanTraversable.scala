package jp.seraphr.traverse.typeclass

trait CanTraversable[T[_]] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B])(aTraversable: T[A]): F[T[B]]
}

object TraversableInstances {
  implicit object ListTraversable extends CanTraversable[List] {
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
}