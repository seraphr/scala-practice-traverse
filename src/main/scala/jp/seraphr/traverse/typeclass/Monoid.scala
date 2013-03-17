package jp.seraphr.traverse.typeclass

trait Monoid[T] {
  def zero: T
  def append(aLeft: T, aRight: T): T
}

object MonoidInstances {
  implicit object IntMonoid extends Monoid[Int] {
    override def zero = 0
    override def append(aLeft: Int, aRight: Int) = aLeft + aRight
  }

  implicit object StringMonoid extends Monoid[String] {
    override def zero = ""
    override def append(aLeft: String, aRight: String) = aLeft + aRight
  }

  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def zero = List[T]()
    override def append(aLeft: List[T], aRight: List[T]) = aLeft ++ aRight
  }

  implicit def monoidPairMonoid[T1: Monoid, T2: Monoid]: Monoid[(T1, T2)] = new Monoid[(T1, T2)] {
    private val mMonoid1 = implicitly[Monoid[T1]]
    private val mMonoid2 = implicitly[Monoid[T2]]

    override def zero = (mMonoid1.zero, mMonoid2.zero)
    override def append(aLeft: (T1, T2), aRight: (T1, T2)) = {
      val (tLeft1, tLeft2) = aLeft
      val (tRight1, tRight2) = aRight

      (mMonoid1.append(tLeft1, tRight1), mMonoid2.append(tLeft2, tRight2))
    }

  }

}