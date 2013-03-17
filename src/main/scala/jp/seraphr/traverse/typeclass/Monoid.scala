package jp.seraphr.traverse.typeclass

trait Monoid[T] {
    def zero: T
    def append(aLeft: T, aRight: T): T
}

object MonoidInstances{
  implicit object IntMonoid extends Monoid[Int]{
    override def zero = 0
    override def append(aLeft: Int, aRight: Int) = aLeft + aRight
  }

  implicit object StringMonoid extends Monoid[String]{
    override def zero = ""
    override def append(aLeft: String, aRight: String) = aLeft + aRight
  }

  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]]{
    override def zero = List[T]()
    override def append(aLeft: List[T], aRight: List[T]) = aLeft ++ aRight
  }

}