package jp.seraphr.traverse.data

case class Product[F1[_], F2[_], A](l: F1[A], r: F2[A]){
  val tuple = (l, r)
}
