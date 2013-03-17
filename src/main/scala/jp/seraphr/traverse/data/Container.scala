package jp.seraphr.traverse.data
import jp.seraphr.traverse.typeclass.Monoid

case class Container[E, +A](value: E)