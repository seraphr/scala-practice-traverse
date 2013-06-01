package jp.seraphr.traverse.typeclass

import org.scalatest.FunSuite

class ApplicativeUtilTest extends FunSuite {
  test("test with Option and Function1") {
    import jp.seraphr.traverse.typeclass.ApplicativeInstances._
    import jp.seraphr.traverse.typeclass.Applicative._

    val tF = (a: Int) => a.toString
    val tArg: Option[Int] = Some(10)

    val tResult = tF <|> tArg

    assert(tResult === Some("10"))
  }

  test("test with Option and Function2") {
    import jp.seraphr.traverse.typeclass.ApplicativeInstances._
    import jp.seraphr.traverse.typeclass.Applicative._

    val tF = (a: Int) => (b: Int) => (a + b).toString
    val tArg: Option[Int] = Some(10)

    val tResult = tF <|> tArg <*> Option(20)

    assert(tResult === Some("30"))
    assert(tF <|> tArg <*> None === None)
  }

  test("test with Option and Function3") {
    import jp.seraphr.traverse.typeclass.ApplicativeInstances._
    import jp.seraphr.traverse.typeclass.Applicative._

    val tF = (a: Int) => (b: Int) => (c: String) => (a + b).toString + c
    val tArg: Option[Int] = Some(10)

    val tResult = tF <|> tArg <*> Option(20) <*> Some("yen")

    assert(tResult === Some("30yen"))
    assert(tF <|> tArg <*> None === None)
  }
}