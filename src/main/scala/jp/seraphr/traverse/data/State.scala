package jp.seraphr.traverse.data

case class State[S, +A](runState: S => (S, A)) {
  def flatMap[B](aFunc: A => State[S, B]): State[S, B] = {
    State((s: S) => {
      val (tBeforeState, tBeforeValue) = runState(s)
      aFunc(tBeforeValue).runState(tBeforeState)
    })
  }
  def map[B](aFunc: A => B): State[S, B] = {
    val tFunc = (a: A) => State((s: S) => (s, aFunc(a)))
    flatMap(tFunc)
  }
}