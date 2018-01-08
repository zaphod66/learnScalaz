object MyTrampoline {
  object Data {
    val l1 = List.fill(10001)('a')
    val l2 = List.fill(10002)(0)
  }

  sealed trait Trampoline[+A] {
    final def run: A = this match {
      case Done(a) => a
      case More(k) => k().run
    }
  }

  case class Done[+A](a: A) extends Trampoline[A]
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

  def even[A](as: List[A]): Trampoline[Boolean] = as match {
    case Nil     => Done(true)
    case _ :: xs => More( () => odd(xs) )
  }

  def odd[A](as: List[A]): Trampoline[Boolean] = as match {
    case Nil     => Done(false)
    case _ :: xs => More( () => even(xs) )
  }

  even(Data.l1).run
  even(Data.l2).run
}
