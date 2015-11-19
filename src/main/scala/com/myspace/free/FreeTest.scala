package com.myspace.free

// see https://softwaremill.com/free-monads/
// see http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html

import scalaz.{Free, Id, ~>, Coyoneda}
import scalaz.std.list._
import scalaz.syntax.traverse._

object Orchestration {
  type UserId = Int
  type UserName = String
  type UserImg = String

  final case class User(id: UserId, name: UserName, img: UserImg)
  final case class Tweet(user: UserId, msg: String)

  sealed trait Service[A]
  final case class GetUserName(userId: UserId) extends Service[UserName]
  final case object GetTweets extends Service[List[Tweet]]
  final case class GetUserImg(userId: UserId) extends Service[UserImg]

  sealed trait Request[A]
  final case class Pure[A](a: A) extends Request[A]
  final case class Fetch[A](service: Service[A]) extends Request[A]

  type Requestable[A] = Coyoneda[Request, A]

  object Request {
    def pure[A](a: A): Free[Requestable, A] = Free.liftFC(Pure(a) : Request[A])
    def fetch[A](service: Service[A]): Free[Requestable, A] = Free.liftFC(Fetch(service) : Request[A])
  }

  object IdInterpreter extends (Request ~> Id.Id) {
    import Id._

    override def apply[A](fa: Request[A]): Id[A] = fa match {
      case Pure(a)        => a
      case Fetch(service) => service match {
        case GetUserName(id) => {
          println(s"GetUserName($id)")
          id match {
            case 1 => "Ada"
            case 2 => "Basic"
            case _ => "Unknown"
          }
        }
        case GetUserImg(id) => {
          println(s"GetUserImg($id)")
          id match {
            case 1 => ":-)"
            case 2 => ":-("
            case _ => "---"
          }
        }
        case GetTweets => {
          println(s"GetTweets")
          List(Tweet(1, "Hi1"), Tweet(2, "Hi2"), Tweet(1, "Bye1"))
        }
      }
    }
  }

  object Program {
    import Request._

    val theId: UserId = 1

    def getUser(id: UserId): Free[Requestable, User] =
      for {
        name <- fetch(GetUserName(id))
        img  <- fetch(GetUserImg(id))
      } yield User(id, name, img)

    def free: Free[Requestable, List[(String,User)]] =
      for {
        tweets <- fetch(GetTweets)
        result <- (tweets map { tweet =>
          for {
            user <- getUser(tweet.user)
          } yield tweet.msg -> user
        }).sequenceU
      } yield result
  }
}

object FreeTest extends App {
  println("Free Monads")

  import Orchestration.Program.free
  import Orchestration.IdInterpreter

  val ma = Free.runFC(free)(IdInterpreter)

  println("=============")
  val t = ma map { case (s,u) => s"$s -> $u"}
  t foreach println
}
