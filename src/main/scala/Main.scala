import cats._
import cats.std.all._

object Main extends App {

  implicit class GenCompOps[A, B](f: A ⇒ B) {
    def ~>[B2, C, C2](g: B2 ⇒ C)(implicit gc: GenCompAux[A, B, B2, C, C2]): A ⇒ C2 = gc(f, g)
  }

  object GenCompAux extends GenCompAuxLowPriority {
    implicit def pure[A, B, C] = new GenCompAux[A, B, B, C, C] {
      def apply(f: A ⇒ B, g: B ⇒ C): A ⇒ C = f andThen g
    }
    implicit def monad[M[_], A, B, C](implicit monad: Monad[M]) =
      new GenCompAux[A, M[B], B, M[C], M[C]] {
        def apply(f: A ⇒ M[B], g: B ⇒ M[C]): A ⇒ M[C] = x ⇒ monad.flatMap(f(x))(g)
      }
  }
  trait GenCompAuxLowPriority {
    implicit def functor[F[_], A, B, C](implicit functor: Functor[F]) =
      new GenCompAux[A, F[B], B, C, F[C]] {
        def apply(f: A ⇒ F[B], g: B ⇒ C): A ⇒ F[C] = x ⇒ functor.map(f(x))(g)
      }
  }

  trait GenCompAux[A, B, B2, C, C2] {
    def apply(f: A ⇒ B, g: B2 ⇒ C): A ⇒ C2
  }

  val f1: Int ⇒ Double = i ⇒ i / 3.0
  val f2: Int ⇒ Option[Double] = i ⇒ Option(i / 3.0)
  val g1: Double ⇒ String = d ⇒ d.toString
  val g2: Double ⇒ Option[String] = d ⇒ Option(d.toString)

  val h0 = f1 ~> g1
  val h1 = f2 ~> g1
  val h2 = f2 ~> g2

  println(h0(1))
  println(h1(1))
  println(h2(1))
}

