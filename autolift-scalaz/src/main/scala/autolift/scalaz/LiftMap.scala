package autolift.scalaz

import autolift.LiftMapSemantic

import scalaz.{Functor, ~>}

trait ScalazLiftMapSemantic extends LiftMapSemantic {
  implicit def polyBase[A, F[_], C[_], B[_]](implicit functor: Functor[F]) =
    new LiftMap[F[C[A]], C ~> B] {
      type Out = F[B[A]]

      override def apply(fa: F[C[A]], f: C ~> B): this.Out =
        map(fa) { i: C[A] => f(i) }
    }
}

