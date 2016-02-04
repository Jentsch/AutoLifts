package autolift.scalaz

import autolift.LiftFlattenSemantic

trait ScalazLiftJoinSyntax extends LiftFlattenSemantic {
  implicit class LiftJoinOps[F[_], A](fa: F[A]){
    /**
     * Automatic lifting of a flatten operation given the juxtaposition of the two of the given types in the nested type 
     * structure.
     *
     * @tparam M the type over which to flatten given that there exists the concept of flattening of the type.
     */
    def liftJoin[M[_]](implicit lift: LiftFlatten[M, F[A]]): lift.Out = lift(fa)
  }
}

