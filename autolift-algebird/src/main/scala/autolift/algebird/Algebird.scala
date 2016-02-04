package autolift

import autolift.algebird._

object Algebird extends Syntax with Context with Reexports with Implicits with LiftMapSyntax with LiftFlatMapSyntax {
  protected type Functor[T[_]] = com.twitter.algebird.Functor[T]
  protected type Monad[T[_]] = com.twitter.algebird.Monad[T]
  protected def map[F[_], A, B](fa: F[A])(f: A => B)(implicit fun: Functor[F]): F[B] =
    fun.map(fa)(f)
  protected def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit m: Monad[M]): M[B] =
    m.flatMap(ma)(f)

	implicit def liftedMapFunctor[A]: Functor[LiftedMap[A, ?]] =
    new Functor[LiftedMap[A, ?]] {
      def map[B, C](lm: LiftedMap[A, B])(f: B => C) = lm map f
    }

	implicit def mkAp[Obj, Fn](implicit lift: AlgeLiftAp[Obj, Fn]): AlgeLiftAp.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkFl[M[_], Obj](implicit lift: AlgeLiftFlatten[M, Obj]): AlgeLiftFlatten.Aux[M, Obj, lift.Out] = lift
	implicit def mkJ[Obj1, Obj2](implicit lift: AlgeLiftMerge[Obj1, Obj2]): AlgeLiftMerge.Aux[Obj1, Obj2, lift.Out] = lift
	implicit def mkJw[Obj1, Obj2, Fn](implicit lift: AlgeLiftMergeWith[Obj1, Obj2, Fn]): AlgeLiftMergeWith.Aux[Obj1, Obj2, Fn, lift.Out] = lift
	implicit def mkFil[Obj, Fn](implicit lift: AlgeLiftFilter[Obj, Fn]): AlgeLiftFilter[Obj, Fn] = lift
}

