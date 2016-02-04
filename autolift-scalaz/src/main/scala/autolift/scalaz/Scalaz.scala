package autolift

import autolift.scalaz._

object Scalaz extends Syntax with Context with Reexports with Implicits with LiftMapSyntax with ScalazLiftBindSyntax {

  protected type Functor[T[_]] = _root_.scalaz.Functor[T]
  protected type Monad[T[_]] = _root_.scalaz.Bind[T]
  protected def map[F[_], A, B](fa: F[A])(f: A => B)(implicit fun: Functor[F]): F[B] =
    fun.map(fa)(f)
  protected def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit m: Monad[M]): M[B] =
    m.bind(ma)(f)

	implicit def liftedMapFunctor[A]: Functor[LiftedMap[A, ?]] =
    new Functor[LiftedMap[A, ?]] {
      def map[B, C](lm: LiftedMap[A, B])(f: B => C) = lm map f
    }


	implicit def mkAp[Obj, Fn](implicit lift: ScalazLiftAp[Obj, Fn]): ScalazLiftAp.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkFldL[Obj, Fn, Z](implicit lift: ScalazLiftFoldLeft[Obj, Fn, Z]): ScalazLiftFoldLeft.Aux[Obj, Fn, Z, lift.Out] = lift
	implicit def mkFldR[Obj, Fn, Z](implicit lift: ScalazLiftFoldRight[Obj, Fn, Z]): ScalazLiftFoldRight.Aux[Obj, Fn, Z, lift.Out] = lift
	implicit def mkFld[Obj](implicit lift: ScalazLiftFold[Obj]): ScalazLiftFold.Aux[Obj, lift.Out] = lift
	implicit def mkFlM[Obj, Fn](implicit lift: ScalazLiftFoldMap[Obj, Fn]): ScalazLiftFoldMap.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkFlA[M[_], Obj](implicit lift: ScalazLiftFoldAt[M, Obj]): ScalazLiftFoldAt.Aux[M, Obj, lift.Out] = lift
	implicit def mkFl[M[_], Obj](implicit lift: ScalazLiftFlatten[M, Obj]): ScalazLiftFlatten.Aux[M, Obj, lift.Out] = lift
	implicit def mkFil[Obj, Fn](implicit lift: ScalazLiftFilter[Obj, Fn]): ScalazLiftFilter[Obj, Fn] = lift
	implicit def mkFAll[Obj, Fn](implicit lift: ScalazFoldAll[Obj, Fn]): ScalazFoldAll[Obj, Fn] = lift
	implicit def mkFAny[Obj, Fn](implicit lift: ScalazFoldExists[Obj, Fn]): ScalazFoldExists[Obj, Fn] = lift
	implicit def mkAll[Obj, Fn](implicit lift: ScalazLiftForAll[Obj, Fn]): ScalazLiftForAll.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkAny[Obj, Fn](implicit lift: ScalazLiftExists[Obj, Fn]): ScalazLiftExists.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkFC[Obj](implicit lift: ScalazFoldComplete[Obj]): ScalazFoldComplete.Aux[Obj, lift.Out] = lift
	implicit def mkFW[Obj, Fn](implicit lift: ScalazFoldWith[Obj, Fn]): ScalazFoldWith.Aux[Obj, Fn, lift.Out] = lift
	implicit def mkFO[M[_], Obj](implicit lift: ScalazFoldOver[M, Obj]): ScalazFoldOver.Aux[M, Obj, lift.Out] = lift
	implicit def mkZip[Obj1, Obj2](implicit lift: ScalazLiftZip[Obj1, Obj2]): ScalazLiftZip.Aux[Obj1, Obj2, lift.Out] = lift
	implicit def mkZW[Obj1, Obj2, Fn](implicit lift: ScalazLiftZipWith[Obj1, Obj2, Fn]): ScalazLiftZipWith.Aux[Obj1, Obj2, Fn, lift.Out] = lift
	implicit def mkJ[Obj1, Obj2](implicit lift: ScalazLiftMerge[Obj1, Obj2]): ScalazLiftMerge.Aux[Obj1, Obj2, lift.Out] = lift
	implicit def mkJw[Obj1, Obj2, Fn](implicit lift: ScalazLiftMergeWith[Obj1, Obj2, Fn]): ScalazLiftMergeWith.Aux[Obj1, Obj2, Fn, lift.Out] = lift
}

