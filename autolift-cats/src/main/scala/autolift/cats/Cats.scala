package autolift

import autolift.cats._

object Cats extends Syntax with Context with Reexports with Implicits with LiftMapSyntax with LiftFlatMapSyntax {
  protected type Functor[F[_]] = _root_.cats.Functor[F]
  protected type FlatMap[M[_]] = _root_.cats.FlatMap[M]

  protected def map[F[_], A, B](fa: F[A])(f: A => B)(implicit fun: Functor[F]): F[B] =
    fun.map(fa)(f)
  protected def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit m: FlatMap[M]): M[B] =
    m.flatMap(ma)(f)

	implicit def liftedMapFunctor[A]: Functor[LiftedMap[A, ?]] =
    new Functor[LiftedMap[A, ?]] {
      def map[B, C](lm: LiftedMap[A, B])(f: B => C) = lm map f
    }

  implicit def mkAp[Obj, Fn](implicit lift: CatsLiftAp[Obj, Fn]): CatsLiftAp.Aux[Obj, Fn, lift.Out] = lift
  implicit def mkFldL[Obj, Fn, Z](implicit lift: CatsLiftFoldLeft[Obj, Fn, Z]): CatsLiftFoldLeft.Aux[Obj, Fn, Z, lift.Out] = lift
  implicit def mkFldR[Obj, Fn, Z](implicit lift: CatsLiftFoldRight[Obj, Fn, Z]): CatsLiftFoldRight.Aux[Obj, Fn, Z, lift.Out] = lift
  implicit def mkFld[Obj](implicit lift: CatsLiftFold[Obj]): CatsLiftFold.Aux[Obj, lift.Out] = lift
  implicit def mkFlM[Obj, Fn](implicit lift: CatsLiftFoldMap[Obj, Fn]): CatsLiftFoldMap.Aux[Obj, Fn, lift.Out] = lift
  implicit def mkFlA[M[_], Obj](implicit lift: CatsLiftFoldAt[M, Obj]): CatsLiftFoldAt.Aux[M, Obj, lift.Out] = lift
  implicit def mkFl[M[_], Obj](implicit lift: CatsLiftFlatten[M, Obj]): CatsLiftFlatten.Aux[M, Obj, lift.Out] = lift
  implicit def mkFil[Obj, Fn](implicit lift: CatsLiftFilter[Obj, Fn]): CatsLiftFilter[Obj, Fn] = lift
  implicit def mkFAll[Obj, Fn](implicit lift: CatsFoldAll[Obj, Fn]): CatsFoldAll[Obj, Fn] = lift
  implicit def mkFAny[Obj, Fn](implicit lift: CatsFoldExists[Obj, Fn]): CatsFoldExists[Obj, Fn] = lift
  implicit def mkAll[Obj, Fn](implicit lift: CatsLiftForAll[Obj, Fn]): CatsLiftForAll.Aux[Obj, Fn, lift.Out] = lift
  implicit def mkAny[Obj, Fn](implicit lift: CatsLiftExists[Obj, Fn]): CatsLiftExists.Aux[Obj, Fn, lift.Out] = lift
  implicit def mkFC[Obj](implicit lift: CatsFoldComplete[Obj]): CatsFoldComplete.Aux[Obj, lift.Out] = lift
  implicit def mkFW[Obj, Fn](implicit lift: CatsFoldWith[Obj, Fn]): CatsFoldWith.Aux[Obj, Fn, lift.Out] = lift
  implicit def mkFO[M[_], Obj](implicit lift: CatsFoldOver[M, Obj]): CatsFoldOver.Aux[M, Obj, lift.Out] = lift
  implicit def mkJ[Obj1, Obj2](implicit lift: CatsLiftMerge[Obj1, Obj2]): CatsLiftMerge.Aux[Obj1, Obj2, lift.Out] = lift
  implicit def mkJw[Obj1, Obj2, Fn](implicit lift: CatsLiftMergeWith[Obj1, Obj2, Fn]): CatsLiftMergeWith.Aux[Obj1, Obj2, Fn, lift.Out] = lift
}
