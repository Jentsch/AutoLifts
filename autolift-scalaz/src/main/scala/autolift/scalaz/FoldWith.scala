package autolift.scalaz

import autolift.FoldWith
import scalaz.{Monoid, Foldable}

trait ScalazFoldWith[Obj, Function] extends FoldWith[Obj, Function]

object ScalazFoldWith extends LowPriorityScalazFoldWith{
	def apply[Obj, Function](implicit lift: ScalazFoldWith[Obj, Function]): Aux[Obj, Function, lift.Out] = lift

	implicit def base[F[_], A, C >: A, B](implicit fold: Foldable[F], ev: Monoid[B]): Aux[F[A], C => B, B] =
		new ScalazFoldWith[F[A], C => B]{
			type Out = B

			def apply(fa: F[A], f: C => B) = fold.foldMap(fa)(f)
		}
}

trait LowPriorityScalazFoldWith{
	type Aux[Obj, Function, Out0] = ScalazFoldWith[Obj, Function]{ type Out = Out0 }

	implicit def recur[F[_], G, Function, Out0](implicit fold: Foldable[F], 
														 lift: FoldWith.Aux[G, Function, Out0], 
														 ev: Monoid[Out0]): Aux[F[G], Function, Out0] =
		new ScalazFoldWith[F[G], Function]{
			type Out = Out0

			def apply(fg: F[G], f: Function) = fold.foldMap(fg){ g: G => lift(g, f) }
		}
}

final class FoldedWith[A, B : Monoid](f: A => B){
	def andThen[C >: B, D : Monoid](that: FoldedWith[C, D]) = that compose this

	def compose[C, D <: A](that: FoldedWith[C, D]) = that map f

	def map[C : Monoid](g: B => C): FoldedWith[A, C] = new FoldedWith(f andThen g)

	def apply[That](that: That)(implicit fold: FoldWith[That, A => B]): fold.Out = fold(that, f)
}

trait FoldWithContext{
	def foldWith[A, B : Monoid](f: A => B) = new FoldedWith(f)
}

