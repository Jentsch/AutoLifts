package autolift.scalaz

import scalaz.{Functor, Bind}
import autolift.LiftFlatMapSemantik 


trait ScalazLiftBindSyntax extends LiftFlatMapSemantik {

	def liftBind[A, B, M[_]](f: A => M[B])(implicit bind: Bind[M]) = new LiftedBind(f)

	/// Syntax extension providing for a `LiftBind` method.
	implicit class LiftBindOps[F[_], A](fa: F[A]) {

		/**
		 * Automatic lifting and flattening of the contained function `f` such that the application point is dicated by the
		 * argument and return type of the function.
		 *
		 * @param f the function that returns a type with a Bind.
		 * @tparam B the argument type of the function.
		 * @tparam C the inner type of the return type of the function.
		 * @tparam M the higher-kinded type of the return type of the function which has a Bind.
		 */
		def liftBind[B, C, M[_]](f: B => M[C])(implicit lift: LiftFlatMap[F[A], B => M[C]]): lift.Out = lift(fa, f)
	}

  final class LiftedBind[A, B, M[_]](protected val f: A => M[B])(implicit bind: Bind[M]){
    def andThen[C >: B, D](that: LiftedBind[C, D, M]) = new LiftedBind({ x: A => bind.bind(f(x))(that.f) })

    def compose[C, D <: A](that: LiftedBind[C, D, M]) = that andThen this

    def map[C](g: B => C): LiftedBind[A, C, M] = new LiftedBind({ x: A => bind.map(f(x))(g) })

    def apply[That](that: That)(implicit lift: LiftFlatMap[That, A => M[B]]): lift.Out = lift(that, f)
  }
}

