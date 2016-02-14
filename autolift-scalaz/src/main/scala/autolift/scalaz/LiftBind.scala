package autolift.scalaz

import scalaz.Alpha.M
import scalaz.{~>, Functor, Bind}
import autolift.LiftFlatMapSemantic

trait ScalazLiftBindSemantic extends LiftFlatMapSemantic {
	implicit def polyBindBase[M[_]: FlatMap, A[_], C]:
	    LiftFlatMap.Aux[M[A[C]], A ~> Lambda[X => M[X]], M[C]] =
		new LiftFlatMap[M[A[C]], A ~> Lambda[X => M[X]]]{
			type Out = M[C]

			def apply(fa: M[A[C]], f: A ~> Lambda[X => M[X]]): this.Out =
        flatMap(fa) { i: A[C] => f(i) }
		}
}

trait ScalazLiftBindSyntax extends ScalazLiftBindSemantic {

	def liftBind[A, B, M[_]](f: A => M[B])(implicit bind: Bind[M]) = new LiftedBind(f)

	/// Syntax extension providing for a `LiftBind` method.
	implicit class LiftBindOps[F[_], A](fa: F[A]) {

		/**
		 * Automatic lifting and flattening of the contained function `f` such that the application point is dictated by the
		 * argument and return type of the function.
		 *
		 * @param f the function that returns a type with a Bind.
		 */
		def liftBind[Func](f: Func)(implicit lift: LiftFlatMap[F[A], Func]): lift.Out = lift(fa, f)
	}

  final class LiftedBind[A, B, M[_]](protected val f: A => M[B])(implicit bind: Bind[M]){
    def andThen[C >: B, D](that: LiftedBind[C, D, M]) = new LiftedBind({ x: A => bind.bind(f(x))(that.f) })

    def compose[C, D <: A](that: LiftedBind[C, D, M]) = that andThen this

    def map[C](g: B => C): LiftedBind[A, C, M] = new LiftedBind({ x: A => bind.map(f(x))(g) })

    def apply[That](that: That)(implicit lift: LiftFlatMap[That, A => M[B]]): lift.Out = lift(that, f)
  }
}

