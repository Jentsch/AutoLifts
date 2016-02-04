package autolift


trait LiftFlatMapSemantic { self =>
  protected type Functor[F[_]]
  protected type Monad[F[_]]
  protected def map[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B]
  protected def flatMap[M[_]: Monad, A, B](ma: M[A])(f: A => M[B]): M[B]

  // implicit def liftedFlatMapFunctor[A, M[_]]: Functor[LiftFlatMap[A, ?, M]]

  /**
   * Type class supporting flat mapping a function over an arbitrary nesting of type constructors.
   *
   * @author Owein Reese
   *
   * @tparam Obj The type to be lifted into.
   * @tparam Function The function to be lifted.
   */
  trait LiftFlatMap[Obj, Function] extends DFunction2[Obj, Function]
  object LiftFlatMap extends LowPriorityLiftFlatMap {
    def apply[Obj, Fn](implicit lift: LiftFlatMap[Obj, Fn]): Aux[Obj, Fn, lift.Out] = lift

    implicit def base[M[_]: Monad, A, C >: A, B]: Aux[M[A], C => M[B], M[B]] =
      new LiftFlatMap[M[A], C => M[B]]{
        type Out = M[B]

        def apply(fa: M[A], f: C => M[B]) = flatMap(fa)(f)
      }
  }

  trait LowPriorityLiftFlatMap{
    type Aux[Obj, Fn, Out0] = LiftFlatMap[Obj, Fn]{ type Out = Out0 }

    implicit def recur[F[_]: Functor, G, Fn](implicit lift: LiftFlatMap[G, Fn]): Aux[F[G], Fn, F[lift.Out]] =
      new LiftFlatMap[F[G], Fn]{
        type Out = F[lift.Out]

        def apply(fg: F[G], f: Fn) = map(fg){ g: G => lift(g, f) }
      }
  }

  final class LiftedFlatMap[A, B, M[_]: Monad: Functor](protected val f: A => M[B]) {
    def andThen[C >: B, D](that: LiftedFlatMap[C, D, M]) = new LiftedFlatMap({ x: A => flatMap(f(x))(that.f) })

    def compose[C, D <: A](that: LiftedFlatMap[C, D, M]) = that andThen this

    def map[C](g: B => C): LiftedFlatMap[A, C, M] = new LiftedFlatMap({ x: A => self.map(f(x))(g) })

    def apply[That](that: That)(implicit lift: LiftFlatMap[That, A => M[B]]): lift.Out = lift(that, f)
  }
}

trait LiftFlatMapSyntax extends LiftFlatMapSemantic {

  /// Syntax extension providing for a `liftFlatMap` method.
  implicit class LiftFlatMapOps[F[_], A](fa: F[A]){

    /**
     * Automatic lifting and flattening of the contained function `f` such that the application point is dicated by the
     * argument and return type of the function.
     *
     * @param f the function that returns a type with a Monad.
     * @tparam B the argument type of the function.
     * @tparam C the inner type of the return type of the function.
     * @tparam M the higher-kinded type of the return type of the function which has a Monad.
     */
    def liftFlatMap[B, C, M[_]](f: B => M[C])(implicit lift: LiftFlatMap[F[A], B => M[C]]): lift.Out = lift(fa, f)
  }

	def liftFlatMap[A, B, M[_]: Monad: Functor](f: A => M[B]) = new LiftedFlatMap(f)
}

