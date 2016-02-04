package autolift

trait LiftMapSemantik {
  protected type Functor[T[_]]
  protected def map[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B]

	implicit def liftedMapFunctor[A]: Functor[LiftedMap[A, ?]]

  /**
   * Type class supporting the mapping over an arbitrary nesting of type constructors.
   *
   * @author Owein Reese
   *
   * @tparam Obj the type to be lifted into.
   * @tparam Function the function to be lifted.
   */
  trait LiftMap[Obj, Function] extends DFunction2[Obj, Function]
  object LiftMap extends LowPriorityLiftMap {
    def apply[Obj, Fn](implicit lift: LiftMap[Obj, Fn]): Aux[Obj, Fn, lift.Out] = lift

    implicit def base[F[_], A, C >: A, B](implicit functor: Functor[F]): Aux[F[A], C => B, F[B]] =
      new LiftMap[F[A], C => B]{
        type Out = F[B]

        def apply(fa: F[A], f: C => B) = map(fa)(f)
      }
  }

  trait LowPriorityLiftMap{
    type Aux[Obj, Fn, Out0] = LiftMap[Obj, Fn]{ type Out = Out0 }

    implicit def recur[F[_], G, Fn](implicit functor: Functor[F], lift: LiftMap[G, Fn]): Aux[F[G], Fn, F[lift.Out]] =
      new LiftMap[F[G], Fn]{
        type Out = F[lift.Out]

        def apply(fg: F[G], f: Fn) = map(fg){ g: G => lift(g, f) }
      }
  }

  final class LiftedMap[A, B](f: A => B){
    def andThen[C >: B, D](that: LiftedMap[C, D]) = that compose this

    def compose[C, D <: A](that: LiftedMap[C, D]) = that map f

    def map[C](g: B => C): LiftedMap[A, C] = new LiftedMap(f andThen g)

    def apply[That](that: That)(implicit lift: LiftMap[That, A => B]): lift.Out = lift(that, f)
  }

}

/**
 * Importing a instance of this trait will add the {{{liftMap}}} methods on
 * Functors.
 */
trait LiftMapSyntax extends LiftMapSemantik {


  /// Syntax extension providing for a `liftMap` method.
  implicit class LiftMapOps[F[_], A](fa: F[A]){

    /**
     * Automatic lifting of the function `f` over the object such that the application point is dictated by the type
     * of function invocation.
     *
     * @param f the function to be lifted.
     * @tparam B the argument type of the function.
     * @tparam C the return type of the function.
     */
    def liftMap[B, C](f: B => C)(implicit lift: LiftMap[F[A], B => C]): lift.Out = lift(fa, f)
  }

  def liftMap[A, B](f: A => B) = new LiftedMap(f)

}

