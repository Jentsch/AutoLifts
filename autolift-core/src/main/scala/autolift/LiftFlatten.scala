package autolift


trait LiftFlattenSemantic {
  protected type Functor[F[_]]
  protected type FlatMap[F[_]]
  protected def map[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B]
  protected def flatMap[M[_]: FlatMap, A, B](ma: M[A])(f: A => M[B]): M[B]

  /**
   * Typeclass supporting flattening a double nested type within a nested type constructor.
   *
   * @author Owein Reese
   *
   * @tparam M The type over which to flatten
   * @tparam Obj The object over which to lift the flatten.
   */
  trait LiftFlatten[M[_], Obj] extends DFunction1[Obj]
  object LiftFlatten extends LowPriorityLiftFlatten{
    def apply[M[_], Obj](implicit lift: LiftFlatten[M, Obj]): Aux[M, Obj, lift.Out] = lift

    implicit def base[M[_], A](implicit fm: FlatMap[M]): Aux[M, M[M[A]], M[A]] =
      new LiftFlatten[M, M[M[A]]]{
        type Out = M[A]

        def apply(mma: M[M[A]]) = flatMap(mma){ ma: M[A] => ma }
      }
  }

  trait LowPriorityLiftFlatten{
    type Aux[M[_], Obj, Out0] = LiftFlatten[M, Obj]{ type Out = Out0 }

    implicit def recur[M[_], F[_], G](implicit functor: Functor[F], lift: LiftFlatten[M, G]): Aux[M, F[G], F[lift.Out]] =
      new LiftFlatten[M, F[G]]{
        type Out = F[lift.Out]

        def apply(fg: F[G]) = map(fg){ g: G => lift(g) }
      }
  }
}

trait LiftFlattenSyntax extends LiftFlattenSemantic {

  ///Syntax extension providing for a `liftFlatten` method.
  implicit class LiftFlattenOps[F[_], A](fa: F[A]){

    /**
     * Automatic lifting of a flatten operation given the juxtaposition of the two of the given types in the nested type 
     * structure.
     *
     * @tparam M the type over which to flatten given that there exists the concept of flattening of the type.
     */
    def liftFlatten[M[_]](implicit lift: LiftFlatten[M, F[A]]): lift.Out = lift(fa)
  }
}


