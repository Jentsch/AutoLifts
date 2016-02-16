package autolift

trait FoldExistsSemantic {
  protected type Foldable[F[_]]

  protected def exists[F[_] : Foldable, A](fa: F[A])(f: A => Boolean): Boolean

  /**
    * Type class supporting checking if at least one of a type defined by `Function` evaluate to `true` within a nested stack of
    * type constructors.
    *
    * @author Owein Reese
    * @tparam Obj      The type constructors over which to evaluate `Function`
    * @tparam Function The boolean producing function which will be iterated over the first applicable type with type stack.
    */
  trait FoldExists[Obj, Function] extends ((Obj, Function) => Boolean)


  object FoldExists extends LowPriorityFoldExists {
    def apply[Obj, Fn](implicit fold: FoldExists[Obj, Fn]) = fold

    implicit def base[F[_], A, C >: A](implicit fold: Foldable[F]) =
      new FoldExists[F[A], C => Boolean] {
        def apply(fa: F[A], f: C => Boolean) = exists(fa)(f)
      }
  }

  trait LowPriorityFoldExists {

    implicit def recur[F[_], G, Fn](implicit fold: Foldable[F], recExists: FoldExists[G, Fn]) =
      new FoldExists[F[G], Fn] {
        def apply(fg: F[G], f: Fn) = exists(fg) { g: G => recExists(g, f) }
      }
  }

  final class FoldedExists[A](f: A => Boolean) {
    def apply[That](that: That)(implicit fold: FoldExists[That, A => Boolean]): Boolean = fold(that, f)
  }

}

trait FoldExistsSyntax extends FoldExistsSemantic {

  implicit class FoldExistsOps[F[_], A](fa: F[A]) {
    def foldExists[B](f: B => Boolean)(implicit fold: FoldExists[F[A], B => Boolean]): Boolean = fold(fa, f)
  }


  trait FoldExistsContext {
    def foldExists[A](f: A => Boolean) = new FoldedExists(f)
  }

}
