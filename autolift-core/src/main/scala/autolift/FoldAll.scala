package autolift

trait FoldAllSemantic {
  protected type Foldable[T[_]]

  protected def all[F[_] : Foldable, A](fa: F[A])(f: A => Boolean): Boolean

  /**
    * Type class supporting checking if all of some type defined by `Fn` evaluate to `true` within a nested stack of type
    * constructors.
    *
    * @author Owein Reese
    * @tparam Obj The type constructors over which to evaluate `Fn`
    * @tparam Fn  The boolean producing predicate which will be iterated over the first applicable type with type stack.
    */
  trait FoldAll[Obj, Fn] extends ((Obj, Fn) => Boolean)

  object FoldAll extends LowPriorityScalazFoldAll {
    def apply[Obj, Fn](implicit fold: FoldAll[Obj, Fn]) = fold

    implicit def base[F[_], A, C >: A](implicit fold: Foldable[F]) =
      new FoldAll[F[A], C => Boolean] {
        def apply(fa: F[A], f: C => Boolean) = all(fa)(f)
      }
  }

  trait LowPriorityScalazFoldAll {
    implicit def recur[F[_], G, Fn](implicit fold: Foldable[F], foldAll: FoldAll[G, Fn]) =
      new FoldAll[F[G], Fn] {
        def apply(fg: F[G], f: Fn) = all(fg) { g: G => foldAll(g, f) }
      }
  }

}


trait FoldAllSyntax extends FoldAllSemantic {

  implicit class FoldAllOps[F[_], A](fa: F[A]) {
    def foldAll[B](f: B => Boolean)(implicit fold: FoldAll[F[A], B => Boolean]): Boolean = fold(fa, f)
  }

  final class FoldedAll[A](f: A => Boolean) {
    def apply[That](that: That)(implicit fold: FoldAll[That, A => Boolean]): Boolean = fold(that, f)
  }

  trait FoldAllContext {
    def foldAll[A](f: A => Boolean) = new FoldedAll(f)
  }

}
