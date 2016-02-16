package autolift.cats

import autolift.FoldAllSemantic

trait CatsFoldForallSyntax extends FoldAllSemantic {

  implicit class FoldForallOps[F[_], A](fa: F[A]) {
    def foldForall[B](f: B => Boolean)(implicit fold: FoldAll[F[A], B => Boolean]): Boolean = fold(fa, f)
  }

  final class FoldedForall[A](f: A => Boolean) {
    def apply[That](that: That)(implicit fold: FoldAll[That, A => Boolean]): Boolean = fold(that, f)
  }

  trait FoldForallContext {
    def foldAll[A](f: A => Boolean) = new FoldedForall(f)
  }
}
