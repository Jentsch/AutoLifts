package autolift.scalaz

import autolift.FoldExistsSemantic

trait ScalazFoldAnySyntax extends FoldExistsSemantic {

  implicit class FoldAnyOps[F[_], A](fa: F[A]) {
    def foldAny[B](f: B => Boolean)(implicit fold: FoldExists[F[A], B => Boolean]) = fold(fa, f)
  }

  trait FoldAnyContext {
    def foldAny[A](f: A => Boolean): FoldedAny[A] = new FoldedExists(f)

    type FoldedAny[A] = FoldedExists[A]
  }

}

