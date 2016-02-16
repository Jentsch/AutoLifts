package autolift.scalaz

import autolift._

trait Syntax extends LiftMapSyntax
	with LiftApSyntax
	with LiftFilterSyntax
	with LiftFoldLeftSyntax
	with LiftFoldRightSyntax
	with LiftFoldAtSyntax
	with LiftFlattenSyntax
	with LiftFoldSyntax
	with LiftFoldMapSyntax
	with LiftAnySyntax
	with LiftAllSyntax
	with LiftZipSyntax
	with LiftZipWithSyntax
	with LiftMergeSyntax
	with LiftMergeWithSyntax
	with FoldAllSyntax
	with FoldCompleteSyntax
	with FoldOverSyntax
	with FoldWithSyntax

