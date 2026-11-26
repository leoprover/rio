package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{Formula, mkConjs, mkDisjs}
import net.aurelee.rio.sat.{consequence, globalPicoSATInstance}

sealed abstract class NetOutputFunction {
  def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula]
  def entails(outFamily: Seq[Seq[Formula]], conjecture: Formula): Boolean
}
case object CredulousNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] =
    throw new UnsupportedOperationException("Credulous output has no single deductively closed basis; retain the family or use entails")

  override def entails(outFamily: Seq[Seq[Formula]], conjecture: Formula): Boolean =
    outFamily.exists(basis => consequence(basis, conjecture))
}
case object SkepticalNetOutput extends NetOutputFunction {
  override def apply(outFamily: Seq[Seq[Formula]]): Seq[Formula] = Seq(mkDisjs(outFamily.map(mkConjs)))

  override def entails(outFamily: Seq[Seq[Formula]], conjecture: Formula): Boolean =
    outFamily.forall(basis => consequence(basis, conjecture))
}
