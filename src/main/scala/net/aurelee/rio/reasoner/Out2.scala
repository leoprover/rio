package net.aurelee.rio.reasoner

import net.aurelee.rio.core.{CNF, Formula, Norm}
import net.aurelee.rio.core.OutOperator
import net.aurelee.rio.core.{cnf, cnfFormulaToMultiset, mkConjs, mkDisjs, mkNeg, heads}


object Out2 extends OutOperator {
  override final def name: String = "out2"
  override final def apply(norms: Seq[Norm], input: Seq[Formula], throughput: Boolean): Seq[Formula] = {
    if (throughput) {
      materialization(norms).concat(input)
    } else {
      val negatedBodies: Seq[CNF] = norms.map(n => cnfFormulaToMultiset(cnf(mkNeg(n._1))))
      getMinimallyWeaklyTriggeredSets(input, negatedBodies).map { support =>
        mkDisjs(support.toSeq.sorted.map(i => norms(i)._2))
      }
    }
  }
}
