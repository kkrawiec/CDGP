package cdgp

import fuel.util.Options
import swim.tree.Op


class SpecialTestsEvaluator[EVecEl](val partialConstraintsInFitness: Boolean,
                                    val globalConstraintInFitness: Boolean,
                                    val sizeInFitness: Boolean,
                                    val programSizeFun: Op => EVecEl,
                                    val weight: Int = 1) {

  def numberOfSpecialTests(state: StateCDGP): Int = {
    weight *
      ((if (!partialConstraintsInFitness) 0 else state.sygusData.formalConstr.size) +
        (if (sizeInFitness) 1 else 0) +
        (if (globalConstraintInFitness) 1 else 0))
  }

  /** Constructs an evaluation vector for the special tests. */
  def getEvalVector(state: StateCDGP)(s: Op, passValue: EVecEl, nonpassValue: EVecEl): Seq[EVecEl] = {
    var vector = Seq[EVecEl]()
    val w = weight
    if (partialConstraintsInFitness)
      vector = Tools.duplicateElements(getPartialConstraintsEvalVector(state)(s, passValue, nonpassValue), w) ++: vector
    if (globalConstraintInFitness)
      vector = Tools.duplicateElements(Seq(getGlobalConstraintsDecision(state)(s, passValue, nonpassValue)), w) ++: vector
    if (sizeInFitness)
      vector = Tools.duplicateElements(Seq(programSizeFun(s)), w) ++: vector
    vector
  }

  /** Verifies solution on all formal constraints in order to add this info to the fitness vector. */
  def getGlobalConstraintsDecision(state: StateCDGP)(s: Op, passValue: EVecEl, nonpassValue: EVecEl): EVecEl = {
    val (dec, _) = state.verify(s)
    if (dec == "unsat") passValue else nonpassValue
  }

  /** Verifies solution on partial constraints in order to add this info to the fitness vector. */
  def getPartialConstraintsEvalVector(state: StateCDGP)(s: Op, passValue: EVecEl, nonpassValue: EVecEl): Seq[EVecEl] = {
    state.sygusData.formalConstr.map{ constr =>
      val template = new TemplateVerification(state.sygusData, false, state.timeout, Some(Seq(constr)))
      val (dec, _) = state.verify(s, template)  //TODO: counterexamples can be collected here too
      if (dec == "unsat") passValue else nonpassValue
    }
  }
}


object SpecialTestsEvaluator {
  def apply[EVecEl](programSizeFun: Op => EVecEl)(implicit opt: Options): SpecialTestsEvaluator[EVecEl] = {
    val partialConstraintsInFitness: Boolean = opt('partialConstraintsInFitness, if (opt('method) == "CDGPprops") true else false)
    val globalConstraintInFitness: Boolean = opt('globalConstraintInFitness, false)
    val sizeInFitness: Boolean = opt('sizeInFitness, false)
    val weight: Int = opt('partialConstraintsWeight, 1, (x: Int) => x >= 1)
    new SpecialTestsEvaluator(partialConstraintsInFitness, globalConstraintInFitness, sizeInFitness, programSizeFun, weight)
  }
}