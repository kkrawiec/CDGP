package cdgp

import fuel.func._
import fuel.core.StatePop
import fuel.util.{Collector, Options, TRandom}
import swim.tree._


class CDGPConvectionEqualNumber[S <: Op, E <: Fitness](
                                override val cdgpState: State,
                                val evalFunc: EvalFunction[S,E],
                                eaCreator: () => EACore[S,E] with CDGPAlgorithm[S,E],
                                reportPreDivide: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] = (s:Seq[StatePop[(S, E)]]) => s,
                                reportPostDivide: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] = (s:Seq[StatePop[(S, E)]]) => s)
                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
  extends MultipopulationEA[S, E](
    MultipopulationEA.convectionEqualNumber(opt("multipop.M", 5), ordering),
    eaCreator,
    maxIter = Option(opt("multipop.maxGenerations", 100)),
    maxTime = Option(opt("multipop.maxTime", 86400000)),
    stop = (pop: StatePop[(S, E)]) => {pop.exists(_._2.correct)})
    with CDGPAlgorithm[S, E] {

  override def iter: Seq[StatePop[(S,E)]] => Seq[StatePop[(S,E)]] =
    popsEvolve andThen savePop andThen reportPreDivide andThen popsDivide andThen
      reportPostDivide andThen report //andThen printPops

  def savePop(pops: Seq[StatePop[(S, E)]]): Seq[StatePop[(S, E)]] = {
    pop = Some(StatePop(pops.flatten))
    pops
  }

  override def report: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] = bsfPops
  override def epilogue: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    bsfPops andThen reportStatsPops andThen epiloguePops
  override val bsf = BestSoFar[S, E](ordering, it)


  override def popsEvolve: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] =
    (pops: Seq[StatePop[(S,E)]]) => pops.map{ pop =>
      val ea = eaCreator()
      // Synchronize fitness to avoid problems
      val synchrPop = StatePop(pop.map { s => evalFunc.updateEval(s) })
      ea.apply(synchrPop)
    }


  def bsfPops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      pops.foreach(bsf(_))
      pops
    }

  def reportStatsPops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      pops.foreach(reportStats(_))
      pops
    }

  def epiloguePops: Function1[Seq[StatePop[(S, E)]], Seq[StatePop[(S, E)]]] =
    (pops: Seq[StatePop[(S, E)]]) => {
      coll.set("multipop.totalGenerations", it.count)
      pops
    }
}


object CDGPConvectionEqualNumber {
  def reportAvgsInGroups(keyPopSuffix: String)(implicit coll: Collector): Seq[StatePop[(Op, FInt)]] => Seq[StatePop[(Op, FInt)]] =
    (pops: Seq[StatePop[(Op, FInt)]]) => {
      pops.indices.zip(pops).foreach { case (i, pop) =>
        val numTests = pop.head._2.totalTests.asInstanceOf[Double]
        val listRatios = pop.map{ case (s, e) => (numTests - e.value) / numTests }
        val k = s"multipop.pop${i}${keyPopSuffix}.fitness"
        computeAndSaveStats(k, listRatios)
      }
      pops
    }

  def reportAvgsInGroupsFSeqInt(keyPopSuffix: String)(implicit coll: Collector): Seq[StatePop[(Op, FSeqInt)]] => Seq[StatePop[(Op, FSeqInt)]] =
    (pops: Seq[StatePop[(Op, FSeqInt)]]) => {
      pops.indices.zip(pops).foreach { case (i, pop) =>
        val numTests = pop.head._2.totalTests.asInstanceOf[Double]
        val listRatios = pop.map{ case (s, e) => (numTests - e.sum) / numTests }
        val k = s"multipop.pop${i}${keyPopSuffix}"
        computeAndSaveStats(k, listRatios)
      }
      pops
    }

  protected def computeAndSaveStats(k: String, passedRatios: Seq[Double])(implicit coll: Collector) {
    val avg = passedRatios.sum / passedRatios.size
    val stdDev = Tools.stddev(passedRatios, avg)
    computeAndSaveStats(k, avg, stdDev )
  }

  protected def computeAndSaveStats(k: String, avg: Double, stdDev: Double)(implicit coll: Collector) {
    val roundedAvg = BigDecimal(avg).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    val roundedStdDev = BigDecimal(stdDev).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    coll.set(k+".fitnessAvg", coll.get(k+".fitnessAvg").getOrElse("") + s"$roundedAvg,")
    coll.set(k+".fitnessStdDev", coll.get(k+".fitnessStdDev").getOrElse("") + s"$roundedStdDev,")
  }
}