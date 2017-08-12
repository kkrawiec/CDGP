package sygusgp

import fuel.util.FApp

class Main extends FApp {
  val benchmark = opt('benchmark)
  val sygusProblem = LoadSygusBenchmark(benchmark)
  val synthTasks = ExtractSynthesisTasks(sygusProblem)
  if (synthTasks.size > 1)
    throw new Exception("SKIPPING: Multiple synth-fun commands detected. Cannot handle such problems.")

  val evaluation = new CGDPFitness(sygusProblem)
}
