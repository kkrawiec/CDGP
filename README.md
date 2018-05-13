# CDGP
Counterexample-Driven Genetic Programming (CDGP) aims to close the gap between inductive program synthesis methods using heuristic search, more precisely genetic programming (GP), and (deductive) program synthesis from a formal specification of the problem. To ensure that a synthesized program meets the formal specification for all possible inputs, a logical proof must be conducted. In our implementation, for proving program correctnes we employ an external Satisfiability Modulo Theories (SMT) solver.

CDGP, similarly to the standard GP, utilizes evolutionary search to find the expected program.
It starts with the empty set of test cases.
After a new population is created and evaluated on the current set of tests, solutions are verified against the formal specification.
If a program is correct, then the evolution ends and it is returned.
If a program is incorrect, a counterexample input returned by an SMT solver is transformed to a test and added to a set of test cases.
A parameter '--testsRatio' can be used to specify, what ratio of collected tests must be passed in order to apply verification.

Here is the conceptual diagram of CDGP, taken from the publication:

<img src="http://www.cs.put.poznan.pl/ibladek/github/cdgp/diagram.jpg" alt="CDGP diagram" width="500">

 
## Dependencies
* [FUEL](https://github.com/kkrawiec/fuel) - main evolution engine.
* [SWIM](https://github.com/kkrawiec/swim) - GP utilities for FUEL.
* [SyGuS](https://github.com/JerrySwan/SyGuS) - parser for the format used by the SyGuS ('Syntax Guided Synthesis') competition.

## How to build
You can create a project in an IDE such as Eclipse or IntelliJ IDEA, or use SBT to automatically download all dependencies and produce a jar file.


## How to run
A path to the benchmark (i.e. problem specification) is specified with '--benchmark' option.

CDGP to run requires a path to the SMT solver, which must be provided using the '--solverPath' option.
CDGP was tested for the following open source SMT solvers:
* [Z3](https://github.com/Z3Prover/z3)
* [CVC4](https://github.com/CVC4/CVC4)

Because of slight differences between them and different required parameters, a type of the solver must be provided as an argument of the '--solverType' option. The accepted values are: 'z3' (default), 'cvc4', 'other'. With the '--moreArgs' option you can provide additional solver parameters, which will be prepended to those preset for the given solver.

## How to cite

K. Krawiec, I. Błądek, J. Swan, Counterexample-Driven Genetic Programming, Genetic and Evolutionary Computation Conference (GECCO), Berlin, July 17-19, 2017, DOI: http://dx.doi.org/10.1145/3071178.3071224.
