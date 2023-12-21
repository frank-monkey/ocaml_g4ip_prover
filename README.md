# Automated Sequent Calculus Prover
This project is an Automated Sequent Calculus Prover developed in OCaml, utilizing the G4ip logic system.

### Prerequisites
Before cloning, ensure you have the following installed:
- OCaml
- Dune (build system)
- OUnit2 (testing)

### Building the Project
To build the project, use the following command:
```bash
dune build
```
### Usage
The prover accepts input in the form of logical propositions. The input is located in bin/main.ml. Define the propositions you wish to evaluate, and
evaluate using 
```bash
dune exec g4ip_prover
```
### Miscellaneous
The logic system is implemented in lib/g4ip.ml. It defines the logical rules and structures used by the prover.
To ensure correctness, the prover is accompanied by a suite of tests. The tests are located in test/test_g4ip_prover.ml. To run the tests, use:
```bash
dune runtest
```
Note that nothing will be displayed if the tests all succeed.
