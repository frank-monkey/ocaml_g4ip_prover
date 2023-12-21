open OUnit2
open G4ip_prover

let tests = "tests" >::: [
  (*simple tests*)
  "TrueIsTrue" >:: (fun _ -> assert_equal (G4ip.prove True) true);
  "TrueImpTrue" >:: (fun _ -> assert_equal (G4ip.prove (Implies(True, True))) true);
  "FalseImpFalse" >:: (fun _ -> assert_equal (G4ip.prove (Implies(False, False))) true);
  "FalseImpA" >:: (fun _ -> assert_equal (G4ip.prove (Implies(False, Atom("A")))) true);
  "AImpA" >:: (fun _ -> assert_equal (G4ip.prove (Implies(Atom("A"), Atom("A")))) true);
  
  (*more complex tests*)
  "AAndAAImpliesBImpliesB" >:: (fun _ -> assert_equal (G4ip.prove (Implies(And(Atom("A"), Implies(And(Atom("A"), Atom("A")), Atom("B"))), Atom("B")))) true);
  "AAndAImpliesBImpliesBAndB" >:: (fun _ -> assert_equal (G4ip.prove (Implies(And(Atom("A"), Implies(Atom("A"), Atom("B"))), And(Atom("B"), Atom("B"))))) true);
  "true1" >:: (fun _ -> assert_equal (G4ip.prove (Implies(And(Atom("A"), Implies(Implies(Atom("A"), Atom("B")), Atom("C"))), And(Atom("A"), Implies(Atom("B"), Atom("C")))))) true);

  (*false tests*)
  "A" >:: (fun _ -> assert_equal (G4ip.prove (Atom("A"))) false);
  "Falsehood" >:: (fun _ -> assert_equal (G4ip.prove (False)) false);
  "TrueImpliesFalse" >:: (fun _ -> assert_equal (G4ip.prove (Implies(True, False))) false);
  "LawOfExcludedMiddle" >:: (fun _ -> assert_equal (G4ip.prove (Or(Atom("A"), Implies(Atom("A"), False)))) false);
  "DoubleNegationElimination" >:: (fun _ -> assert_equal (G4ip.prove (Implies(Implies(Implies(Atom("A"), False), False), Atom("A")))) false);
  "false1" >:: (fun _ -> assert_equal (G4ip.prove (Or(Atom("A"), Implies(Atom("A"), False)))) false);
  "false2" >:: (fun _ -> assert_equal (G4ip.prove (Implies(Implies(Implies(Atom("A"), False), False), Atom("A")))) false);
]

let _ = run_test_tt_main tests