open OUnit2
open G4ip_prover

let tests = "test suite for sum" >::: [
  "TrueIsTrue" >:: (fun _ -> assert_equal (G4ip.prove True) true);
  "TrueImpTrue" >:: (fun _ -> assert_equal (G4ip.prove (Implies(True, True))) true);
  "FalseImpFalse" >:: (fun _ -> assert_equal (G4ip.prove (Implies(False, False))) true);
  "AImpA" >:: (fun _ -> assert_equal (G4ip.prove (Implies(Atom("A"), Atom("A")))) true);
]

let _ = run_test_tt_main tests