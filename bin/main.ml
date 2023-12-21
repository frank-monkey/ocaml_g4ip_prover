open G4ip_prover

(*input below*)
let prop = G4ip.Implies(G4ip.Atom("A"), G4ip.Atom("A"))
let t_or_f = if (G4ip.prove (prop)) then "true" else "false" ;;
print_endline ("The equation is constructively " ^ t_or_f)