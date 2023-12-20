let rec _factorial n =
  if n <= 1 
  then 1
else _factorial (n-1) * n;;