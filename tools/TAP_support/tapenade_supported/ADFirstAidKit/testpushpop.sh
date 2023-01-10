for i in {1..99}
 do
  testpushpop 0 "*" $i
  testpushpop 0 "3[* *]" $i
  testpushpop 0 "* 2[* 2[*] *]" $i
  testpushpop 0 "* * (* *) * *" $i
  testpushpop 0 "* * 2[* * * *] * *" $i
  testpushpop 0 "* * 2[* * (* * * *) *] * *" $i
  testpushpop 0 "* * 2[* * 3[*] *] * *" $i
  testpushpop 0 "* * 2[* (* 3[*] *) * (3[((*)*)*]) * * *] *" $i
  testpushpop 0 "* (*) * L(* *) *" $i
  testpushpop 0 "* L(* (* L((*) *) *) *) *" $i
  testpushpop 0 "* 3[* L(* *) *] *" $i
  testpushpop 0 "* (* 2[* 3[* L(* *) *] *] *) (*) *" $i
  testpushpop 0 "* 3[2[(*) *] (* *) *] *" $i
  testpushpop 0 "* 3[(* *) * 2[(*) *]] *" $i
  testpushpop 0 "* (((*) *) * (* (*))) *" $i
  testpushpop 0 "* (2[((*) *)] 3[* (* (*))]) *" $i
done
