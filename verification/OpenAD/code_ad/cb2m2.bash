for i in `ls *_mod.h | sed 's/\(.*\)_mod\.h/\1/'`
do 
  echo -n "replacing: $i " 
  for j in `egrep -l "include *\"$i" *.F`
  do 
    echo -n "."
    echo  "s/include *\(\"${i}.h\"\)/include \"${i}_mod.h\"/" > script.sed
    cat $j | sed -f script.sed | sed 's/IMPLICIT NONE//' | sed 's/implicit none//' > $j.1
    mv $j.1 $j
  done
  echo ""     
done
