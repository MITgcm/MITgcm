echo -n "make copies for all files"
for j in `ls *.F`
do 
  cp $j ${j%.F}_cb2m.FF90
  echo -n "."
done
echo ""
for i in `ls *_mod.h | sed 's/\(.*\)_mod\.h/\1/'`
do 
  echo -n "replacing: $i " 
  echo  "s/include *\(\"${i}.h\"\)/include \"${i}_mod.h\"/" > script.sed
  for j in `egrep -l "include *\"$i" *_cb2m.FF90`
  do 
    echo -n "."
    cat $j | sed -f script.sed | sed 's/IMPLICIT NONE//' | sed 's/implicit none//' > ${j}.1
    mv ${j}.1 $j	
  done
  echo ""     
done
