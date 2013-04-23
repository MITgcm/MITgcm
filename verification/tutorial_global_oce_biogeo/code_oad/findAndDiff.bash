for i in `find ../../../../MITgcm -name $1 -print | grep -v verification`
do 
  echo -n "diff $i ? y/[n] "
  read answer
  if [ "$answer" == "y" ]
  then
    kdiff3 $i $1
  fi 
done
