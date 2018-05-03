for somefile in $(ls *.h)
do 
  echo $somefile
  diff -u ../../../model/inc/$somefile $somefile > $somefile.patch
  mv patches/$somefile.patch patches/$somefile.patch.old
  mv $somefile.patch patches
done

for somefile in $(ls KPP*)
do 
  echo $somefile
  diff -u ../../../pkg/kpp/$somefile $somefile > $somefile.patch
  mv patches/$somefile.patch patches/$somefile.patch.old
  mv $somefile.patch patches
done

for somefile in $(ls *.F)
do 
  echo $somefile
  diff -u ../../../model/src/$somefile $somefile > $somefile.patch
  mv patches/$somefile.patch patches/$somefile.patch.old
  mv $somefile.patch patches
done

