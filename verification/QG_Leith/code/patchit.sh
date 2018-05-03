ROOTDIR=~/fortran/MITgcm.new

for somefile in $(ls PARAMS.h FFIELDS.h)
do 
  echo $somefile
  cp $somefile patches/${somefile}.old
  cp $ROOTDIR/model/inc/$somefile .
  patch -p0 $somefile < patches/${somefile}.patch
  mv ${somefile}.old patches
done

for somefile in $(ls *.F)
do 
  echo $somefile
  cp $somefile patches/${somefile}.old
  cp $ROOTDIR/model/src/$somefile .
  patch -p0 $somefile < patches/${somefile}.patch
  mv ${somefile}.old patches
done

