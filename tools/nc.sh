#!/bin/csh -f
#
# Usage: nc oldvar newvar filelist
#

if ( $#argv < 3 ) then
 echo "Usage: $0 oldvar newvar filelist"
 exit
endif

set  oldvar = "$1";
set  newvar = "$2";
shift; shift;

echo Replace \""$oldvar"\" with \"$newvar\" '[yes/(no)]'
set response = ( $< )
if ( "$response" != "yes" ) then
 exit
endif

set savedir = ( nc-$$ )
mkdir $savedir

foreach curfile ( $* )

 echo Substituting \"$newvar\" for \""$oldvar"\" in file \"$curfile\".
 cp   $curfile $savedir/${curfile:t}.before
 cat  $savedir/${curfile:t}.before | sed s/'\([^a-zA-Z_]\)\('"$oldvar"'\)/\1'$newvar'/g' > $curfile

end
#
# cat $oldfile | sed s/'\([^a-zA-Z]\)\([Nn][Zz]\)/\1Nr/' > $newfile
#
