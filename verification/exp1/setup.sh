#!/bin/csh -f
#
#
clear

echo "|==========================================================|"
echo "|                                                          |"
echo "|       MITgcmUV Baseline Code reconfiguration script      |"
echo "|                                                          |"
echo "|==========================================================|"

set UV_CONFIG_DIR = ( `pwd` )
echo " "
echo " - Using MITgcmUV configuration directory = " \"${UV_CONFIG_DIR}\"

cd ../..
set UV_BASELINE_DIR = ( `pwd` )
echo " - Updating MITgcmUV baseline directory = " \"${UV_BASELINE_DIR}\"

echo " "
echo 'Press ctrl-C to quit, yes to continue'
echo " "

set response = ( $< )
if ( "$response" != "yes" ) then
 echo You said \" $response \" configure abandoned
 sleep 2
 exit
endif

clear
echo "|==========================================================|"
echo "|                                                          |"
echo "|      MITgcmUV Baseline Code reconfiguration script       |"
echo "|                                                          |"
echo "|==========================================================|"
echo " "
echo "Building list of files to change "
cd ${UV_CONFIG_DIR}/changes
set fc = 0
foreach f ( `find . -type f` )
 set CVS_DIR = ( `echo $f | grep 'CVS' ` )
 if ( "$CVS_DIR" == "" ) then
  set f = ( `echo $f | sed s'/^\.\/\(.*\)/\1/' ` )
  set fnam = ${f:t}
  if ( -f ${UV_BASELINE_DIR}/$f ) then
   cmp $f ${UV_BASELINE_DIR}/$f >& /dev/null
   set cstat = ( $status )
  endif
  if ( "$cstat" != "0" ) then
   echo \"$f\"
   @ fc = $fc + 1
  endif
 endif
end

if ( "$fc" == "0" ) then
 echo " "
 echo '** No changes are needed **'
 echo All files in configuration directory \"${UV_CONFIG_DIR}/changes\" are the same as those in \"${UV_BASELINE_DIR}\"
 sleep 2
 echo " "
 exit
endif

echo " "
echo 'Press ctrl-C to quit, yes to continue'
echo " "

set response = ( $< )
if ( "$response" != "yes" ) then
 echo You said \" $response \" configure abandoned
 sleep 2
 exit
endif

clear
echo "|==========================================================|"
echo "|                                                          |"
echo "|      MITgcmUV Baseline Code reconfiguration script       |"
echo "|                                                          |"
echo "|==========================================================|"
echo " "
cd ${UV_CONFIG_DIR}/changes
foreach f ( `find . -type f` )
 set CVS_DIR = ( `echo $f | grep 'CVS' ` )
 if ( "$CVS_DIR" == "" ) then
  set f = ( `echo $f | sed s'/^\.\/\(.*\)/\1/' ` )
  set fnam = ${f:t}
  if ( -f ${UV_BASELINE_DIR}/$f ) then
   cmp $f ${UV_BASELINE_DIR}/$f >& /dev/null
   set cstat = ( $status )
  endif
  if ( "$cstat" != "0" ) then
   echo "Replacing $f"
   \cp -r $f ${UV_BASELINE_DIR}/$f
  endif
 endif
end

