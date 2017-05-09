#!/bin/csh
#
#  $Header: /u/gcmpack/MITgcm/tools/example_scripts/ref_machine/setenv_OpenAD.csh,v 1.1 2017/05/09 00:56:08 jmc Exp $
#  $Name:  $

##########################################################
# This file is part of OpenAD released under the LGPL.   #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the OpenAD distribution             #
##########################################################

# this set some env. vars such as "OPENADROOT"
#  (needed to generate and use Makefile)

#./tools/setenv/setenv.py --shell=csh > setenv.tmp~
set tmp_file="/tmp/setenv_OpenAD.$$"
#echo $tmp_file
/home/jm_c/OpenAD/tools/setenv/setenv.py --shell=csh > $tmp_file
if ( $status != 0 ) then
  echo  "Error executing: ./tools/setenv/setenv.py --shell=csh > $tmp_file"
else
  source $tmp_file
  if ( $status != 0 ) then
    echo "Error executing: source $tmp_file"
  else
    rm -f $tmp_file
  endif
endif
