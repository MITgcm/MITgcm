#!/bin/csh -f
#
# 64-bit single precision
set sym64bitConst = ( E )
#
# 32-bit single precision
set sym64bitConst = ( D )
sed s'/ * _d  */'${sym64bitConst}'/g'
