      subroutine diagnostics_setunits (myThid, chardiag, diagunits)
C***********************************************************************
C  Purpose
C  -------
C   Wrapper routine to set the units of an active diagnostic
C
C  Arguments Description
C  ----------------------
C     myThid ..... Current Process(or)
C     chardiag ... Character *8 expression for diag to fill
C     diagunits .. Character *16 expression for diag units
C***********************************************************************
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid
      character *8 chardiag
      character *16 diagunits

c Local variables
c ===============
      integer n, ndiagnum, ipointer

C Run through list of active diagnostics to make sure
C we are trying to fill a valid diagnostic

      ndiagnum = 0
      ipointer = 0
      do n = 1,ndiagt
       if(chardiag.eq.cdiag(n)) then
        ndiagnum = n
        ipointer = idiag(n)
       endif
      enddo
      if( ndiagnum.ne.0 .and. ipointer.ne.0 ) then
       udiag(ndiagnum)=diagunits
      else
       write(6,1000)chardiag
      endif

 1000 format(' ',' Warning: Trying to set units of diagnostic ',a8,
     .        ' But it is not a valid (or active) name ')
      return
      end
