      subroutine diagnostics_write (myThid, myIter) 
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     myIter ..... Current Iteration Number
C     myThid ..... Current Process(or)
C***********************************************************************
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      integer myThid, myIter

c Local variables
c ===============
      integer   n

C***********************************************************************
C***   Check to see if its time for Diagnostic Output                ***
C***********************************************************************
      do n=1,nlists
      if ( myIter.ne.niter0) then
       if ( mod(myIter,freq(n)).eq.0 ) then
        call diagout(myThid,myIter,n)
        call clrindx(myThid,n)
       endif
      endif
      enddo

      return
      end
