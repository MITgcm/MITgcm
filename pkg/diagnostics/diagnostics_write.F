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
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid, myIter

c Local variables
c ===============
      integer   n

C***********************************************************************
C***   Check to see if its time for Diagnostic Output                ***
C***********************************************************************
      do n=1,nlists
      if ( mod(freq(n),myIter).eq.0 ) then
       call diagout(myThid,n)
       call clrindx(myThid,n)
      endif
      enddo

      return
      end
