C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_write_datetime.F,v 1.2 2004/10/22 14:52:14 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_DATETIME( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write Fizhi date and time to ascii file

      implicit none

#include "chronos.h"

C     !INPUT PARAMETERS:
      integer myIter, myThid
      _RL myTime
CEOP

      integer ku

      if( myThid.eq.1) then
C      Write Supplemental Ascii File with Current Time Info
       open(10,file='datetime',form='formatted')
       write(10,1000)nymd,nhms
       print *,' Writing datetime file: NYMD = ',nymd,' NHMS = ',nhms
       close (10)
      endif
 1000 format(i8,2x,i6)

      return
      end
