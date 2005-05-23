C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_write_datetime.F,v 1.3 2005/05/23 02:48:41 jmc Exp $
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
       CALL MDSFINDUNIT( ku, myThid )
       open(ku,file='datetime',form='formatted')
       write(ku,1000)nymd,nhms
       print *,' Writing datetime file: NYMD = ',nymd,' NHMS = ',nhms
       close (ku)
      endif
 1000 format(i8,2x,i6)

      return
      end
