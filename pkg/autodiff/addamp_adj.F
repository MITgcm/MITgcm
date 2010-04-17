C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addamp_adj.F,v 1.1 2010/04/17 20:47:24 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ADDAMP_ADJ
C     !INTERFACE:
      SUBROUTINE ADDAMP_ADJ(
     I    NrIn,minRef,maxRef,timeDamp,
     I    fieldRef,adfieldDamp,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | author: Gael Forget
C     *==========================================================*

      IMPLICIT NONE

C     == Global arguments ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer NrIn, myThid
      _RL fieldRef(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)
      _RL adfieldDamp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)
      _RL minRef,maxRef,timeDamp,facDamp
      integer bi,bj,i,j,k

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,NrIn
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           if ( (fieldRef(i,j,k,bi,bj).GE.maxRef).OR.
     &          (fieldRef(i,j,k,bi,bj).LE.minRef) ) then
              facDamp=(1. _d 0-deltaTClock/timeDamp)
           else
              facDamp=1. _d 0
           endif
           adfieldDamp(i,j,k,bi,bj)=facDamp*
     &        adfieldDamp(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      end

