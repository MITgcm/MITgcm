C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addamp_adj.F,v 1.2 2012/08/12 18:49:09 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: ADDAMP_ADJ
C     !INTERFACE:
      SUBROUTINE ADDAMP_ADJ(
     I       NrIn, minRef, maxRef, timeDamp,
     I       fieldRef,
     U       adfieldDamp,
     I       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | author: Gael Forget
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL minRef,maxRef,timeDamp
      _RL fieldRef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
      _RL adfieldDamp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      _RL facDamp
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,NrIn
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( (fieldRef(i,j,k,bi,bj).GE.maxRef).OR.
     &          (fieldRef(i,j,k,bi,bj).LE.minRef) ) THEN
              facDamp=(1. _d 0-deltaTClock/timeDamp)
           ELSE
              facDamp=1. _d 0
           ENDIF
           adfieldDamp(i,j,k,bi,bj)=facDamp*
     &        adfieldDamp(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
