C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_average_3d.F,v 1.3 2009/06/28 16:35:48 jmc Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_AVERAGE_3D
C     !INTERFACE:
      SUBROUTINE LONGSTEP_AVERAGE_3D(
     I      lsCnt,
     U      lsFld,
     I      myNr, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_AVERAGE_3D
C     | o compute longstep average for one field (incl. overlap)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C      lsCnt :: Counter to update
C      lsFld :: average to update
C       myNr :: number of levels to update
C     myThid :: my Thread Id number
      INTEGER myNr
      _RL lsFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      INTEGER lsCnt(nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj
      _RL factor
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        factor = FLOAT(lsCnt(bi,bj))
        IF (lsCnt(bi,bj).NE.0) factor = 1. _d 0 / factor
        DO k = 1,myNr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           lsFld(i,j,k,bi,bj) = lsFld(i,j,k,bi,bj)*factor
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_LONGSTEP */

      RETURN
      END

