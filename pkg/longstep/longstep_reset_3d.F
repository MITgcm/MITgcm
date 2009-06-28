C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_reset_3d.F,v 1.3 2009/06/28 16:35:48 jmc Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_RESET_3D
C     !INTERFACE:
      SUBROUTINE LONGSTEP_RESET_3D(
     O      lsCnt, lsFld,
     I      myNr, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_RESET_3D
C     | o reset longstep average for one field
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
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        lsCnt(bi,bj) = 0
        DO k = 1,myNr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           lsFld(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_LONGSTEP */

      RETURN
      END

