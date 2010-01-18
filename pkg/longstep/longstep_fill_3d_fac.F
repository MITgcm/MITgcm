C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_fill_3d_fac.F,v 1.2 2010/01/18 01:02:56 jmc Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_FILL_3D_FAC
C     !INTERFACE:
      SUBROUTINE LONGSTEP_FILL_3D_FAC(
     U      lsCnt, lsFld,
     I      fld, fac, myNr, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_FILL_3D_FAC
C     | o update longstep average for one field (incl. overlap)
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
C        fld :: field to accumulate
C       myNr :: number of levels to update
C     myThid :: my Thread Id number
      INTEGER myNr
      _RL lsFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RL Fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS Fac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      INTEGER lsCnt(nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        lsCnt(bi,bj) = lsCnt(bi,bj) + 1
        DO k = 1,myNr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           lsFld(i,j,k,bi,bj) = lsFld(i,j,k,bi,bj)
     &                        + fld(i,j,k,bi,bj)*fac(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_LONGSTEP */

      RETURN
      END

