C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_calc_uvflow.F,v 1.2 2002/11/14 22:43:49 heimbach Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_CALC_UVFLOW
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_UVFLOW(
     U     uTrans, vTrans,
     I     bi, bj, k, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE GMREDI_CALC_UVFLOW
C     |   Add GM-bolus flow to Eulerian horizontal transport
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: tile indices
C     k      :: level index
C     myThid :: thread number
C     uTrans :: zonal volume transport (updated)
C     vTrans :: meridional volume transport (updated)
      INTEGER bi, bj, k, myThid
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j :: loop indices
      INTEGER i, j
      INTEGER kp1
      _RL maskp1
CEOP

      IF (GM_AdvForm .AND. .NOT.GM_AdvSeparate) THEN

       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         uTrans(i,j) = uTrans(i,j)
     &          + dyG(i,j,bi,bj)*( GM_PsiX(i,j,kp1,bi,bj)*maskp1
     &                            -GM_PsiX(i,j,k,bi,bj) )
     &                          *maskW(i,j,k,bi,bj)
         vTrans(i,j) = vTrans(i,j)
     &          + dxG(i,j,bi,bj)*( GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &                            -GM_PsiY(i,j,k,bi,bj) )
     &                          *maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
