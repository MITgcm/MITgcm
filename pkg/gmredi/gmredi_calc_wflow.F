C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_calc_wflow.F,v 1.3 2004/03/29 03:33:51 edhill Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_CALC_WFLOW
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_WFLOW(
     U     rTrans,
     I     bi, bj, k, myThid)
C     !DESCRIPTION:
C     Add GM-bolus flow to Eulerian vertical transport.

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
C     rTrans :: vertical volume transport (updated)
      INTEGER bi, bj, k, myThid
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j :: loop indices
      INTEGER i, j
CEOP

      IF (GM_AdvForm .AND. .NOT.GM_AdvSeparate) THEN

       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1    
         rTrans(i,j) = rTrans(i,j)
     &   +( dyG(i+1,j,bi,bj)*GM_PsiX(i+1,j,k,bi,bj)
     &     -dyG( i ,j,bi,bj)*GM_PsiX( i ,j,k,bi,bj)
     &     +dxG(i,j+1,bi,bj)*GM_PsiY(i,j+1,k,bi,bj)
     &     -dxG(i, j ,bi,bj)*GM_PsiY(i, j ,k,bi,bj)
     &    )*maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
