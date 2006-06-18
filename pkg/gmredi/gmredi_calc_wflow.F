C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_calc_wflow.F,v 1.4 2006/06/18 23:28:17 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_CALC_WFLOW
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_WFLOW(
     U                       wFld, rTrans,
     I                       k, bi, bj, myThid )
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
C     wFld   :: vertical volume transport (updated)
C     rTrans :: vertical volume transport (updated)
C     k      :: level index
C     bi,bj  :: tile indices
C     myThid :: thread number
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj, myThid

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j :: loop indices
      INTEGER i, j
      _RL delPsi
CEOP

      IF (GM_AdvForm .AND. .NOT.GM_AdvSeparate) THEN

       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         delPsi = ( dyG(i+1,j,bi,bj)*GM_PsiX(i+1,j,k,bi,bj)
     &             -dyG( i ,j,bi,bj)*GM_PsiX( i ,j,k,bi,bj)
     &             +dxG(i,j+1,bi,bj)*GM_PsiY(i,j+1,k,bi,bj)
     &             -dxG(i, j ,bi,bj)*GM_PsiY(i, j ,k,bi,bj)
     &            )*maskC(i,j,k,bi,bj)
         wFld(i,j)   = wFld(i,j)   + delPsi*recip_rA(i,j,bi,bj)
         rTrans(i,j) = rTrans(i,j) + delPsi
        ENDDO
       ENDDO

      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
