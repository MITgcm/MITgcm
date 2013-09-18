C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_calc_uvflow.F,v 1.7 2013/09/18 22:04:27 m_bates Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_CALC_UVFLOW
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_UVFLOW(
     U                       uFld, vFld, uTrans, vTrans,
     I                       k, bi, bj, myThid )
C     !DESCRIPTION:
C     Add GM-bolus flow to Eulerian horizontal transport.

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#ifdef ALLOW_EDDYPSI
# include "DYNVARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld   :: zonal      velocity (updated)
C     vFld   :: meridional velocity (updated)
C     uTrans :: zonal      volume transport (updated)
C     vTrans :: meridional volume transport (updated)
C     k      :: level index
C     bi,bj  :: tile indices
C     myThid :: thread number
      INTEGER k, bi, bj, myThid
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL delPsi
#ifdef ALLOW_EDDYPSI
      _RL ustar, vstar
#endif
CEOP

      IF (GM_AdvForm .AND. .NOT.GM_AdvSeparate
     & .AND. .NOT.GM_InMomAsStress) THEN

       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         delPsi = GM_PsiX(i,j,kp1,bi,bj)*maskp1
     &          - GM_PsiX(i,j, k, bi,bj)
         uFld(i,j)   = uFld(i,j)
     &               + delPsi*recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
         uTrans(i,j) = uTrans(i,j)
     &               + dyG(i,j,bi,bj)*delPsi*maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         delPsi = GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &          - GM_PsiY(i,j, k, bi,bj)
         vFld(i,j)   = vFld(i,j)
     &               + delPsi*recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
         vTrans(i,j) = vTrans(i,j)
     &               + dxG(i,j,bi,bj)*delPsi*maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

#ifdef ALLOW_EDDYPSI
      ELSEIF(GM_AdvForm .AND. .NOT.GM_AdvSeparate
     & .AND. GM_InMomAsStress) THEN
C       Calculate the mean velocity from the residual and bolus
       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         delPsi = GM_PsiX(i,j,kp1,bi,bj)*maskp1
     &          - GM_PsiX(i,j, k, bi,bj)
         ustar = delPsi*recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
         uMean(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) - ustar
        ENDDO
       ENDDO
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         delPsi = GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &          - GM_PsiY(i,j, k, bi,bj)
         vstar  = delPsi*recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
         vMean(i,j,k,bi,bj) = vVel(i,j,k,bi,bj) - vstar
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
       CALL DIAGNOSTICS_FILL(uMean, 'UVELMEAN',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(vMean, 'VVELMEAN',0,Nr,0,1,1,myThid)
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_EDDYPSI */       
      ENDIF
#endif /* GM_BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
