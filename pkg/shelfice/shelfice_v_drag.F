C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_v_drag.F,v 1.3 2006/08/14 17:16:10 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"
#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_V_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_V_DRAG(
     I        bi,bj,k,
     I        vFld, KE, KappaRV,
     O        vDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag due to friction and the no-slip condition at the
C bottom of the shelf-ice (in analogy to bottom drag)
C \begin{equation*}
C G^v_{drag} = - ( r_b + C_D |v| + \frac{2}{\Delta r_c} ) v
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vFld                 :: meridional flow
C  KE                   :: Kinetic energy
C  KappaRV              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscFac
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vDragTerms           :: drag term
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_SHELFICE 
C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER I,J,kUp,kUpC,kTop
      _RL rdrckp1,maskUp
CEOP

C-  No-slip BCs impose a drag at top
      IF ( usingZCoords ) THEN
       kTop    = 1
       kUp     = k
       kUpC    = kUp
      ELSE
       kTop    = Nr
       kUp     = min(k+1,kTop)
       kUpC    = kUp
      ENDIF	
      rdrckp1=recip_drC(kUpC)
      viscFac=0.
      IF (no_slip_shelfice) viscFac=1.
      IF (k.EQ.kUp) rdrckp1=recip_drF(k)
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        maskUp=_maskS(i,j,kUp,bi,bj)
CML        IF (k.EQ.kTop) maskUp=0.
        vDragTerms(i,j)=
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &   *( 
     &      2.*KappaRV(i,j,kUpC)*rdrckp1*viscFac
     &     + bottomDragLinear
     &    )*(1.-maskUp)*vFld(i,j)
        IF ( (KE(i,j)+KE(i,j-1)) .NE. 0. ) THEN
         vDragTerms(i,j)=vDragTerms(i,j)
     &    -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *bottomDragQuadratic*sqrt(KE(i,j)+KE(i,j-1))
     &    *(1.-maskUp)*vFld(i,j)
        ENDIF
       ENDDO
      ENDDO

CML#ifdef ALLOW_DIAGNOSTICS
CML      IF (useDiagnostics) THEN
CML        CALL DIAGNOSTICS_FILL(vDragTerms,'VBotDrag',k,1,2,bi,bj,myThid)
CML      ENDIF
CML#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
