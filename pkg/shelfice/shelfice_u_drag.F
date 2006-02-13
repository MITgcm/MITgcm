C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_u_drag.F,v 1.2 2006/02/13 12:49:21 mlosch Exp $
C $Name:  $

#include "SHELFICE.h"
#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_U_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_U_DRAG(
     I        bi,bj,k,
     I        uFld, KE, KappaRU,
     O        uDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag due to friction and the no-slip condition at the 
C bottom of the shelf-ice (in analogy to bottom drag)
C \begin{equation*}
C G^u_{drag} = - ( r_b + C_D |v| + \frac{2}{\Delta r_c} ) u
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
C  uFld                 :: zonal flow
C  KE                   :: Kinetic energy
C  KappaRU              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER I,J,kUp,kUpC,kTop
      _RL viscFac
      _RL rdrckp1,maskUp
CEOP

C-  No-slip BCs impose a drag at bottom
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
      IF (k.EQ.kUp) rdrckp1=recip_drF(k)
      viscFac=0.
      IF (no_slip_shelfice) viscFac=1.
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        maskUp=_maskW(i,j,kUp,bi,bj)
CML        IF (k.EQ.kTop) maskUp=0.
        uDragTerms(i,j)=
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *( 
     &      2.*KappaRU(i,j,kUpC)*rdrckp1*viscFac
     &     + SHELFICEDragLinear
     &    )*(1.-maskUp)*uFld(i,j)
        IF ( (KE(i,j)+KE(i-1,j)) .NE. 0. ) THEN
         uDragTerms(i,j)=uDragTerms(i,j)
     &    -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    *SHELFICEDragQuadratic*sqrt(KE(i,j)+KE(i-1,j))
     &    *(1.-maskUp)*uFld(i,j)
        ENDIF
       ENDDO
      ENDDO

CML#ifdef ALLOW_DIAGNOSTICS
CML      IF (useDiagnostics) THEN
CML        CALL DIAGNOSTICS_FILL(uDragTerms,'UIceShelfDrag',k,1,2,bi,bj,
CML     &      myThid)
CML      ENDIF
CML#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
