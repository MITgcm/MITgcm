C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_u_drag.F,v 1.11 2015/01/04 00:01:12 jmc Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_U_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_U_DRAG(
     I        bi, bj, k,
     I        uFld, vFld, KE, kappaRU,
     O        uDragTerms,
     I        myThid )

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
C  vFld                 :: meridional flow
C  KE                   :: Kinetic energy
C  kappaRU              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER I,J,kUp,kUpC,kTop
      _RL viscFac
      _RL rdrckp1,maskUp
CEOP

C-  No-slip BCs impose a drag at top
      IF ( usingZCoords ) THEN
       kTop    = 1
       kUp     = max(k-1,kTop)
       kUpC    = k
      ELSE
       kTop    = Nr
       kUp     = min(k+1,kTop)
       kUpC    = k+1
      ENDIF
      rdrckp1=recip_drC(kUpC)
CML      IF (k.EQ.kTop) rdrckp1=recip_drF(k)
      viscFac=0.
      IF (no_slip_shelfice) viscFac=2.
      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
        maskUp=_maskW(i,j,kUp,bi,bj)
CML        IF (k.EQ.kTop) maskUp=0.
        uDragTerms(i,j)=
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *(
     &     - kappaRU(i,j,kUpC)*rdrckp1*viscFac
     &     + SHELFICEDragLinear
     &    )*(1.-maskUp)*uFld(i,j)
        IF ( (KE(i,j)+KE(i-1,j)) .GT. 0. ) THEN
         uDragTerms(i,j)=uDragTerms(i,j)
     &    -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    *SHELFICEDragQuadratic*SQRT(KE(i,j)+KE(i-1,j))
     &    *(1.-maskUp)*uFld(i,j)
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(uDragTerms,'SHIUDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
