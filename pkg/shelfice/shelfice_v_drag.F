C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_v_drag.F,v 1.11 2015/02/14 21:58:05 jmc Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_V_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_V_DRAG(
     I        bi, bj, k,
     I        uFld, vFld, KE, kappaRV,
     O        vDragTerms,
     I        myThid )

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
C  uFld                 :: zonal flow
C  vFld                 :: meridional flow
C  KE                   :: Kinetic energy
C  kappaRV              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vDragTerms           :: drag term
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER i,j,kUpC,kTop
      _RL viscFac, vSq
      _RL rdrckp1
CEOP

C-  No-slip BCs impose a drag at top
      IF ( usingZCoords ) THEN
       kTop    = 1
       kUpC    = k
      ELSE
       kTop    = Nr
       kUpC    = k+1
      ENDIF
      rdrckp1=recip_drC(kUpC)
CML      IF (k.EQ.kTop) rdrckp1=recip_drF(k)
      viscFac=0.
      IF (no_slip_shelfice) viscFac=2.

C--   Friction at the bottom of ice-shelf (no-slip BC)
      IF ( no_slip_shelfice ) THEN
C-    ignores partial-cell reduction of the distance to the surface
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i,j-1,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           vDragTerms(i,j) =
     &      - _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      * kappaRV(i,j,kUpC)*rdrckp1*viscFac
     &      * vFld(i,j)
         ELSE
           vDragTerms(i,j) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           vDragTerms(i,j) = 0. _d 0
         ENDDO
       ENDDO
      ENDIF
      IF ( no_slip_shelfice .AND. bottomVisc_pCell ) THEN
C-    friction accounts for true distance (including hFac) to the surface
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
           vDragTerms(i,j) = vDragTerms(i,j)
     &                     * _recip_hFacS(i,j,k,bi,bj)
         ENDDO
       ENDDO
      ENDIF

C--   Add Linear drag:
      IF ( SHELFICEDragLinear.NE.zeroRL ) THEN
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i,j-1,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           vDragTerms(i,j) = vDragTerms(i,j)
     &      - _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragLinear
     &      * vFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Add quadratic drag
      IF ( SHELFICEselectDragQuadr.EQ.0 ) THEN
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          vSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i,j-1,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           vSq = KE(i,j)+KE(i,j-1)
          ENDIF
          IF ( vSq.GT.zeroRL ) THEN
           vDragTerms(i,j) = vDragTerms(i,j)
     &      - _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(vSq)
     &      * vFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( SHELFICEselectDragQuadr.EQ.1 ) THEN
C-    calculate locally velocity norm @ U.pt (local U & 4 V averaged)
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          vSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i,j-1,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           vSq = vFld(i,j)*vFld(i,j)
     &       + ( (uFld( i ,j-1)*uFld( i ,j-1)*hFacW( i ,j-1,k,bi,bj)
     &           +uFld( i , j )*uFld( i , j )*hFacW( i , j ,k,bi,bj))
     &         + (uFld(i+1,j-1)*uFld(i+1,j-1)*hFacW(i+1,j-1,k,bi,bj)
     &           +uFld(i+1, j )*uFld(i+1, j )*hFacW(i+1, j ,k,bi,bj))
     &         )*recip_hFacS(i,j,k,bi,bj)*0.25 _d 0
          ENDIF
          IF ( vSq.GT.zeroRL ) THEN
           vDragTerms(i,j) = vDragTerms(i,j)
     &      - _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(vSq)
     &      * vFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( SHELFICEselectDragQuadr.EQ.2 ) THEN
C-    same as above but using wet-point method to average 4 V
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          vSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i,j-1,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           vSq = ( hFacW( i ,j-1,k,bi,bj) + hFacW( i , j ,k,bi,bj) )
     &         + ( hFacW(i+1,j-1,k,bi,bj) + hFacW(i+1, j ,k,bi,bj) )
           IF ( vSq.GT.zeroRL ) THEN
            vSq = vFld(i,j)*vFld(i,j)
     &        +( (uFld( i ,j-1)*uFld( i ,j-1)*hFacW( i ,j-1,k,bi,bj)
     &           +uFld( i , j )*uFld( i , j )*hFacW( i , j ,k,bi,bj))
     &         + (uFld(i+1,j-1)*uFld(i+1,j-1)*hFacW(i+1,j-1,k,bi,bj)
     &           +uFld(i+1, j )*uFld(i+1, j )*hFacW(i+1, j ,k,bi,bj))
     &         )/vSq
           ELSE
            vSq = vFld(i,j)*vFld(i,j)
           ENDIF
          ENDIF
          IF ( vSq.GT.zeroRL ) THEN
           vDragTerms(i,j) = vDragTerms(i,j)
     &      - _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(vSq)
     &      * vFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND.
     &     ( no_slip_shelfice .OR. SHELFICEDragLinear.NE.zeroRL
     &                        .OR. SHELFICEselectDragQuadr.GE.0 )
     &   ) THEN
        CALL DIAGNOSTICS_FILL(vDragTerms,'SHIVDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
