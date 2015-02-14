C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_u_drag.F,v 1.12 2015/02/14 21:58:05 jmc Exp $
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
      INTEGER i,j,kUpC,kTop
      _RL viscFac, uSq
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
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uDragTerms(i,j) =
     &      - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      * kappaRU(i,j,kUpC)*rdrckp1*viscFac
     &      * uFld(i,j)
         ELSE
           uDragTerms(i,j) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           uDragTerms(i,j) = 0. _d 0
         ENDDO
       ENDDO
      ENDIF
      IF ( no_slip_shelfice .AND. bottomVisc_pCell ) THEN
C-    friction accounts for true distance (including hFac) to the surface
       DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
           uDragTerms(i,j) = uDragTerms(i,j)
     &                     * _recip_hFacW(i,j,k,bi,bj)
         ENDDO
       ENDDO
      ENDIF

C--   Add Linear drag:
      IF ( SHELFICEDragLinear.NE.zeroRL ) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uDragTerms(i,j) = uDragTerms(i,j)
     &      - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragLinear
     &      * uFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Add quadratic drag
      IF ( SHELFICEselectDragQuadr.EQ.0 ) THEN
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uSq = KE(i,j)+KE(i-1,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           uDragTerms(i,j) = uDragTerms(i,j)
     &      - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(uSq)
     &      * uFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( SHELFICEselectDragQuadr.EQ.1 ) THEN
C-    calculate locally velocity norm @ U.pt (local U & 4 V averaged)
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uSq = uFld(i,j)*uFld(i,j)
     &       + ( (vFld(i-1, j )*vFld(i-1, j )*hFacS(i-1, j ,k,bi,bj)
     &           +vFld( i , j )*vFld( i , j )*hFacS( i , j ,k,bi,bj))
     &         + (vFld(i-1,j+1)*vFld(i-1,j+1)*hFacS(i-1,j+1,k,bi,bj)
     &           +vFld( i ,j+1)*vFld( i ,j+1)*hFacS( i ,j+1,k,bi,bj))
     &         )*recip_hFacW(i,j,k,bi,bj)*0.25 _d 0
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           uDragTerms(i,j) = uDragTerms(i,j)
     &      - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(uSq)
     &      * uFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( SHELFICEselectDragQuadr.EQ.2 ) THEN
C-    same as above but using wet-point method to average 4 V
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uSq = ( hFacS(i-1, j ,k,bi,bj) + hFacS( i , j ,k,bi,bj) )
     &         + ( hFacS(i-1,j+1,k,bi,bj) + hFacS( i ,j+1,k,bi,bj) )
           IF ( uSq.GT.zeroRL ) THEN
            uSq = uFld(i,j)*uFld(i,j)
     &        +( (vFld(i-1, j )*vFld(i-1, j )*hFacS(i-1, j ,k,bi,bj)
     &           +vFld( i , j )*vFld( i , j )*hFacS( i , j ,k,bi,bj))
     &         + (vFld(i-1,j+1)*vFld(i-1,j+1)*hFacS(i-1,j+1,k,bi,bj)
     &           +vFld( i ,j+1)*vFld( i ,j+1)*hFacS( i ,j+1,k,bi,bj))
     &         )/uSq
           ELSE
            uSq = uFld(i,j)*uFld(i,j)
           ENDIF
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           uDragTerms(i,j) = uDragTerms(i,j)
     &      - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      * SHELFICEDragQuadratic*SQRT(uSq)
     &      * uFld(i,j)
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND.
     &     ( no_slip_shelfice .OR. SHELFICEDragLinear.NE.zeroRL
     &                        .OR. SHELFICEselectDragQuadr.GE.0 )
     &   ) THEN
        CALL DIAGNOSTICS_FILL(uDragTerms,'SHIUDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
