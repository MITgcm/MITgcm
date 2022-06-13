#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_U_DRAG_COEFF

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_U_DRAG_COEFF(
     I        bi, bj, k, inp_KE,
     I        uFld, vFld, kappaRU,
     U        KE,
     O        cDrag,
     I        myIter, myThid )

C !DESCRIPTION:
C Calculates the drag coefficient due to friction and the no-slip condition
C at the bottom of the shelf-ice (in analogy to bottom drag)
C such as the ice-shelf stress: taux_{ice} = -Cd * U_{top} * rUnit2mass ;

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj          :: tile indices
C  k              :: vertical level to process
C  inp_KE         :: =T : KE is provided as input ; =F : to compute here
C  uFld           :: velocity, zonal component
C  vFld           :: velocity, meridional component
C  kappaRU        :: vertical viscosity
C  KE             :: Kinetic energy (input when inp_KE = T)
C  myIter         :: current iteration number
C  myThid         :: my Thread Id number
      INTEGER bi, bj, k
      LOGICAL inp_KE
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C  KE             :: Kinetic energy (output when inp_KE = F)
C  cDrag          :: bottom drag coefficient
      _RL cDrag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C  i,j            :: loop indices
C  Kp1            :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER i, j
      INTEGER kUpC, kTop
      _RL viscFac, dragFac, uSq
      _RL rDrCkp1
CEOP

C-  No-slip BCs impose a drag at top
      IF ( usingZCoords ) THEN
       kTop    = 1
       kUpC    = k
c      dragFac = mass2rUnit*rhoConst
       dragFac = 1. _d 0
      ELSE
       kTop    = Nr
       kUpC    = k+1
       dragFac = mass2rUnit*rhoConst
      ENDIF
      rDrCkp1 = recip_drC(kUpC)
CML      IF (k.EQ.kTop) rDrCkp1=recip_drF(k)
      viscFac = 0. _d 0
      IF (no_slip_shelfice) viscFac = 2. _d 0

C--   Initialise drag-coeff
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         cDrag(i,j) = 0. _d 0
       ENDDO
      ENDDO

C--   Friction at the bottom of ice-shelf (no-slip BC)
      IF ( no_slip_shelfice .AND. bottomVisc_pCell ) THEN
C-    friction accounts for true distance (including hFac) to the surface
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           cDrag(i,j) = cDrag(i,j)
     &                + kappaRU(i,j,kUpC)*rDrCkp1*viscFac
     &                        * _recip_hFacW(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ELSEIF ( no_slip_shelfice ) THEN
C-    ignores partial-cell reduction of the distance to the surface
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           cDrag(i,j) = cDrag(i,j)
     &                + kappaRU(i,j,kUpC)*rDrCkp1*viscFac
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Add Linear drag contribution:
      IF ( SHELFICEDragLinear.NE.zeroRL ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
          cDrag(i,j) = cDrag(i,j) + SHELFICEDragLinear*dragFac
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Add quadratic drag
      IF ( SHELFICEselectDragQuadr.EQ.0 ) THEN
       IF ( .NOT.inp_KE ) THEN
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx-1
            KE(i,j) = 0.25*(
     &          ( uFld( i , j )*uFld( i , j )*_hFacW(i,j,k,bi,bj)
     &           +uFld(i+1, j )*uFld(i+1, j )*_hFacW(i+1,j,k,bi,bj) )
     &        + ( vFld( i , j )*vFld( i , j )*_hFacS(i,j,k,bi,bj)
     &           +vFld( i ,j+1)*vFld( i ,j+1)*_hFacS(i,j+1,k,bi,bj) )
     &                     )*_recip_hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.EQ.MAX( kTopC(i-1,j,bi,bj),kTopC(i,j,bi,bj) ) ) THEN
           uSq = KE(i,j)+KE(i-1,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)
     &                + shiDragQuadFld(i,j,bi,bj)*SQRT(uSq)*dragFac
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
           cDrag(i,j) = cDrag(i,j)
     &                + shiDragQuadFld(i,j,bi,bj)*SQRT(uSq)*dragFac
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
           cDrag(i,j) = cDrag(i,j)
     &                + shiDragQuadFld(i,j,bi,bj)*SQRT(uSq)*dragFac
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND.
     &     ( no_slip_shelfice .OR. SHELFICEDragLinear.NE.zeroRL
     &                        .OR. SHELFICEselectDragQuadr.GE.0 )
     &   ) THEN
       IF ( selectImplicitDrag.EQ.0 ) THEN
C-     Explicit case: diagnose directly the Ice-Shelf stress
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           shelficeDragU(i,j,bi,bj) = shelficeDragU(i,j,bi,bj)
     &                              - cDrag(i,j)*uFld(i,j)*rUnit2mass
          ENDDO
         ENDDO
       ELSE
C-     Implicit case: save drag-coeff for diagnostics of the Ice-Shelf stress
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           shelficeDragU(i,j,bi,bj) = shelficeDragU(i,j,bi,bj)
     &                              + cDrag(i,j)*rUnit2mass
          ENDDO
         ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
