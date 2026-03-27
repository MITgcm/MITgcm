#include "ICEBERG_OPTIONS.h"

CBOP
C !ROUTINE: ICEBERG_U_DRAG_COEFF

C !INTERFACE: ==========================================================
      SUBROUTINE ICEBERG_U_DRAG_COEFF(
     I        bi, bj, k, inp_KE,
     I        uFld, vFld, kappaRU,
     U        KE,
     O        cDrag,
     I        myIter, myThid )

C !DESCRIPTION:
C Calculates the drag coefficient due to friction and the no-slip condition
C at the bottom of the iceberg (in analogy to bottom drag)
C such as the iceberg stress: taux_{ice} = -Cd * U_{top} * rUnit2mass ;

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

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

#ifdef ALLOW_ICEBERG
C !LOCAL VARIABLES : ====================================================
C  i,j            :: loop indices
C  Kp1            :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER i, j
      INTEGER kUpC, kTop
      _RL viscFac, dragFac, uSq
      _RL rDrCkp1
      _RL fillFactor (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C-  No-slip BCs impose a drag at top
      IF ( usingZCoords ) THEN
       kTop    = 1
       kUpC    = k
c      dragFac = mass2rUnit*rhoConst !equals about 1 anyway
       dragFac = 1. _d 0
      ELSE
       kTop    = Nr
       kUpC    = k+1
       dragFac = mass2rUnit*rhoConst
      ENDIF
      rDrCkp1 = recip_drC(kUpC)
CML      IF (k.EQ.kTop) rDrCkp1=recip_drF(k)
      viscFac = 0. _d 0

C--   Initialise drag-coeff, fill factor
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         cDrag(i,j) = 0. _d 0
         fillFactor(i,j) = 0. _d 0
       ENDDO
      ENDDO

C     Compute fill factor
      IF( brg_SelectFill.EQ.1) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fillFactor(i,j) = 1.0 - _hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ELSEIF( brg_SelectFill.EQ.2 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fillFactor(i,j) = 1.0 - _hFacC(i,j,k,bi,bj)**2
         ENDDO
        ENDDO
      ELSEIF( brg_SelectFill.EQ.3 ) THEN  
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fillFactor(i,j) = 1.0 - _hFacC(i,j,k,bi,bj)**4
         ENDDO
        ENDDO
      ENDIF !fillFactor 

C--   Add drag contribution: 
      IF ( brg_SelectDrag.EQ.1 ) THEN !linear
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         IF ( k.LE.
     &       MAX( brg_kBotC(i-1,j,bi,bj),brg_kBotC(i,j,bi,bj) ) ) THEN
          cDrag(i,j) = cDrag(i,j) 
     &       + brg_DragForm * dragFac *  
     &                   drF(k) * fillFactor(i,j)
         ENDIF
        ENDDO
       ENDDO
      ELSEIF( brg_SelectDrag.EQ.2 ) THEN !quadratic
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
       ENDIF !.not.inp_KE
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.LE.
     &         MAX( brg_kBotC(i-1,j,bi,bj),brg_kBotC(i,j,bi,bj) ) ) THEN
           uSq = KE(i,j)+KE(i-1,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)  
     &            + brg_DragForm * dragFac *  
     &                   SQRT(uSq) * 
     &                   drF(k) *
     &                   fillFactor(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( brg_SelectDrag.EQ.3 ) THEN !nonlinear
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
       ENDIF !.not.inp_KE
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.LE.
     &         MAX( brg_kBotC(i-1,j,bi,bj),brg_kBotC(i,j,bi,bj) ) ) THEN
           uSq = KE(i,j)+KE(i-1,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)  
     &            + brg_DragForm * dragFac *  
     &                   SQRT(uSq)**(.75*_hFacC(i,j,k,bi,bj)) * 
     &                   drF(k) *
     &                   fillFactor(i,j)
          ENDIF
        ENDDO
       ENDDO
      ELSEIF ( brg_SelectDrag.EQ.4 ) THEN !nonlinear
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
       ENDIF !.not.inp_KE
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          uSq = 0. _d 0
          IF ( k.LE.
     &     MAX( brg_kBotC(i-1,j,bi,bj),brg_kBotC(i,j,bi,bj) ) ) THEN
           uSq = KE(i,j)+KE(i-1,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)  
     &            + brg_DragForm * dragFac *  
     &                   SQRT(uSq)**(.75*_hFacC(i,j,k,bi,bj)**3) * 
     &                   drF(k) *
     &                   fillFactor(i,j)
          ENDIF
        ENDDO
       ENDDO
      ENDIF !selectDrag 

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       IF ( selectImplicitDrag.EQ.0 ) THEN !this is default
C-     Explicit case: diagnose directly the iceberg stress, only implemented case
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           brg_DragU(i,j,k,bi,bj) = - cDrag(i,j)
     &                                  * uFld(i,j) * rUnit2mass
          ENDDO
         ENDDO
       ENDIF
       CALL DIAGNOSTICS_FILL_RS( brg_DragU, 'BRG_TauX',
     &                          0, Nr, 1, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL_RS( hFacC, 'BRGhFacC',
     &                          0, Nr, 1, bi, bj, myThid )
      ENDIF !useDiagnostics+ 
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEBERG */

      RETURN
      END
