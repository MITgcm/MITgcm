#include "MOM_COMMON_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: MOM_U_BOTDRAG_COEFF

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_BOTDRAG_COEFF(
     I                 bi, bj, k, inp_KE,
     I                 uFld, vFld, kappaRU,
     U                 KE,
     O                 cDrag,
     I                 myIter, myThid )

C !DESCRIPTION:
C Compute bottom-drag coefficient (Cd) for U component momentum,
C   such that bottom stress: taux_{bot} = -Cd * U_{bot} * rUnit2mass ;
C include linear and quadratic bottom drag and friction (no-slip BC) at bottom

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
# include "MOM_VISC.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
#endif

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
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL KE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C  KE             :: Kinetic energy (output when inp_KE = F)
C  cDrag          :: bottom drag coefficient
      _RL cDrag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j            :: loop indices
      INTEGER i, j
      INTEGER kDown,kLowF,kBottom
      _RL viscFac, dragFac, uSq
      _RL recDrC
CEOP

C-  No-slip BCs impose a drag at bottom
      viscFac = 0.
      IF (no_slip_bottom) viscFac = 2.

c     DO k= k1,k2

       IF ( usingZCoords ) THEN
        kBottom = Nr
        kDown   = MIN(k+1,Nr)
        kLowF   = k+1
c       dragFac = mass2rUnit*rhoConst
c       dragFac = wUnit2rVel(k+1)
        dragFac = 1. _d 0
       ELSE
        kBottom = 1
        kDown   = MAX(k-1,1)
        kLowF   = k
        dragFac = mass2rUnit*rhoConst
c       dragFac = wUnit2rVel(k)
       ENDIF
       IF ( k.EQ.kBottom ) THEN
        recDrC = recip_drF(k)
       ELSE
        recDrC = recip_drC(kLowF)
       ENDIF

C--   Linear bottom drag contribution to cDrag:
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
          cDrag(i,j) = bottomDragLinear*dragFac
#ifdef ALLOW_BOTTOMDRAG_CONTROL
     &      + halfRL*( bottomDragFld(i-1,j,bi,bj)
     &               + bottomDragFld(i,j,bi,bj) )*dragFac
#endif
        ENDDO
       ENDDO

C--   Add friction at the bottom (no-slip BC) to cDrag:
       IF ( no_slip_bottom .AND. bottomVisc_pCell ) THEN
C-    bottom friction accounts for true distance (including hFac) to the bottom
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
           cDrag(i,j) = cDrag(i,j)
     &      + kappaRU(i,j,kLowF)*recDrC*viscFac
     &                          *_recip_hFacW(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ELSEIF ( no_slip_bottom ) THEN
C-    ignores partial-cell reduction of the distance to the bottom
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
           cDrag(i,j) = cDrag(i,j)
     &      + kappaRU(i,j,kLowF)*recDrC*viscFac
         ENDDO
        ENDDO
       ENDIF

C--   Add quadratic bottom drag to cDrag:
       IF ( selectBotDragQuadr.EQ.0 ) THEN
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
          IF ( (KE(i,j)+KE(i-1,j)).GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)
     &      + (
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
     &          bottomDragCoeffW(i,j,bi,bj)
#else
     &          bottomDragQuadratic
#endif
     &          )*SQRT(KE(i,j)+KE(i-1,j))*dragFac
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.EQ.1 ) THEN
C-    calculate locally velocity norm @ U.pt (local U & 4 V averaged)
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          uSq = uFld(i,j)*uFld(i,j)
     &     + ( (vFld(i-1, j )*vFld(i-1, j )*hFacS(i-1, j ,k,bi,bj)
     &         +vFld( i , j )*vFld( i , j )*hFacS( i , j ,k,bi,bj))
     &       + (vFld(i-1,j+1)*vFld(i-1,j+1)*hFacS(i-1,j+1,k,bi,bj)
     &         +vFld( i ,j+1)*vFld( i ,j+1)*hFacS( i ,j+1,k,bi,bj))
     &       )*recip_hFacW(i,j,k,bi,bj)*0.25 _d 0
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)
     &      + (
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
     &          bottomDragCoeffW(i,j,bi,bj)
#else
     &          bottomDragQuadratic
#endif
     &          )*SQRT(uSq)*dragFac
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.EQ.2 ) THEN
C-    same as above but using wet-point method to average 4 V
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          uSq = ( hFacS(i-1, j ,k,bi,bj) + hFacS( i , j ,k,bi,bj) )
     &        + ( hFacS(i-1,j+1,k,bi,bj) + hFacS( i ,j+1,k,bi,bj) )
          IF ( uSq.GT.zeroRL ) THEN
           uSq = uFld(i,j)*uFld(i,j)
     &      + ( (vFld(i-1, j )*vFld(i-1, j )*hFacS(i-1, j ,k,bi,bj)
     &          +vFld( i , j )*vFld( i , j )*hFacS( i , j ,k,bi,bj))
     &        + (vFld(i-1,j+1)*vFld(i-1,j+1)*hFacS(i-1,j+1,k,bi,bj)
     &          +vFld( i ,j+1)*vFld( i ,j+1)*hFacS( i ,j+1,k,bi,bj))
     &        )/uSq
          ELSE
           uSq = uFld(i,j)*uFld(i,j)
          ENDIF
          IF ( uSq.GT.zeroRL ) THEN
           cDrag(i,j) = cDrag(i,j)
     &      + (
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
     &          bottomDragCoeffW(i,j,bi,bj)
#else
     &          bottomDragQuadratic
#endif
     &          )*SQRT(uSq)*dragFac
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.NE.-1 ) THEN
        STOP 'MOM_U_BOTDRAG_COEFF: invalid selectBotDragQuadr value'
       ENDIF

C--   Apply bottom mask (i.e., zero except at bottom):
       IF ( k.EQ.kBottom ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
           cDrag(i,j) = cDrag(i,j)*_maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
           cDrag(i,j) = cDrag(i,j)*_maskW(i,j,k,bi,bj)
     &                * ( oneRS -_maskW(i,j,kDown,bi,bj) )
         ENDDO
        ENDDO
       ENDIF

C-    end k loop
c     ENDDO

      RETURN
      END
