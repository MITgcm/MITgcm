#include "MOM_COMMON_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: MOM_V_BOTDRAG_IMPL

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_BOTDRAG_IMPL(
     I                 uFld, vFld, kappaRV,
     O                 cDrag,
     I                 bi, bj, myIter, myThid )

C !DESCRIPTION:
C Add contribution from drag due to friction and the no-slip condition at bottom
C  to matrix main diagonal for implicit momentum solver
C \begin{equation*}
C C_{drag} = \frac{\Delta t}{\Delta r_f} (r_b + Cd |v| + \nu \frac{2}{\Delta r_c})
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  uFld           :: zonal flow
C  vFld           :: meridional flow
C  kappaRV        :: vertical viscosity
C  bi,bj          :: tile indices
C  myIter         :: current iteration number
C  myThid         :: thread number
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER bi, bj
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================
C  cDrag          :: drag contribution to matrix main diagnonal
      _RL cDrag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C !LOCAL VARIABLES: ====================================================
C  i,j,k          :: loop indices
C  KE             :: Kinetic energy
      INTEGER i,j,k
      INTEGER kDown,kLowF,kBottom
      _RL viscFac, dragFac, vSq
      _RL recDrC
      _RL recDrF_bot(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C-  No-slip BCs impose a drag at bottom
      viscFac = 0.
      IF (no_slip_bottom) viscFac = 2.

      DO k=1,Nr

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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           recDrF_bot(i,j) = _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
         ENDDO
        ENDDO
       ELSE
        recDrC = recip_drC(kLowF)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           recDrF_bot(i,j) = _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &                     * ( 1. _d 0 -_maskS(i,j,kDown,bi,bj) )
         ENDDO
        ENDDO
       ENDIF

C--   Linear bottom drag:
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
          cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &      *( bottomDragLinear*dragFac
#ifdef ALLOW_BOTTOMDRAG_CONTROL
     &       + halfRL*( bottomDragFld(i,j-1,bi,bj)
     &                + bottomDragFld(i,j,bi,bj) )*dragFac
#endif
     &       )*deltaTMom
        ENDDO
       ENDDO

C--   Add friction at the bottom (no-slip BC)
       IF ( no_slip_bottom .AND. bottomVisc_pCell ) THEN
C-    bottom friction accounts for true distance (including hFac) to the bottom
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &      *( kappaRV(i,j,kLowF)*recDrC*viscFac
     &                           *_recip_hFacS(i,j,k,bi,bj)
     &       )*deltaTMom
         ENDDO
        ENDDO
       ELSEIF ( no_slip_bottom ) THEN
C-    ignore partial-cell reduction of the distance to the bottom
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &      *( kappaRV(i,j,kLowF)*recDrC*viscFac
     &       )*deltaTMom
         ENDDO
        ENDDO
       ENDIF

C--   Add quadratic bottom drag
       IF ( selectBotDragQuadr.EQ.0 ) THEN
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          KE(i,j) = 0.25*(
     &        ( uFld( i , j ,k)*uFld( i , j ,k)*_hFacW(i,j,k,bi,bj)
     &         +uFld(i+1, j ,k)*uFld(i+1, j ,k)*_hFacW(i+1,j,k,bi,bj) )
     &      + ( vFld( i , j ,k)*vFld( i , j ,k)*_hFacS(i,j,k,bi,bj)
     &         +vFld( i ,j+1,k)*vFld( i ,j+1,k)*_hFacS(i,j+1,k,bi,bj) )
     &                   )*_recip_hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
C-    average grid-cell-center KE to get velocity norm @ V.pt
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          IF ( (KE(i,j)+KE(i,j-1)) .GT. 0. ) THEN
           cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &       *bottomDragQuadratic*SQRT(KE(i,j)+KE(i,j-1))*dragFac
     &       *deltaTMom
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.EQ.1 ) THEN
C-    calculate locally velocity norm @ V.pt (local V & 4 U averaged)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          vSq = vFld(i,j,k)*vFld(i,j,k)
     &     + ( (uFld( i ,j-1,k)*uFld( i ,j-1,k)*hFacW( i ,j-1,k,bi,bj)
     &         +uFld( i , j ,k)*uFld( i , j ,k)*hFacW( i , j ,k,bi,bj))
     &       + (uFld(i+1,j-1,k)*uFld(i+1,j-1,k)*hFacW(i+1,j-1,k,bi,bj)
     &         +uFld(i+1, j ,k)*uFld(i+1, j ,k)*hFacW(i+1, j ,k,bi,bj))
     &       )*recip_hFacS(i,j,k,bi,bj)*0.25 _d 0
          IF ( vSq.GT.zeroRL ) THEN
           cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &       *bottomDragQuadratic*SQRT(vSq)*dragFac
     &       *deltaTMom
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.EQ.2 ) THEN
C-    same as above but using wet-point method to average 4 U
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          vSq = ( hFacW( i ,j-1,k,bi,bj) + hFacW( i , j ,k,bi,bj) )
     &        + ( hFacW(i+1,j-1,k,bi,bj) + hFacW(i+1, j ,k,bi,bj) )
          IF ( vSq.GT.zeroRL ) THEN
           vSq = vFld(i,j,k)*vFld(i,j,k)
     &      +( (uFld( i ,j-1,k)*uFld( i ,j-1,k)*hFacW( i ,j-1,k,bi,bj)
     &         +uFld( i , j ,k)*uFld( i , j ,k)*hFacW( i , j ,k,bi,bj))
     &       + (uFld(i+1,j-1,k)*uFld(i+1,j-1,k)*hFacW(i+1,j-1,k,bi,bj)
     &         +uFld(i+1, j ,k)*uFld(i+1, j ,k)*hFacW(i+1, j ,k,bi,bj))
     &       )/vSq
          ELSE
           vSq = vFld(i,j,k)*vFld(i,j,k)
          ENDIF
          IF ( vSq.GT.zeroRL ) THEN
           cDrag(i,j,k) = cDrag(i,j,k)
     &      + recDrF_bot(i,j)
     &       *bottomDragQuadratic*SQRT(vSq)*dragFac
     &       *deltaTMom
          ENDIF
         ENDDO
        ENDDO
       ELSEIF ( selectBotDragQuadr.NE.-1 ) THEN
        STOP 'MOM_V_BOTDRAG_IMPL: invalid selectBotDragQuadr value'
       ENDIF

c#ifdef ALLOW_DIAGNOSTICS
c      IF (useDiagnostics) THEN
c        CALL DIAGNOSTICS_FILL(vDragTerms,'VBotDrag',k,1,2,bi,bj,myThid)
c      ENDIF
c#endif /* ALLOW_DIAGNOSTICS */

C-    end k loop
      ENDDO

      RETURN
      END
