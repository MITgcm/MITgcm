C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_bottomdrag.F,v 1.17 2015/01/04 16:16:32 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: MOM_U_BOTTOMDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_BOTTOMDRAG(
     I        bi, bj, k,
     I        uFld, vFld, KE, kappaRU,
     O        uDragTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the drag due to friction and the no-slip condition at bottom:
C \begin{equation*}
C G^u_{drag} = - \frac{1}{\Delta r_f} ( r_b + C_D |v| + \frac{2}{\Delta r_c} ) u
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

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,kDown,kLowF,kBottom
      _RL viscFac, dragFac, uSq
      _RL recDrC
      _RL recDrF_bot(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C-  No-slip BCs impose a drag at bottom
      viscFac = 0.
      IF (no_slip_bottom) viscFac = 2.
      IF ( usingZCoords ) THEN
       kBottom = Nr
       kDown   = MIN(k+1,Nr)
       kLowF   = k+1
c      dragFac = mass2rUnit*rhoConst
c      dragFac = wUnit2rVel(k+1)
       dragFac = 1. _d 0
      ELSE
       kBottom = 1
       kDown   = MAX(k-1,1)
       kLowF   = k
       dragFac = mass2rUnit*rhoConst
c      dragFac = wUnit2rVel(k)
      ENDIF
      IF ( k.EQ.kBottom ) THEN
       recDrC = recip_drF(k)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          recDrF_bot(i,j) = _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
        ENDDO
       ENDDO
      ELSE
       recDrC = recip_drC(kLowF)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          recDrF_bot(i,j) = _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &                    * ( 1. _d 0 -_maskW(i,j,kDown,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

C--   Linear bottom drag:
      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
         uDragTerms(i,j) =
     &    - recDrF_bot(i,j)
     &    *( bottomDragLinear*dragFac
#ifdef ALLOW_BOTTOMDRAG_CONTROL
     &     + halfRL*( bottomDragFld(i-1,j,bi,bj)
     &              + bottomDragFld(i,j,bi,bj) )*dragFac
#endif
     &     )*uFld(i,j)
       ENDDO
      ENDDO

C--   Add friction at the bottom (no-slip BC)
      IF ( no_slip_bottom .AND. bottomVisc_pCell ) THEN
C-    bottom friction accounts for true distance (including hFac) to the bottom
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         uDragTerms(i,j) = uDragTerms(i,j)
     &    - recDrF_bot(i,j)
     &    *( kappaRU(i,j,kLowF)*recDrC*viscFac
     &                         *_recip_hFacW(i,j,k,bi,bj)
     &     )*uFld(i,j)
        ENDDO
       ENDDO
      ELSEIF ( no_slip_bottom ) THEN
C-    ignores partial-cell reduction of the distance to the bottom
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         uDragTerms(i,j) = uDragTerms(i,j)
     &    - recDrF_bot(i,j)
     &    *( kappaRU(i,j,kLowF)*recDrC*viscFac
     &     )*uFld(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Add quadratic bottom drag
      IF ( selectBotDragQuadr.EQ.0 ) THEN
C-    average grid-cell-center KE to get velocity norm @ U.pt
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF ( (KE(i,j)+KE(i-1,j)) .GT. 0. ) THEN
          uDragTerms(i,j) = uDragTerms(i,j)
     &     - recDrF_bot(i,j)
     &      *bottomDragQuadratic*SQRT(KE(i,j)+KE(i-1,j))*dragFac
     &      *uFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ELSEIF ( selectBotDragQuadr.EQ.1 ) THEN
C-    calculate locally velocity norm @ U.pt (local U & 4 V averaged)
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         uSq = uFld(i,j)*uFld(i,j)
     &       + ( (vFld(i-1, j )*vFld(i-1, j )*hFacS(i-1, j ,k,bi,bj)
     &           +vFld( i , j )*vFld( i , j )*hFacS( i , j ,k,bi,bj))
     &         + (vFld(i-1,j+1)*vFld(i-1,j+1)*hFacS(i-1,j+1,k,bi,bj)
     &           +vFld( i ,j+1)*vFld( i ,j+1)*hFacS( i ,j+1,k,bi,bj))
     &         )*recip_hFacW(i,j,k,bi,bj)*0.25 _d 0
         IF ( uSq.GT.zeroRL ) THEN
          uDragTerms(i,j) = uDragTerms(i,j)
     &     - recDrF_bot(i,j)
     &      *bottomDragQuadratic*SQRT(uSq)*dragFac
     &      *uFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ELSEIF ( selectBotDragQuadr.EQ.2 ) THEN
C-    same as above but using wet-point method to average 4 V
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         uSq = ( hFacS(i-1, j ,k,bi,bj) + hFacS( i , j ,k,bi,bj) )
     &       + ( hFacS(i-1,j+1,k,bi,bj) + hFacS( i ,j+1,k,bi,bj) )
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
         IF ( uSq.GT.zeroRL ) THEN
          uDragTerms(i,j) = uDragTerms(i,j)
     &     - recDrF_bot(i,j)
     &      *bottomDragQuadratic*SQRT(uSq)*dragFac
     &      *uFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ELSEIF ( selectBotDragQuadr.NE.-1 ) THEN
        STOP 'MOM_U_BOTTOMDRAG: invalid selectBotDragQuadr value'
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(uDragTerms,'UBotDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
