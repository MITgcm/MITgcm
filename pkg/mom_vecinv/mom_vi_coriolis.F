#include "MOM_VECINV_OPTIONS.h"

CBOP
C !ROUTINE: MOM_VI_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_VI_CORIOLIS(
     I        bi, bj, k,
     I        uFld, vFld, hFacZ, r_hFacZ,
     O        uCoriolisTerm, vCoriolisTerm,
     I        myThid )

C !DESCRIPTION:
C  Calculates the 2 horizontal components of Coriolis acceleration

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C   bi, bj         :: current tile indices
C   k              :: current vertical level
C   uFld, vFld     :: local copy of horizontal velocity (u & v components)
C   hFacZ          :: hFac thickness factor at corner location
C   r_hFacZ        :: reciprocal hFac thickness factor at corner location
C   myThid         :: my Thread Id number
      INTEGER bi,bj,k
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C   uCoriolisTerm  :: Coriolis tendency for u-component momentum
C   vCoriolisTerm  :: Coriolis tendency for v-component momentum
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C   i, j           :: loop indices
C   uBarXY, vBarXY :: averaged (in X & Y direction) of u & v velocity
C   uBarYm, uBarYp :: u velocity, Y direction averaged (at i and i+1)
C   vBarXm, vBarXp :: v velocity, X direction averaged (at j and j+1)
C   epsil          :: small number
      INTEGER i,j
      _RL uBarXY, vBarXY
      _RL uBarYm, uBarYp, vBarXm, vBarXp
      _RS epsil
CEOP

      epsil = 1. _d -9

      IF ( selectCoriScheme .EQ. 0 ) THEN
C- Simple average, no hFac :
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj))
     &               )
         uCoriolisTerm(i,j)=
     &    +0.5*( fCoriG(i,j,bi,bj)+fCoriG(i,j+1,bi,bj)
     &         )*vBarXY*recip_dxC(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme .EQ. 1 ) THEN
C- Partial-cell generalization of the Wet-point average method :
C     (formerly called: useJamartWetPoints)
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXY=(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj)))
     &   / MAX( epsil,(_hFacS(i, j ,k,bi,bj)+_hFacS(i-1, j ,k,bi,bj))
     &               +(_hFacS(i,j+1,k,bi,bj)+_hFacS(i-1,j+1,k,bi,bj)) )
         uCoriolisTerm(i,j)=
     &    +0.5*( fCoriG(i,j,bi,bj)+fCoriG(i,j+1,bi,bj)
     &         )*vBarXY*recip_dxC(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
c     ELSEIF ( selectCoriScheme .EQ. 2 ) THEN
      ELSEIF ( selectCoriScheme .EQ. 2
     &    .OR. selectCoriScheme .EQ. 4 ) THEN
C- hFac weighted average :
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj))
     &               )
         uCoriolisTerm(i,j)=
     &    +0.5*( fCoriG(i,j,bi,bj)+fCoriG(i,j+1,bi,bj)
     &         )*vBarXY*recip_dxC(i,j,bi,bj)*_recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme .EQ. 3 ) THEN
C- Energy-conserving discretisation with hFac weighted average :
       DO j=1-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx
         vBarXm = halfRL *(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
         vBarXp = halfRL *(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
         uCoriolisTerm(i,j) = +0.5 _d 0
     &        *( vBarXm*fCoriG(i, j ,bi,bj)
     &          +vBarXp*fCoriG(i,j+1,bi,bj)
     &         )*recip_dxC(i,j,bi,bj)*_recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
        STOP 'MOM_VI_CORIOLIS: invalid selectCoriScheme'
      ENDIF

      IF ( selectCoriScheme .EQ. 0 ) THEN
C- Simple average, no hFac :
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         uBarXY=0.25*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj))
     &               )
         vCoriolisTerm(i,j)=
     &    -0.5*( fCoriG(i,j,bi,bj)+fCoriG(i+1,j,bi,bj)
     &         )*uBarXY*recip_dyC(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme .EQ. 1 ) THEN
C- Partial-cell generalization of the Wet-point average method :
C     (formerly called: useJamartWetPoints)
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         uBarXY=(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj)))
     &   / MAX( epsil,(_hFacW( i ,j,k,bi,bj)+_hFacW( i ,j-1,k,bi,bj))
     &               +(_hFacW(i+1,j,k,bi,bj)+_hFacW(i+1,j-1,k,bi,bj)) )
         vCoriolisTerm(i,j)=
     &    -0.5*( fCoriG(i,j,bi,bj)+fCoriG(i+1,j,bi,bj)
     &         )*uBarXY*recip_dyC(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
c     ELSEIF ( selectCoriScheme .EQ. 2 ) THEN
      ELSEIF ( selectCoriScheme .EQ. 2
     &    .OR. selectCoriScheme .EQ. 4 ) THEN
C- hFac weighted average :
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         uBarXY=0.25*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj))
     &               )
         vCoriolisTerm(i,j)=
     &    -0.5*( fCoriG(i,j,bi,bj)+fCoriG(i+1,j,bi,bj)
     &         )*uBarXY*recip_dyC(i,j,bi,bj)*_recip_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme .EQ. 3 ) THEN
C- Energy-conserving discretisation with hFac weighted average :
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         uBarYm = halfRL *(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj) )
         uBarYp = halfRL *(
     &       uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj) )
         vCoriolisTerm(i,j) = -0.5 _d 0
     &        *( uBarYm*fCoriG( i ,j,bi,bj)
     &          +uBarYp*fCoriG(i+1,j,bi,bj)
     &         )*recip_dyC(i,j,bi,bj)*_recip_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ELSE
        STOP 'MOM_VI_CORIOLIS: invalid selectCoriScheme'
      ENDIF

      RETURN
      END
