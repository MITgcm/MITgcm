C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_coriolis.F,v 1.3 2001/09/05 17:46:03 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_U_CORIOLIS( 
     I        bi,bj,K,
     I        vFld,omega3,r_hFacZ,
     O        uCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_VI_U_CORIOLIS                                    |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL vBarXY,vort3u
      LOGICAL upwindVort3

      upwindVort3=.FALSE.

       DO J=1-Oly,sNy+Oly-1
        DO I=2-Olx,sNx+Olx
         vBarXY=0.25*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj)
     &      +vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacS(i-1, j ,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj))
c        vBarXY=0.25*(vFld( i ,j)+vFld( i ,j+1)
c    &               +vFld(i-1,j)+vFld(i-1,j+1))
         IF (upwindVort3) THEN
          IF (vBarXY.GT.0.) THEN
           vort3u=omega3(I,J)*r_hFacZ(i,j)
          ELSE
           vort3u=omega3(I,J+1)*r_hFacZ(i,j+1)
          ENDIF
         ELSE
          vort3u=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i,j+1)*r_hFacZ(i,j+1))
         ENDIF
         uCoriolisTerm(i,j)=
     &   +vort3u*vBarXY*recip_dxc(i,j,bi,bj)
     &   *_maskW(I,J,K,bi,bj)
cph *note* put these comments after end of continued line
cph        to ensure TAMC compatibility
C high order vorticity advection term
c    &   ...
C linear Coriolis term
c    &   +0.5*(fCoriG(I,J,bi,bj)+fCoriG(I,J+1,bi,bj))*vBarXY
C full nonlinear Coriolis term
c    &   +0.5*(omega3(I,J)+omega3(I,J+1))*vBarXY
C correct energy conserving form of Coriolis term
c    &   +0.5*( fCori(I  ,J,bi,bj)*vBarY(I  ,J,K,bi,bj) +
c    &          fCori(I-1,J,bi,bj)*vBarY(I-1,J,K,bi,bj)  )
C original form of Coriolis term (copied from calc_mom_rhs)
c    &   +0.5*(fCori(i,j,bi,bj)+fCori(i-1,j,bi,bj))*vBarXY
        ENDDO
       ENDDO

      RETURN
      END
