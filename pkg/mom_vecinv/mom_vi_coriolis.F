C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_coriolis.F,v 1.3 2001/09/05 17:46:03 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_CORIOLIS( 
     I        bi,bj,K,
     I        uFld,vFld,omega3,r_hFacZ,
     O        uCoriolisTerm,vCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R VORTICITY_X_V                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL vBarXY,uBarXY,vort3u,vort3v

       DO J=1-Oly,sNy+Oly-1
        DO I=2-Olx,sNx+Olx
c        vBarXY=0.25*(vFld( i ,j)+vFld( i ,j+1)
c    &               +vFld(i-1,j)+vFld(i-1,j+1))
c        vort3u=0.5*(omega3(i,j)+omega3(I,J+1))
         vBarXY=0.25*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj)
     &      +vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacS(i-1, j ,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj))
         vort3u=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &              +omega3(i,j+1)*r_hFacZ(i,j+1))
         uCoriolisTerm(i,j)=
     &   +0.5*(fCoriG(i,j,bi,bj)*r_hFacZ(i,j)
     &        +fCoriG(i,j+1,bi,bj)*r_hFacZ(i,j+1))*vBarXY
     *        *recip_dxc(i,j,bi,bj)
     &   *_maskW(I,J,K,bi,bj)
cph *note* put these comments after end of continued line
cph        to ensure TAMC compatibility
C high order vorticity advection term
c    &   +vort3u*vBarXY*recip_dxc(i,j,bi,bj)
C linear Coriolis term (enstrophy conserving)
c    &   ...
C full nonlinear Coriolis term
c    &   +0.5*(omega3(i,j)+omega3(i,j+1))*vBarXY
C correct energy conserving form of Coriolis term
c    &   +0.25*( fCori( i ,j,bi,bj)*(vFld( i ,j)+vFld( i ,j+1)) +
c    &           fCori(i-1,j,bi,bj)*(vFld(i-1,j)+vFld(i-1,j+1)) )
C original form of Coriolis term (copied from calc_mom_rhs)
c    &   +0.5*(fCori(i,j,bi,bj)+fCori(i-1,j,bi,bj))*vBarXY
        ENDDO
       ENDDO

       DO J=2-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx-1
c        uBarXY=0.25*( uFld(i, j )+uFld(i+1, j )
c    &                +uFld(i,j-1)+uFld(i+1,j-1))
c        vort3v=0.5*(omega3(i,j)+omega3(I+1,J))
         uBarXY=0.25*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj))
         vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &              +omega3(i+1,j)*r_hFacZ(i+1,j))
         vCoriolisTerm(i,j)=
     &   -0.5 *(fCoriG(i,j,bi,bj)*r_hFacZ(i,j)
     &         +fCoriG(i+1,j,bi,bj)*r_hFacZ(i+1,j))*uBarXY
     &         *recip_dyc(i,j,bi,bj)
     &   *_maskS(I,J,K,bi,bj)
cph *note* put these comments after end of continued line
cph        to ensure TAMC compatibility
C high order vorticity advection term
c    &   -vort3v*uBarXY*recip_dyc(i,j,bi,bj)
C linear Coriolis term (enstrophy conserving)
c    &   ...
C full nonlinear Coriolis term
c    &   -0.5*(omega3(i,j)+omega3(i+1,j))*uBarXY
C correct energy conserving form of Coriolis term
c    &   -0.25*( fCori(i,j  ,bi,bj)*(uFld(i, j )+uFld(i+1,j)) +
c    &           fCori(i,j-1,bi,bj)*(uFld(i,j-1)+uFld(i+1,j)) )
C original form of Coriolis term (copied from calc_mom_rhs)
c    &   -0.5*(fCori(i,j,bi,bj)+fCori(i,j-1,bi,bj))*uBarXY
        ENDDO
       ENDDO


      RETURN
      END
