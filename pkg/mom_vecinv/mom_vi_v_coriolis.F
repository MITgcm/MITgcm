C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_coriolis.F,v 1.3 2001/09/05 17:46:03 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_V_CORIOLIS( 
     I        bi,bj,K,
     I        uFld,omega3,r_hFacZ,
     O        vCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_VI_V_CORIOLIS                                    |
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
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL uBarXY,vort3v
      LOGICAL upwindVort3

      upwindVort3=.FALSE.

       DO J=2-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx-1
         uBarXY=0.25*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj))
c        uBarXY=0.25*( uFld(i, j )+uFld(i+1, j )
c    &                +uFld(i,j-1)+uFld(i+1,j-1))
         IF (upwindVort3) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)*r_hFacZ(i,j)
          ELSE
           vort3v=omega3(i+1,j)*r_hFacZ(i+1,j)
          ENDIF
         ELSE
          vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i+1,j)*r_hFacZ(i+1,j))
         ENDIF
         vCoriolisTerm(i,j)=
     &   -vort3v*uBarXY*recip_dyc(i,j,bi,bj)
     &   *_maskS(I,J,K,bi,bj)
C high order vorticity advection term
c    &   ...
C linear Coriolis term
c    &   -0.5 *(fCoriG(I,J,bi,bj)+fCoriG(I+1,J,bi,bj))*uBarXY
C full nonlinear Coriolis term
c    &   -0.5*(omega3(I,J)+omega3(I+1,J))*uBarXY
C correct energy conserving form of Coriolis term
c    &   -0.5 *( fCori(I,J  ,bi,bj)*uBarX(I,J  ,K,bi,bj) +
c    &           fCori(I,J-1,bi,bj)*uBarX(I,J-1,K,bi,bj)  )
C original form of Coriolis term (copied from calc_mom_rhs)
c    &   -0.5*(fCori(i,j,bi,bj)+fCori(i,j-1,bi,bj))*uBarXY
        ENDDO
       ENDDO

      RETURN
      END
