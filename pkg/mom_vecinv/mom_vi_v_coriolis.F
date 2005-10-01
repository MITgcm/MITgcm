C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_coriolis.F,v 1.9 2005/10/01 19:17:17 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_V_CORIOLIS(
     I        bi,bj,k,
     I        uFld,omega3,hFacZ,r_hFacZ,
     O        vCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     *==========================================================*
C     | S/R MOM_VI_V_CORIOLIS
C     | o Calculate zonal flux of vorticity at V point
C     *==========================================================*

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      LOGICAL use_original_hFac
      INTEGER I,J
      _RL uBarXY,vort3v,Zp,Zm
      _RS epsil
      PARAMETER ( use_original_hFac=.FALSE. )

      epsil = 1. _d -9

      DO J=2-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx-1
        IF ( use_original_hFac ) THEN
         uBarXY=0.25*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj)))
         IF (upwindVorticity) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)*r_hFacZ(i,j)
          ELSE
           vort3v=omega3(i+1,j)*r_hFacZ(i+1,j)
          ENDIF
         ELSE
           vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &                +omega3(i+1,j)*r_hFacZ(i+1,j))
         ENDIF
        ELSEIF ( SadournyCoriolis ) THEN
         Zm=0.5*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj) )
         Zp=0.5*(
     &       uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj) )
         IF (upwindVorticity) THEN
          IF ( (Zm+Zp) .GT.0.) THEN
           vort3v=Zm*r_hFacZ( i ,j)*omega3( i ,j)
          ELSE
           vort3v=Zp*r_hFacZ(i+1,j)*omega3(i+1,j)
          ENDIF
         ELSE
          Zm=Zm*r_hFacZ( i ,j)*omega3( i ,j)
          Zp=Zp*r_hFacZ(i+1,j)*omega3(i+1,j)
          vort3v=0.5*( Zm + Zp )
         ENDIF
         uBarXY=1.
        ELSE
c--      test a different formulation (relatively to hFac)
         uBarXY=0.5*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*hFacZ(i,j)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacZ(i,j)  )
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacZ(i+1,j)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacZ(i+1,j))
     &              )/MAX( epsil, hFacZ(i,j)+hFacZ(i+1,j) )
         IF (upwindVorticity) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)
          ELSE
           vort3v=omega3(i+1,j)
          ENDIF
         ELSE
           vort3v=0.5*(omega3(i,j)+omega3(i+1,j))
         ENDIF
        ENDIF

        IF (useJamartMomAdv)
     &   uBarXY = uBarXY * 4. _d 0 * hFacS(i,j,k,bi,bj)
     &   / MAX( epsil, (hFacW( i ,j,k,bi,bj)+hFacW( i ,j-1,k,bi,bj))
     &                +(hFacW(i+1,j,k,bi,bj)+hFacW(i+1,j-1,k,bi,bj)) )

        vCoriolisTerm(i,j)=
     &   -vort3v*uBarXY*recip_dyC(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
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
