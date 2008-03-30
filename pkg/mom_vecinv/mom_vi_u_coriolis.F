C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_coriolis.F,v 1.11 2008/03/30 21:46:48 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_U_CORIOLIS(
     I        bi,bj,k,
     I        vFld,omega3,hFacZ,r_hFacZ,
     O        uCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     *==========================================================*
C     | S/R MOM_VI_U_CORIOLIS
C     | o Calculate meridional flux of vorticity at U point
C     *==========================================================*

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      LOGICAL use_original_hFac
      INTEGER I,J
      _RL vBarXY,vort3u,Zp,Zm
      _RS epsil
      PARAMETER ( use_original_hFac=.FALSE. )

      epsil = 1. _d -9

      DO J=1-Oly,sNy+Oly-1
       DO I=2-Olx,sNx+Olx
        IF ( use_original_hFac ) THEN
         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj)))
         IF (upwindVorticity) THEN
          IF (vBarXY.GT.0.) THEN
           vort3u=omega3(I,J)*r_hFacZ(i,j)
          ELSE
           vort3u=omega3(I,J+1)*r_hFacZ(i,j+1)
          ENDIF
         ELSE
           vort3u=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &                +omega3(i,j+1)*r_hFacZ(i,j+1))
         ENDIF
        ELSEIF ( selectVortScheme.EQ.2 ) THEN
         Zm=0.5*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj) )
         Zp=0.5*(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj) )
         IF (upwindVorticity) THEN
          IF ( (Zm+Zp) .GT.0.) THEN
           vort3u=Zm*r_hFacZ(i, j )*omega3(i, j )
          ELSE
           vort3u=Zp*r_hFacZ(i,j+1)*omega3(i,j+1)
          ENDIF
         ELSE
          Zm=Zm*r_hFacZ(i, j )*omega3(i, j )
          Zp=Zp*r_hFacZ(i,j+1)*omega3(i,j+1)
          vort3u=0.5*( Zm + Zp )
         ENDIF
         vBarXY=1.
        ELSE
c--      test a different formulation (relatively to hFac)
         vBarXY=0.5*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*hFacZ(i,j)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacZ(i,j)  )
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacZ(i,j+1)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacZ(i,j+1))
     &              )/MAX( epsil, hFacZ(i,j)+hFacZ(i,j+1) )
         IF (upwindVorticity) THEN
          IF (vBarXY.GT.0.) THEN
           vort3u=omega3(i,j)
          ELSE
           vort3u=omega3(i,j+1)
          ENDIF
         ELSE
           vort3u=0.5*(omega3(i,j)+omega3(i,j+1))
         ENDIF
        ENDIF

        IF (useJamartMomAdv)
     &   vBarXY = vBarXY * 4. _d 0 * hFacW(i,j,k,bi,bj)
     &   / MAX( epsil,(_hFacS(i, j ,k,bi,bj)+_hFacS(i-1, j ,k,bi,bj))
     &               +(_hFacS(i,j+1,k,bi,bj)+_hFacS(i-1,j+1,k,bi,bj)) )

        uCoriolisTerm(i,j)=
     &   +vort3u*vBarXY*recip_dxC(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
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
