C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_coriolis.F,v 1.9 2006/06/07 01:55:15 heimbach Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_CORIOLIS(
     I        bi,bj,K,
     I        uFld,vFld,hFacZ,r_hFacZ,
     O        uCoriolisTerm,vCoriolisTerm,
     I        myThid)
      IMPLICIT NONE
C     *==========================================================*
C     | S/R MOM_VI_CORIOLIS
C     *==========================================================*
C     *==========================================================*

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL vBarXY,uBarXY
      _RS epsil
      epsil = 1. _d -9

      IF (useJamartWetPoints) THEN
C- Partial-cell generalization of the Wet-point average method :
       DO J=1-Oly,sNy+Oly-1
        DO I=2-Olx,sNx+Olx
         vBarXY=(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*_hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*_hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*_hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*_hFacS(i-1,j+1,k,bi,bj)))
     &   / MAX( epsil,(_hFacS(i, j ,k,bi,bj)+_hFacS(i-1, j ,k,bi,bj))
     &               +(_hFacS(i,j+1,k,bi,bj)+_hFacS(i-1,j+1,k,bi,bj)) )
         uCoriolisTerm(i,j)=
     &    +0.5*( fCoriG(i,j,bi,bj)+fCoriG(i,j+1,bi,bj)
     &         )*vBarXY*recip_dxC(i,j,bi,bj)*_maskW(I,J,K,bi,bj)
        ENDDO
       ENDDO
      ELSE
C- Simple average, no hFac :
       DO J=1-Oly,sNy+Oly-1
        DO I=2-Olx,sNx+Olx
         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj))
     &               )
         uCoriolisTerm(i,j)=
     &    +0.5*( fCoriG(i,j,bi,bj)+fCoriG(i,j+1,bi,bj)
     &         )*vBarXY*recip_dxC(i,j,bi,bj)*_maskW(I,J,K,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      IF (useJamartWetPoints) THEN
C- Partial-cell generalization of the Wet-point average method :
       DO J=2-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx-1
         uBarXY=(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)*_hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*_hFacW( i ,j-1,k,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)*_hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*_hFacW(i+1,j-1,k,bi,bj)))
     &   / MAX( epsil,(_hFacW( i ,j,k,bi,bj)+_hFacW( i ,j-1,k,bi,bj))
     &               +(_hFacW(i+1,j,k,bi,bj)+_hFacW(i+1,j-1,k,bi,bj)) )
         vCoriolisTerm(i,j)=
     &    -0.5*( fCoriG(i,j,bi,bj)+fCoriG(i+1,j,bi,bj)
     &         )*uBarXY*recip_dyC(i,j,bi,bj)*_maskS(I,J,K,bi,bj)
        ENDDO
       ENDDO
      ELSE
C- Simple average, no hFac :
       DO J=2-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx-1
         uBarXY=0.25*(
     &      (uFld( i , j )*dyG( i , j ,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj))
     &     +(uFld(i+1, j )*dyG(i+1, j ,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj))
     &               )
         vCoriolisTerm(i,j)=
     &    -0.5*( fCoriG(i,j,bi,bj)+fCoriG(i+1,j,bi,bj)
     &         )*uBarXY*recip_dyC(i,j,bi,bj)*_maskS(I,J,K,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
