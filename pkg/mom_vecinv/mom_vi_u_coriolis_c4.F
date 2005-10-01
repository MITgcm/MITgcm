C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_coriolis_c4.F,v 1.6 2005/10/01 19:17:17 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_U_CORIOLIS_C4
C     !INTERFACE:
      SUBROUTINE MOM_VI_U_CORIOLIS_C4(
     I        bi,bj,k,
     I        vFld,omega3,r_hFacZ,
     O        uCoriolisTerm,
     I        myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_VI_U_CORIOLIS_C4
C     |==========================================================*
C     | o Calculate meridional flux of vorticity at U point
C     |   using 4th order interpolation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

C     == Local variables ==
      INTEGER i,j
      _RL vBarXY,vort3u,Rjp,Rjm
      _RL vBarXm,vBarXp,oneSixth
      LOGICAL upwindVort3
      LOGICAL fourthVort3

      PARAMETER(oneSixth=1.D0/6.D0)
      PARAMETER(upwindVort3=.FALSE.)
      PARAMETER(fourthVort3=.TRUE. )

c     DO j=2-Oly,sNy+Oly-2
c      DO i=2-Olx,sNx+Olx
      DO j=1,sNy
       DO i=1,sNx+1

        IF ( SadournyCoriolis ) THEN
C-      using SadournyCoriolis discretization:

         vBarXY=1.
         vBarXm=0.5*(
     &       vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacS(i-1, j ,k,bi,bj) )
         vBarXp=0.5*(
     &       vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj) )
         IF (upwindVorticity) THEN
          IF ( (vBarXm+vBarXp) .GT.0.) THEN
           vort3u=vBarXm*r_hFacZ(i, j )*omega3(i, j )
          ELSE
           vort3u=vBarXp*r_hFacZ(i,j+1)*omega3(i,j+1)
          ENDIF
         ELSEIF (fourthVort3) THEN
          Rjp = omega3(i,j+1)*r_hFacZ(i,j+1)
     &         -oneSixth*( omega3(i,j+2)*r_hFacZ(i,j+2)
     &                    -omega3(i, j )*r_hFacZ(i, j ) )
          Rjm = omega3(i,j)*r_hFacZ(i,j)
     &         +oneSixth*( omega3(i,j+1)*r_hFacZ(i,j+1)
     &                    -omega3(i,j-1)*r_hFacZ(i,j-1) )
          vort3u=0.5*( vBarXm*Rjm + vBarXp*Rjp )
         ELSE
          vort3u=0.5*( vBarXm*r_hFacZ(i, j )*omega3(i, j )
     &               + vBarXp*r_hFacZ(i,j+1)*omega3(i,j+1) )
         ENDIF

        ELSE
C-      not using SadournyCoriolis discretization:

         vBarXY=0.25*(
     &      (vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj)
     &      +vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacS(i-1, j ,k,bi,bj))
     &     +(vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &      +vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj))
     &               )
         IF (upwindVort3) THEN
          IF (vBarXY.GT.0.) THEN
           vort3u=omega3(i,j)*r_hFacZ(i,j)
          ELSE
           vort3u=omega3(i,j+1)*r_hFacZ(i,j+1)
          ENDIF
         ELSEIF (fourthVort3) THEN
          Rjp=omega3(i,j+2)*r_hFacZ(i,j+2)
     &       -omega3(i,j+1)*r_hFacZ(i,j+1)
          Rjm=omega3(i,j)*r_hFacZ(i,j)
     &       -omega3(i,j-1)*r_hFacZ(i,j-1)
          vort3u=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i,j+1)*r_hFacZ(i,j+1)
     &               -1./12.*(Rjp-Rjm)
     &               )
         ELSE
          vort3u=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i,j+1)*r_hFacZ(i,j+1))
         ENDIF

C-      end if / else SadournyCoriolis
        ENDIF

        uCoriolisTerm(i,j)=
     &    vort3u*vBarXY*recip_dxC(i,j,bi,bj)
     &                 * _maskW(i,j,k,bi,bj)

       ENDDO
      ENDDO

      RETURN
      END
