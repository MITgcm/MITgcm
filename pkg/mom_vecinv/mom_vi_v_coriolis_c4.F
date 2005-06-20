C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_coriolis_c4.F,v 1.5 2005/06/20 23:23:00 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_V_CORIOLIS_C4
C     !INTERFACE:
      SUBROUTINE MOM_VI_V_CORIOLIS_C4(
     I        bi,bj,k,
     I        uFld,omega3,r_hFacZ,
     O        vCoriolisTerm,
     I        myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MOM_VI_V_CORIOLIS_C4
C     |==========================================================*
C     | o Calculate zonal flux of vorticity at V point
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

C     == Local variables ==
      INTEGER i,j
      _RL uBarXY,vort3v,Rjp,Rjm
      _RL uBarYm,uBarYp,oneSixth
      LOGICAL upwindVort3
      LOGICAL fourthVort3

      PARAMETER(oneSixth=1.D0/6.D0)
      PARAMETER(upwindVort3=.FALSE.)
      PARAMETER(fourthVort3=.TRUE. )

c     DO j=2-Oly,sNy+Oly
c      DO i=2-Olx,sNx+Olx-2
      DO j=1,sNy+1
       DO i=1,sNx

        IF ( SadournyCoriolis ) THEN
C-      using SadournyCoriolis discretization:

         uBarXY=1.
         uBarYm=0.5*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj) )
         uBarYp=0.5*(
     &       uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj) )
         IF (upwindVorticity) THEN
          IF ( (uBarYm+uBarYp) .GT.0.) THEN
           vort3v=uBarYm*r_hFacZ( i ,j)*omega3( i ,j)
          ELSE
           vort3v=uBarYp*r_hFacZ(i+1,j)*omega3(i+1,j)
          ENDIF
         ELSEIF (fourthVort3) THEN
          Rjp = omega3(i+1,j)*r_hFacZ(i+1,j)
     &         -oneSixth*( omega3(i+2,j)*r_hFacZ(i+2,j)
     &                    -omega3( i ,j)*r_hFacZ( i ,j) )
          Rjm = omega3(i,j)*r_hFacZ(i,j)
     &         +oneSixth*( omega3(i+1,j)*r_hFacZ(i+1,j)
     &                    -omega3(i-1,j)*r_hFacZ(i-1,j) )
          vort3v=0.5*( uBarYm*Rjm + uBarYp*Rjp )
         ELSE
          vort3v=0.5*( uBarYm*r_hFacZ( i ,j)*omega3( i ,j)
     &                +uBarYp*r_hFacZ(i+1,j)*omega3(i+1,j) )
         ENDIF

        ELSE
C-      not using SadournyCoriolis discretization:

         uBarXY=0.25*(
     &       uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &      +uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &      +uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj)
     &      +uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj))
         IF (upwindVort3) THEN
          IF (uBarXY.GT.0.) THEN
           vort3v=omega3(i,j)*r_hFacZ(i,j)
          ELSE
           vort3v=omega3(i+1,j)*r_hFacZ(i+1,j)
          ENDIF
         ELSEIF (fourthVort3) THEN
          Rjp=omega3(i+2,j)*r_hFacZ(i+2,j)
     &       -omega3(i+1,j)*r_hFacZ(i+1,j)
          Rjm=omega3(i,j)*r_hFacZ(i,j)
     &       -omega3(i-1,j)*r_hFacZ(i-1,j)
          vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i+1,j)*r_hFacZ(i+1,j)
     &               -1./12.*(Rjp-Rjm)
     &               )
         ELSE
          vort3v=0.5*(omega3(i,j)*r_hFacZ(i,j)
     &               +omega3(i+1,j)*r_hFacZ(i+1,j))
         ENDIF

C-      end if / else SadournyCoriolis
        ENDIF

        vCoriolisTerm(i,j)=
     &   -vort3v*uBarXY*recip_dyC(i,j,bi,bj)
     &                 * _maskS(i,j,k,bi,bj)

       ENDDO
      ENDDO

      RETURN
      END
