C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_calc_relvort3.F,v 1.3 2001/09/05 17:46:03 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_CALC_RELVORT3( 
     I        bi,bj,k,
     I        uFld, vFld, hFacZ,
     O        vort3,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_RELVORT3                                    |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO J=2-Oly,sNy+Oly
       DO I=2-Olx,sNx+Olx

C       Horizontal curl of flow field - ignoring lopping factors
        vort3(I,J)=
     &      recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &                           )

C       Horizontal curl of flow field - including lopping factors
        IF (hFacZ(i,j).NE.0.) THEN
c        vort3(I,J)=
c    &      recip_rAz(I,J,bi,bj)*(
c    &      vFld(I,J)*dyc(I,J,bi,bj)*_hFacW(i,j,k,bi,bj)
c    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)*_hFacW(i-1,j,k,bi,bj)
c    &     -uFld(I,J)*dxc(I,J,bi,bj)*_hFacS(i,j,k,bi,bj)
c    &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)*_hFacS(i,j-1,k,bi,bj)
c    &                           )
c    &                            /hFacZ(i,j)
        ELSE
         vort3(I,J)=0.
        ENDIF

       ENDDO
      ENDDO
        
C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
         I=1
         J=1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
         I=sNx+1
         J=1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &      vFld(I,J)*dyc(I,J,bi,bj)
         I=1
         J=sNy+1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
         I=sNx+1
         J=sNy+1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &      vFld(I,J)*dyc(I,J,bi,bj)
       ENDIF

      RETURN
      END
