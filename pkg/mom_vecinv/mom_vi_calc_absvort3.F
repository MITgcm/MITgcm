C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_calc_absvort3.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_CALC_ABSVORT3( 
     I        bi,bj,k,
     I        vort3,
     O        omega3,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_ABSVORT3                                    |
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
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL nonLinFac,useCoriolisFac

      nonLinFac=0.
      IF (momAdvection) nonLinFac=0.
      useCoriolisFac=0.
      IF (useCoriolis) useCoriolisFac=1.

      DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
        omega3(I,J)=
     &      fCoriG(I,J,bi,bj)*useCoriolisFac
     &     +vort3(i,j)*nonLinFac
       ENDDO
      ENDDO
        
      RETURN
      END
