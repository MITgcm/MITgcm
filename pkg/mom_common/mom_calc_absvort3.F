C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_absvort3.F,v 1.2 2005/04/28 14:51:04 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_CALC_ABSVORT3( 
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

      IF (momAdvection) THEN
        nonLinFac=1.
      ELSE
        nonLinFac=0.
      ENDIF
      IF (useCoriolis) THEN
        useCoriolisFac=1.
      ELSE
        useCoriolisFac=0.
      ENDIF

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        omega3(I,J)=
     &      fCoriG(i,j,bi,bj)*useCoriolisFac
     &     +vort3(i,j)*nonLinFac
       ENDDO
      ENDDO
        
      RETURN
      END
