C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_ph.F,v 1.2 1998/04/24 02:05:40 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CALC_PH( bi, bj, iMin, iMax, jMin, jMax, pH, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_PH                                       |
C     | o Find the hydrostatic component of pressure             |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "PARAMS.h"
C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      integer myThid
C     == Local variables ==
      INTEGER i,j,k
      INTEGER k_up,k_down,kp1

C--   pH(k=1) is half-way within top level
      DO j=jMin,jMax
       DO i=iMin,iMax
        pH(i,j,1)=0.
     &      +dzF(1)*hFacC(i,j,1,bi,bj)*Gravity*Rho(i,j,1,bi,bj)*0.5 _d 0
       ENDDO
      ENDDO

C--   Integrate down for lower levels
      DO k=2,Nz
       DO j=jMin,jMax
        DO i=iMin,iMax
         pH(i,j,k)=pH(i,j,k-1)
     &       +Gravity*( dzF(k  )*hFacC(i,j,k  ,bi,bj)*0.5 _d 0
     &                 +dzF(k-1)*hFacC(i,j,k-1,bi,bj)*0.5 _d 0
     &                )*
     &        (Rho(i,j,k,bi,bj)+Rho(i,j,k-1,bi,bj))*0.5 _d 0
        ENDDO
       ENDDO
      ENDDO

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
