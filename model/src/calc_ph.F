C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_ph.F,v 1.3 1998/04/29 21:28:20 adcroft Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CALC_PH( bi, bj, iMin, iMax, jMin, jMax, K, rhokm1, pH, myThid)
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
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhokm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      integer myThid
C     == Local variables ==
      INTEGER i,j
      _RL delta_Ph

C     (Before we do anything, pH(:,:,K-1) contains the hydrostatic
C      pressure at the W(K-1) level. If K=2, then that is same as the
C      surface pH which is normally initialized as pH(z=0)=0 )

C--   Contribution to PH(:,:,K-1) from RHO(:,:,K-1)
C     (This is now the actual hydrostatic pressure at the T/S points)
      DO j=jMin,jMax
       DO i=iMin,iMax
        pH(i,j,K-1)=pH(i,j,K-1)
     &    +0.5 _d 0*dzF(K-1)*hFacC(i,j,K-1,bi,bj)*Gravity*rhokm1(i,j)
       ENDDO
      ENDDO

      if (K.le.Nz) then
C--   Contribution to PH(:,:,K) from RHO(:,:,K-1)
C     (This is, temporarily, the hydrostatic pressure at the W points
C      and will be further added to at the next level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         pH(i,j, K )=pH(i,j,K-1)
     &     +0.5 _d 0*dzF(K-1)*hFacC(i,j,K-1,bi,bj)*Gravity*rhokm1(i,j)
        ENDDO
       ENDDO
      endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
