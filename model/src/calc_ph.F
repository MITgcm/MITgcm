C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_ph.F,v 1.6 1998/06/09 16:48:02 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CALC_PH( bi, bj, iMin, iMax, jMin, jMax, K,
     I    rhokm1, rhokp1, pH, myThid)
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
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhokm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhokp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      integer myThid
C     == Local variables ==
      INTEGER i,j,Km1
      _RL halfLayer

      if (K.eq.1) then
       Km1=1
       halfLayer=0.5 _d 0
      else
       Km1=K-1
       halfLayer=1.0 _d 0
      endif

C--   Contribution to PH(:,:,K-1) from RHO(:,:,K-1)
C     (This is now the actual hydrostatic pressure at the T/S points)
      DO j=jMin,jMax
       DO i=iMin,iMax
        pH(i,j,K)=pH(i,j,Km1)+Gravity*halfLayer
     &  *0.5 _d 0*(dzF(Km1)*_hFacC(i,j,Km1,bi,bj)+dzF(K)*_hFacC(i,j,K,bi,bj))
     &  *0.5 _d 0*( rhokm1(i,j)+rhokp1(i,j) )
       ENDDO
      ENDDO

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
