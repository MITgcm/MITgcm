C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_biharm_y.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_BIHARM_Y( 
     I           bi,bj,k,
     I           yA,del2T,diffK4,
     O           dfy,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS yA   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2T(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffK4
      _RL dfy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

C     Difference of meridional fluxes ...
      DO i=1-Olx,sNx+Olx
       dfy(i,1-Oly) = 0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
         dfy(i,j) = diffK4
     &    *yA(i,j)
     &    *(del2T(i,j)-del2T(i,j-1))*_recip_dyC(i,j,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &    *sqCosFacV(j,bi,bj)
#else
     &    *CosFacV(j,bi,bj)
#endif
#endif
       ENDDO
      ENDDO

      RETURN
      END
