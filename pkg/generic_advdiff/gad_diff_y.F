C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diff_y.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DIFF_Y( 
     I           bi,bj,k,
     I           yA, diffKh,
     I           tracer,
     O           dfy,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO i=1-Olx,sNx+Olx
       dfy(i,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        dfy(i,j) = -diffKh*yA(i,j)
     &      *_recip_dyC(i,j,bi,bj)
     &      *(Tracer(i,j)-Tracer(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
     &      *CosFacV(j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
