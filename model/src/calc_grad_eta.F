C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_grad_eta.F,v 1.1 1998/08/20 18:43:11 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R CALC_GRAD_ETA                                        |
C     | o Calculate the gradient of the surface height           |
C     \==========================================================/
      SUBROUTINE CALC_GRAD_ETA( bi, bj, iMin, iMax, jMin, jMax,
     O                       etaX, etaY,
     I                       myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL etaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j

C     Zonal pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaX(i,j)=_rdxC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
       ENDDO
      ENDDO

C     Meridional pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaY(i,j)=_rdyC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
       ENDDO
      ENDDO

      RETURN
      END
