C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_grad_eta_surf.F,v 1.4 1998/11/06 22:44:44 cnh Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CALC_GRAD_ETA_SURF                                   |
C     | o Calculate the gradient of the surface height           |
C     \==========================================================/
      SUBROUTINE CALC_GRAD_ETA_SURF( bi, bj, iMin, iMax, jMin, jMax,
     O                       etaSurfX, etaSurfY,
     I                       myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
C     == Routine Arguments ==
C     etaSurfX, etaSurfY - Surface slope in the X and Y directions.
C     bi,bj,iMin,iMax,jMin,jMax - Loop counters
C     myThid - Instance number for this call of the routine.
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL etaSurfX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaSurfY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j

C     Zonal term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaSurfX(i,j)=_recip_dxC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
       ENDDO
      ENDDO

C     Meridional term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaSurfY(i,j)=_recip_dyC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
       ENDDO
      ENDDO

      RETURN
      END
