C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_grad_eta_surf.F,v 1.6 2001/03/06 16:51:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CALC_GRAD_ETA_SURF                                   |
C     | o Calculate the gradient of the surface height           |
C     \==========================================================/
      SUBROUTINE CALC_GRAD_ETA_SURF( bi, bj, iMin, iMax, jMin, jMax,
     I                       etaFld,
     O                       etaSurfX, etaSurfY,
     I                       myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine Arguments ==
C     etaFld   - free-surface r-anomaly (r unit).
C     etaSurfX, etaSurfY - Surface slope in the X and Y directions.
C     bi,bj,iMin,iMax,jMin,jMax - Loop counters
C     myThid - Instance number for this call of the routine.
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaSurfX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaSurfY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j

C     Zonal term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaSurfX(i,j)=_recip_dxC(i,j,bi,bj)*
     &   (etaFld(i,j,bi,bj)-etaFld(i-1,j,bi,bj))
       ENDDO
      ENDDO

C     Meridional term
      DO j=jMin,jMax
       DO i=iMin,iMax
        etaSurfY(i,j)=_recip_dyC(i,j,bi,bj)*
     &   (etaFld(i,j,bi,bj)-etaFld(i,j-1,bi,bj))
       ENDDO
      ENDDO

      RETURN
      END
