C $Header: /u/gcmpack/MITgcm/model/src/Attic/grad_psurf.F,v 1.3 1998/05/27 21:01:47 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R GRAD_PSURF                                           |
C     | o Calculate the gradient of the surface pressure         |
C     \==========================================================/
      SUBROUTINE GRAD_PSURF( bi, bj, iMin, iMax, jMin, jMax,
     O                       pSurfX, pSurfY,
     I                       myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "CG2D.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL pSurfX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL pSurfY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j

C     Zonal pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        pSurfX(i,j)=_rdxC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
     &   *gBaro*rhonil
       ENDDO
      ENDDO

C     Meridional pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        pSurfY(i,j)=rDyC(i,j,bi,bj)*
     &   (cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
     &   *gBaro*rhonil
       ENDDO
      ENDDO

      RETURN
      END
