C $Header: /u/gcmpack/MITgcm/model/src/Attic/freeze.F,v 1.3 2001/09/19 02:43:27 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R FREEZE                                               |
C     | o Check water temperature and limit range of temperature |
C     | appropriately.                                           |
C     \==========================================================/
      SUBROUTINE FREEZE( bi, bj, iMin, iMax, jMin, jMax,
     I                     K,
     I                     myThid )
      implicit none
C Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j
      _RL Tfreezing

      Tfreezing=-1.9

C     Check for water that should have frozen
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF (gT(i,j,k,bi,bj) .LT. Tfreezing) THEN
          gT(i,j,k,bi,bj)=Tfreezing
         ENDIF
        ENDDO
       ENDDO

      RETURN
      END
