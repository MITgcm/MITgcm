C $Header: /u/gcmpack/MITgcm/model/src/Attic/freeze.F,v 1.2 2001/02/04 14:38:47 cnh Exp $
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
         IF (gTNm1(i,j,k,bi,bj) .LT. Tfreezing) THEN
          gTNm1(i,j,k,bi,bj)=Tfreezing
         ENDIF
        ENDDO
       ENDDO

      RETURN
      END
