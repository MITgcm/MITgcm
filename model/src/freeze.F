C $Header: /u/gcmpack/MITgcm/model/src/Attic/freeze.F,v 1.11 2005/01/25 22:26:52 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREEZE
C     !INTERFACE:
      SUBROUTINE FREEZE( bi, bj, iMin, iMax, jMin, jMax,
     I                     K,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREEZE                                                
C     | o Check water temperature and limit range of temperature  
C     | appropriately.                                            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER i,j
      _RL Tfreezing
CEOP

      Tfreezing=-1.9 _d 0

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
