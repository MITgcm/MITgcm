#include "RW_OPTIONS.h"

CBOP
C     !ROUTINE: RW_GET_SUFFIX
C     !INTERFACE:

      SUBROUTINE RW_GET_SUFFIX(
     O                  suff,
     I                  myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     Return a suffix for file names to read/write.
C     Default is myIter. Options are for seconds, hours etc, as below

C     !USES:
      IMPLICIT NONE

C     Global variables:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: My Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     suff   :: file-name suffix (10.c long)
      CHARACTER*(*) suff

C     !LOCAL VARIABLES:
CEOP

      IF ( rwSuffixType.EQ.1 ) THEN
C     100ths of a second
         WRITE(suff,'(I10.10)') NINT( myTime*100. _d 0 )
      ELSEIF ( rwSuffixType.EQ.2 ) THEN
C     seconds
         WRITE(suff,'(I10.10)') NINT( myTime )
      ELSEIF ( rwSuffixType.EQ.3 ) THEN
C     10ths hours
         WRITE(suff,'(I10.10)') NINT( myTime/360. _d 0 )
      ELSEIF ( rwSuffixType.EQ.4 ) THEN
C     hours
         WRITE(suff,'(I10.10)') NINT( myTime/3600. _d 0 )
      ELSE
C     default, iteration number
         WRITE(suff,'(I10.10)') myIter
      ENDIF

      RETURN
      END
