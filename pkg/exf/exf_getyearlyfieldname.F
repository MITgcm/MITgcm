#include "EXF_OPTIONS.h"

CBOP
C !ROUTINE: exf_GetYearlyFieldName

C !INTERFACE:
      SUBROUTINE exf_GetYearlyFieldName(
     I     useYearlyFields, twoDigitYear, genperiod, year, genfile,
     O     genfileout,
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     o Determine actual name of forcing file including year extension
C
C     started: Martin Losch  24-Jan-2008
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===

C     !INPUT PARAMETERS:
      LOGICAL useYearlyFields
      LOGICAL twoDigitYear
      _RL     genperiod
      INTEGER year
      CHARACTER*(128) genfile
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !OUTPUT PARAMETERS:
      CHARACTER*(128) genfileout

#ifdef ALLOW_EXF
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER yearLoc
      INTEGER il
CEOP

      IF ( useYearlyFields .AND. genperiod.NE.-12 ) THEN
C     Complete filename with YR or _YEAR extension
       il = ILNBLNK( genfile )
       IF ( twoDigitYear) THEN
        yearLoc = year-1900
        IF ( year.GE.2000 ) yearLoc = year - 2000
        WRITE(genfileout(1:128),'(A,I2.2)') genfile(1:il), yearLoc
       ELSE
        WRITE(genfileout(1:128),'(2A,I4.4)') genfile(1:il), '_', year
       ENDIF
      ELSE
       genfileout = genfile
      ENDIF

#endif /* ALLOW_EXF */
      RETURN
      END
