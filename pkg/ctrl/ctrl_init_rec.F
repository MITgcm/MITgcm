#include "CTRL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CTRL_INIT_REC(
     I     fldname,
     I     fldstartdate1, fldstartdate2, fldperiod, nfac,
     O     fldstartdate, diffrec, startrec, endrec,
     I     myThid )

C     ==================================================================
C     SUBROUTINE CTRL_INIT_REC
C     ==================================================================
C
C     helper routine to compute the first and last record of a
C     time dependent control variable
C
C     Martin.Losch@awi.de, 2011-Mar-15
C
C     ==================================================================
C     SUBROUTINE CTRL_INIT_REC
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     == input variables ==
C     fldstartdate1/2 :: start time (date/time) of fld
C     fldperod        :: sampling interval of fld
C     nfac            :: factor for the case that fld is an obcs variable
C                        in this case nfac = 4, otherwise nfac = 1
C     myThid          :: my thread Id number
      CHARACTER*(*) fldname
      INTEGER fldstartdate1
      INTEGER fldstartdate2
      _RL     fldperiod
      INTEGER nfac
      INTEGER myThid

C     == output variables ==
C     fldstartdate :: full date from fldstartdate1 and 2
C     startrec     :: first record of ctrl variable
C     startrec     :: last record of ctrl variable
C     diffrec      :: difference between first and last record of ctrl variable
      INTEGER fldstartdate(4)
      INTEGER startrec
      INTEGER endrec
      INTEGER diffrec

C     == functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER i
#ifdef ALLOW_CAL
      INTEGER difftime(4)
      INTEGER modelBaseDate(4)
      _RL     diffsecs
#endif /* ALLOW_CAL */
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER il

      IF ( debugLevel .GE. debLevB ) THEN
       il = ILNBLNK(fldname)
       WRITE( msgBuf,'(A,A)')
     &     'CTRL_INIT_REC: Getting record indices for ',fldname(1:il)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C     initialise some output
      DO i = 1,4
       fldstartdate(i) = 0
      ENDDO
      startrec = 0
      endrec   = 0
      diffrec  = 0
      IF ( fldperiod .EQ. -12. ) THEN
       startrec = 1
       endrec   = 12*nfac
      ELSEIF ( fldperiod .EQ. 0. ) THEN
       startrec = 1
       endrec   = 1*nfac
# ifdef ALLOW_CAL
      ELSEIF ( useCAL ) THEN
       CALL cal_FullDate( startdate_1, startdate_2,
     &                        modelBaseDate , myThid )
       CALL cal_FullDate( fldstartdate1, fldstartdate2,
     &                        fldstartdate , myThid )
       CALL cal_TimePassed( modelBaseDate, fldstartdate,
     &                           difftime, myThid )
       CALL cal_ToSeconds ( difftime, diffsecs, myThid )
       startrec = int((modelstart - diffsecs)/fldperiod) + 1
       endrec   = int((modelend   - diffsecs + modelstep/2)
     &                                       /fldperiod) + 2
       IF ( nfac .NE. 1 ) THEN
C     This is the case of obcs.
        startrec = (startrec - 1)*nfac + 1
        endrec   = endrec*nfac
       ENDIF
#endif /* ALLOW_CAL */
      ELSE
       startrec = 1
       endrec   = (int((endTime - startTime)/fldperiod) + 1)*nfac
      ENDIF
      diffrec  = endrec - startrec + 1

      IF ( debugLevel .GE. debLevB ) THEN
       WRITE( msgBuf,'(A,A,A)')
     &      'CTRL_INIT_REC: Record indices for ',fldname(1:il),':'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE( msgBuf,'(A,I10,A,I10)')
     &      'CTRL_INIT_REC: startrec = ',startrec,', endrec = ',endrec
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

      RETURN
      END
