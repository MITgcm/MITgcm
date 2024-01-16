#include "ECCO_OPTIONS.h"

      SUBROUTINE ECCO_SUMMARY( myThid )

c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================
c
c     o Summarize the setup of the ECCO release.
c
c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==
c     myThid  :: my Thread Id number
      INTEGER myThid

c     == external ==
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

c     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER msgUnit
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER k, k2, il
#endif

c     == end of interface ==

      msgUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ECCO configuration >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO k = 1, NGENCOST
       IF (using_gencost(k)) THEN

        il = ILNBLNK(gencost_name(k))
        WRITE(msgBuf,'(A,I2,2A)') 'gencost(',k,
     &        ') = ',gencost_name(k)(1:il)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)') '-------------'
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

        IF ( gencost_datafile(k) .NE. ' ' ) THEN
         il = ILNBLNK(gencost_datafile(k))
         WRITE(msgBuf,'(2A)') ' data file = ',gencost_datafile(k)(1:il)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

        IF (gencost_nrec(k).GT.1) THEN
         WRITE(msgBuf,'(A)') '  starts and ends at : '
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
         CALL CAL_PRINTDATE( gencost_startdate(1,k), myThid )
         CALL CAL_PRINTDATE( gencost_enddate(1,k), myThid )
         WRITE(msgBuf,'(A,I5)') '  number of records = ',
     &                           gencost_nrec(k)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

        il = ILNBLNK(gencost_barfile(k))
        WRITE(msgBuf,'(2A)') ' model file = ',gencost_barfile(k)(1:il)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        IF ((gencost_barfile(k)(1:9).EQ.'m_ptracer').OR.
     &      (gencost_barfile(k)(1:17).EQ.'m_boxmean_ptracer')) THEN
         WRITE(msgBuf,'(A,I2)') ' gencost_itracer = ',
     &         gencost_itracer(k)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        il = ILNBLNK(gencost_errfile(k))
        WRITE(msgBuf,'(2A)') ' error file = ',gencost_errfile(k)(1:il)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        IF (gencost_scalefile(k).NE.' ') THEN
         il = ILNBLNK(gencost_scalefile(k))
         WRITE(msgBuf,'(2A)') ' scale file = ',
     &        gencost_scalefile(k)(1:il)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        DO k2 = 1, NGENPPROC
         IF (gencost_preproc(k2,k).NE.' ') THEN
          il = ILNBLNK(gencost_preproc(k2,k))
          WRITE(msgBuf,'(2A)') ' preprocess = ',
     &                          gencost_preproc(k2,k)(1:il)
          CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         IF (gencost_posproc(k2,k).NE.' ') THEN
          il = ILNBLNK(gencost_posproc(k2,k))
          WRITE(msgBuf,'(2A)') ' posprocess = ',
     &                          gencost_posproc(k2,k)(1:il)
          CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDDO

        WRITE(msgBuf,'(A,I2)') ' gencost_flag = ',gencost_flag(k)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I2)') ' gencost_outputlevel = ',
     &        gencost_outputlevel(k)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I2)') ' gencost_kLev_select = ',
     &        gencost_kLev_select(k)
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

        IF (gencost_is3d(k)) THEN
         WRITE(msgBuf,'(A,I2)') ' gencost_pointer3d = ',
     &         gencost_pointer3d(k)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        IF (gencost_timevaryweight(k)) THEN
         WRITE(msgBuf,'(A,L2)') ' time varia. weight = ',
     &         gencost_timevaryweight(k)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        IF (gencost_barskip(k)) THEN
         WRITE(msgBuf,'(A,L2)') ' skip barfile WRITE = ',
     &         gencost_barskip(k)
         CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

       ENDIF
      ENDDO
c--
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ECCO configuration  >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, msgUnit, SQUEEZE_RIGHT, myThid )

      RETURN
      END
