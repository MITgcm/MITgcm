#include "RBCS_OPTIONS.h"

CBOP
C !ROUTINE: RBCS_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize RBCS parameters, read in data.rbcs

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: my thread Id. number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_RBCS

C     === Local variables ===
C     msgBuf      :: Informational/error message buffer
C     iUnit       :: Work variable for IO unit number
C     iTracer     :: passive tracer index
C     relaxMaskFile :: local mask-file name to read from namelist
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER irbc, errCount
      INTEGER locSize
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
      PARAMETER( locSize = 10+PTRACERS_num )
#else
      PARAMETER( locSize = 4 )
#endif
      CHARACTER*(MAX_LEN_FNAM) relaxMaskFile(locSize)
C--   useRBCptracers is no longer used
      LOGICAL useRBCptracers
      INTEGER rbcsIniter
CEOP

C--   RBCS parameters:
      NAMELIST /RBCS_PARM01/
     &          tauRelaxU,
     &          tauRelaxV,
     &          tauRelaxT,
     &          tauRelaxS,
     &          relaxMaskUFile,
     &          relaxMaskVFile,
     &          relaxMaskFile,
     &          relaxUFile,
     &          relaxVFile,
     &          relaxTFile,
     &          relaxSFile,
     &          useRBCuVel,
     &          useRBCvVel,
     &          useRBCtemp,
     &          useRBCsalt,
     &          useRBCptracers,
     &          rbcsIniter,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle,
     &          rbcsForcingOffset,
     &          rbcsVanishingTime,
     &          rbcsSingleTimeFiles,
     &          deltaTrbcs,
     &          rbcsIter0

#ifdef ALLOW_PTRACERS
      NAMELIST /RBCS_PARM02/
     &          useRBCpTrNum, tauRelaxPTR,
     &          relaxPtracerFile
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useRBCS ) THEN
C-    pkg RBCS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.rbcs is found
         CALL PACKAGES_UNUSED_MSG( 'useRBCS', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values
      useRBCuVel =.FALSE.
      useRBCvVel =.FALSE.
      useRBCtemp =.FALSE.
      useRBCsalt =.FALSE.
      tauRelaxU = 0.
      tauRelaxV = 0.
      tauRelaxT = 0.
      tauRelaxS = 0.
      relaxMaskUFile = ' '
      relaxMaskVFile = ' '
      DO irbc=1,locSize
        relaxMaskFile(irbc) = ' '
      ENDDO
      relaxUFile = ' '
      relaxVFile = ' '
      relaxTFile = ' '
      relaxSFile = ' '
      rbcsIniter = 0
      rbcsForcingPeriod = 0. _d 0
      rbcsForcingCycle  = 0. _d 0
      rbcsForcingOffset = 0. _d 0
      rbcsVanishingTime = 0. _d 0
      rbcsSingleTimeFiles = .FALSE.
      deltaTrbcs = deltaTClock
      rbcsIter0 = 0
#ifdef ALLOW_PTRACERS
      DO iTracer=1,PTRACERS_num
        useRBCpTrNum(iTracer)=.FALSE.
        tauRelaxPTR(iTracer) = 0.
        relaxPtracerFile(iTracer) = ' '
      ENDDO
#endif
      useRBCptracers=.FALSE.

C--   Open and read the data.rbcs file

      WRITE(msgBuf,'(A)') ' RBCS_READPARMS: opening data.rbcs'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.rbcs', 'RBCS_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=RBCS_PARM01)
#ifdef ALLOW_PTRACERS
      READ(UNIT=iUnit,NML=RBCS_PARM02)
#endif
      WRITE(msgBuf,'(A)')
     &  ' RBCS_READPARMS: finished reading data.rbcs'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C-    save local mask-file name into relaxMaskTrFile (stored in common block)
      DO irbc=1,maskLEN
        relaxMaskTrFile(irbc) = ' '
      ENDDO
      errCount = 0
      DO irbc=1,locSize
        IF ( irbc.LE.maskLEN ) THEN
          relaxMaskTrFile(irbc) = relaxMaskFile(irbc)
        ELSEIF ( relaxMaskFile(irbc).NE.' ' ) THEN
          errCount = errCount + 1
        ENDIF
      ENDDO
      IF ( errCount.GT.0 ) THEN
        WRITE(msgBuf,'(2A,I6)') 'RBCS_READPARMS: ',
     &   'Too many "relaxMaskFile" are set ! exceeds maskLEN=', maskLEN
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &   '==> Increase maskLEN (in RBCS_SIZE.h) and recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C---  Check RBCS config and params:
#ifdef DISABLE_RBCS_MOM
      IF ( useRBCuVel .OR. useRBCvVel ) THEN
        WRITE(msgBuf,'(2A,2(L2,A))') 'RBCS_READPARMS: ',
     &    'cannot use RBC for U,V (useRBCuVel=',useRBCuVel,
     &    ', useRBCvVel=',useRBCvVel,')'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'when DISABLE_RBCS_MOM is defined (in RBCS_OPTIONS.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* DISABLE_RBCS_MOM */
      IF (rbcsIniter.NE.0) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &  'rbcsIniter has been replaced by rbcsForcingOffset '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &  'which is in seconds. Please change your data.rbcs'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ELSEIF (startTime.LT.rbcsForcingOffset+0.5*rbcsForcingPeriod .AND.
     &    .NOT. rbcsSingleTimeFiles) THEN
       IF (rbcsForcingCycle.GT.0.) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_READPARMS: ',
     &  'startTime before rbcsForcingOffset+0.5*rbcsForcingPeriod '
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_READPARMS: ',
     &  'will use last record'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ELSE
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &  'startTime before rbcsForcingOffset+0.5*rbcsForcingPeriod '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &  'not allowed with rbcsForcingCycle=0 unless rbcsSingleTimeFiles'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( useRBCuVel .AND. tauRelaxU.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxU cannot be zero with useRBCuVel'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useRBCvVel .AND. tauRelaxV.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxV cannot be zero with useRBCvVel'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useRBCtemp .AND. tauRelaxT.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxT cannot be zero with useRBCtemp'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useRBCsalt .AND. tauRelaxS.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxS cannot be zero with useRBCsalt'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#ifdef ALLOW_PTRACERS
      DO iTracer=1,PTRACERS_num
       IF ( useRBCpTrNum(iTracer) ) THEN
        IF ( .NOT.usePTRACERS ) THEN
         WRITE(msgBuf,'(2A,I6,A)') 'RBCS_READPARMS: ',
     &   'usePTRACERS=F => cannot use RBCS for tracer:', iTracer
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
c       IF ( iTracer.GT.PTRACERS_numInUse ) THEN
c        STOP 'ABNORMAL END: S/R RBCS_READPARMS'
c       ENDIF
        IF ( tauRelaxPTR(iTracer).LE.0. ) THEN
         WRITE(msgBuf,'(2A,I6,A)') 'RBCS_READPARMS: ',
     &     'tauRelaxPTR(itr=', iTracer, ' ) = 0. is'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I6,A)') 'RBCS_READPARMS: ',
     &     'not allowed with useRBCptr(itr)=T'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO
#endif
      IF ( errCount.GT.0 ) THEN
       WRITE(msgBuf,'(A,I3,A)')
     &       'RBCS_READPARMS: detected', errCount,' fatal error(s)'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R RBCS_READPARMS'
      ENDIF
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_RBCS */

      RETURN
      END
