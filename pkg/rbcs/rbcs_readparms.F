C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_readparms.F,v 1.6 2010/04/06 20:38:18 jmc Exp $
C $Name:  $

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
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_RBCS

C     === Local variables ===
C     msgBuf      :: Informational/error message buffer
C     iUnit       :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER irbc
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif
C--   useRBCptracers is no longer used
      LOGICAL useRBCptracers
CEOP

C--   RBCS parameter
      NAMELIST /RBCS_PARM01/
     &          tauRelaxT,
     &          tauRelaxS,
     &          relaxMaskFile,
     &          relaxTFile,
     &          relaxSFile,
     &          useRBCtemp,
     &          useRBCsalt,
     &          useRBCptracers,
     &          rbcsIniter,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle

#ifdef ALLOW_PTRACERS
      NAMELIST /RBCS_PARM02/
     &          useRBCptrnum, tauRelaxPTR,
     &          relaxPtracerFile
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Default values
      useRBCtemp =.FALSE.
      useRBCsalt =.FALSE.
      tauRelaxT = 0.
      tauRelaxS = 0.
      DO irbc=1,maskLEN
        relaxMaskFile(irbc) = ' '
      ENDDO
      relaxTFile = ' '
      relaxSFile = ' '
      rbcsIniter = nIter0
      rbcsForcingPeriod = 0. _d 0
      rbcsForcingCycle  = 0. _d 0
#ifdef ALLOW_PTRACERS
      DO iTracer=1,PTRACERS_num
        useRBCptrnum(iTracer)=.FALSE.
        tauRelaxPTR(iTracer) = 0.
        relaxPtracerFile(iTracer) = ' '
      ENDDO
#endif
      useRBCptracers=.FALSE.

C Open and read the data.rbcs file

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

C Close the open data file
      CLOSE(iUnit)

      IF ( useRBCtemp .AND. tauRelaxT.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxT cannot be zero with useRBCtemp'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R RBCS_READPARMS'
      ENDIF
      IF ( useRBCsalt .AND. tauRelaxS.LE.0. ) THEN
        WRITE(msgBuf,'(2A)') 'RBCS_READPARMS: ',
     &    'tauRelaxS cannot be zero with useRBCsalt'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R RBCS_READPARMS'
      ENDIF
#ifdef ALLOW_PTRACERS
      DO iTracer=1,PTRACERS_num
       IF ( useRBCptrnum(iTracer) ) THEN
        IF ( .NOT.usePTRACERS ) THEN
         WRITE(msgBuf,'(2A,I6,A)') 'RBCS_READPARMS: ',
     &   'usePTRACERS=F => cannot use RBCS for tracer:', iTracer
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R RBCS_READPARMS'
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
         STOP 'ABNORMAL END: S/R RBCS_READPARMS'
        ENDIF
       ENDIF
      ENDDO
#endif
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_RBCS */

      RETURN
      END
