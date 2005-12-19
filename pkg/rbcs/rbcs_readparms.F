#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

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
#include "GRID.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#endif
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_RBCS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER iTracer
      INTEGER ic
      INTEGER irbc
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

C--   Default values
      tauRelaxT =1.
      tauRelaxS =1.
      do irbc=1,maskLEN
        relaxMaskFile(irbc) = ' '
      enddo
      relaxTFile = ' '
      relaxSFile = ' '
      useRBCtemp=.FALSE.
      useRBCsalt=.FALSE.
      useRBCptracers=.FALSE.
      rbcsIniter=nIter0
      rbcsForcingPeriod=0.
      rbcsForcingCycle=0.
#ifdef ALLOW_PTRACERS
      DO iTracer=1,PTRACERS_num
        useRBCptrnum(iTracer)=.FALSE.
        tauRelaxPTR(iTracer)=1.
       DO ic = 1,MAX_LEN_FNAM
           relaxPtracerFile(iTracer)(ic:ic) = ' '
       ENDDO
      ENDDO
#endif


C Open and read the data.rbcs file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' RBCS_READPARMS: opening data.rbcs'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.rbcs', 'RBCS_PARM01',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=RBCS_PARM01)
#ifdef ALLOW_PTRACERS
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.rbcs', 'RBCS_PARM02',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=RBCS_PARM02)
#endif
      WRITE(msgBuf,'(A)')
     &  ' RBCS_READPARMS: finished reading data.rbcs'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)

      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_RBCS */

      RETURN
      END
