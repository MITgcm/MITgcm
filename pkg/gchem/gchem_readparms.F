#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize PTRACERS parameters, read in data.gchem

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer              :: loop indices
C  iUnit		:: unit number for I/O
C  msgBuf		:: message buffer
      INTEGER iTracer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C Set defaults values for parameters in GCHEM.h
       tIter0=0
       nsubtime=1
       windFile=' '
       atmospFile=' ' 
       IceFile=' '
       IronFile=' '
       SilicaFile=' '


C Open and read the data.ptracers file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' GCHEM_READPARMS: opening data.gchem'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.gchem', 'GCHEM_PARM01',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=GCHEM_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' GCHEM_READPARMS: finished reading data.gchem'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_PTRACERS */

      RETURN
      END
