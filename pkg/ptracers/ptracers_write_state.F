C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_state.F,v 1.2 2003/09/25 03:01:59 dimitri Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_WRITE_STATE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_WRITE_STATE( myIter,myTime,myThid )

C !DESCRIPTION:
C     Calculates tendancy for passive tracers and integrates forward
C     in time.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer		:: loop indices
C  suff			:: character buffer for creating suffix in filename
C  iRec			:: record number
      INTEGER iTracer
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iRec
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

#ifdef MULTIPLE_RECORD_STATE_FILES
C Write each snap-shot as a record in a time-history file
        IF (PTRACERS_useRecords) THEN
C        Write each passive tracer as a record
         WRITE(suff,'(A)') 'PTRACERS'
         iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
         iRec = (iRec-1)*PTRACERS_num + iTracer + 1
        ELSE
C        Write each passive tracer in its own file
         WRITE(pref,'(A7,I2.2)') 'PTRACER',iTracer
         iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
        ENDIF
#else
C Write each snap-shot as a new file with a 10-digit suffix to
C indicate time-step number
        IF (PTRACERS_useRecords) THEN
C        Write each passive tracer as a record
         WRITE(suff,'(A,I10.10)') 'PTRACERS',myIter
         iRec=iTracer
        ELSE
C        Write each passive tracer in its own file
         WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PTRACER',iTracer,'.',myIter
         iRec=1
        ENDIF
#endif /* MULTIPLE_RECORD_STATE_FILES */

        CALL WRITE_REC_XYZ_RL(suff,
     &      pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
