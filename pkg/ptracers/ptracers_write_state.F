C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_state.F,v 1.4 2004/09/03 20:10:47 edhill Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_WRITE_STATE

C     !INTERFACE:
      SUBROUTINE PTRACERS_WRITE_STATE( myIter,myTime,myThid )

C     !DESCRIPTION:
C     Calculates tendancy for passive tracers and integrates forward in
C     time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C     !INPUT PARAMETERS:
C     myIter    :: time-step number
C     myTime    :: model time
C     myThid    :: thread number
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer   :: loop indices
C     suff      :: character buffer for creating suffix in filename
C     iRec      :: record number
      INTEGER iTracer
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iRec
#ifdef ALLOW_MNC
      INTEGER ii

      IF (useMNC
     &     .AND. (mod(PTRACERS_iotypes,4) .GT. 1)
     &     .AND. PTRACERS_mnc_write ) THEN
        CALL MNC_CW_SET_UDIM('ptracers', -1, myThid)
        CALL MNC_CW_I_W('I','ptracers',0,0,'iter',myIter,myThid)
        CALL MNC_CW_SET_UDIM('ptracers', 0, myThid)
        DO ii = 1,PTRACERS_numInUse
CEH3          WRITE(*,*) 'PTRACERS_names(',ii,') = ''',
CEH3     &         PTRACERS_names(ii),''''
          CALL MNC_CW_RL_W('D','ptracers',0,0,PTRACERS_names(ii),
     &         pTracer(1-OLx,1-OLy,1,1,1,ii),myThid)
        ENDDO
      ENDIF
#endif /*  ALLOW_MNC  */

      IF (mod(PTRACERS_iotypes,2) .GT. 0) THEN
C       use MDSIO

C       Loop over tracers
        DO iTracer=1,PTRACERS_numInUse

#ifdef MULTIPLE_RECORD_STATE_FILES
C         Write each snap-shot as a record in a time-history file
          IF (PTRACERS_useRecords) THEN
C           Write each passive tracer as a record
            WRITE(suff,'(A)') 'PTRACERS'
            iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
            iRec = (iRec-1)*PTRACERS_num + iTracer + 1
          ELSE
C           Write each passive tracer in its own file
            WRITE(pref,'(A7,I2.2)') 'PTRACER',iTracer
            iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
          ENDIF
#else
C         Write each snap-shot as a new file with a 10-digit suffix to
C         indicate time-step number
          IF (PTRACERS_useRecords) THEN
C           Write each passive tracer as a record
            WRITE(suff,'(A,I10.10)') 'PTRACERS',myIter
            iRec=iTracer
          ELSE
C           Write each passive tracer in its own file
            WRITE(suff,'(A7,I2.2,A1,I10.10)') 
     &           'PTRACER',iTracer,'.',myIter
            iRec=1
          ENDIF
#endif /* MULTIPLE_RECORD_STATE_FILES */

          CALL WRITE_REC_XYZ_RL(suff,
     &         pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
          
C         End of tracer loop
        ENDDO
        
      ENDIF
#endif /* ALLOW_PTRACERS */

      RETURN
      END
