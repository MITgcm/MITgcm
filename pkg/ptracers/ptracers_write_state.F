C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_state.F,v 1.21 2014/04/04 19:34:00 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_WRITE_STATE

C     !INTERFACE:
      SUBROUTINE PTRACERS_WRITE_STATE(
     I     myIter, myTime, myThid )

C     !DESCRIPTION:
C     dump to files current passive tracer state

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C     !INPUT PARAMETERS:
C     myIter    :: time-step number
C     myTime    :: model time
C     myThid    :: thread number
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !LOCAL VARIABLES:
C     iTracer   :: loop indices
C     pref      :: character buffer for creating prefix in filename
C     iRec      :: record number
      INTEGER iTracer
      CHARACTER*(MAX_LEN_FNAM) pref
      INTEGER iRec
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

      IF ( DIFFERENT_MULTIPLE( PTRACERS_dumpFreq, myTime, deltaTClock )
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN

C--   Write to files
       _BARRIER

#ifdef ALLOW_MNC
        IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
          pf(1:1) = 'D'
        ELSE
          pf(1:1) = 'R'
        ENDIF

       IF ( PTRACERS_snapshot_mnc ) THEN
        CALL MNC_CW_SET_UDIM('ptracers', -1, myThid)
        CALL MNC_CW_RL_W_S('D','ptracers',0,0,'T',myTime,myThid)
        CALL MNC_CW_SET_UDIM('ptracers', 0, myThid)
        CALL MNC_CW_I_W_S('I','ptracers',0,0,'iter',myIter,myThid)
        DO iTracer = 1,PTRACERS_numInUse
         CALL MNC_CW_RL_W(pf,'ptracers',0,0,PTRACERS_names(iTracer),
     &        pTracer(1-OLx,1-OLy,1,1,1,iTracer),myThid)
        ENDDO
       ENDIF
#endif /*  ALLOW_MNC  */

       IF ( PTRACERS_snapshot_mdsio ) THEN

C       Loop over tracers
        DO iTracer=1,PTRACERS_numInUse

#ifdef MULTIPLE_RECORD_STATE_FILES
C     Write each snap-shot as a record in a time-history file
         IF (PTRACERS_useRecords) THEN
C     Write each passive tracer as a record
          WRITE(pref,'(A)') 'PTRACERS'
          iRec = 1 + NINT( (myTime-startTime) / PTRACERS_dumpFreq )
          iRec = (iRec-1)*PTRACERS_num + iTracer + 1
         ELSE
C     Write each passive tracer in its own file
          WRITE(pref,'(2A)') 'PTRACER',PTRACERS_ioLabel(iTracer)
          iRec = 1 + NINT( (myTime-startTime) / PTRACERS_dumpFreq )
         ENDIF
#else
C     Write each snap-shot as a new file with a 10-digit suffix to
C     indicate time-step number
         IF (PTRACERS_useRecords) THEN
C     Write each passive tracer as a record
          WRITE(pref,'(A,I10.10)') 'PTRACERS.',myIter
          iRec=iTracer
         ELSE
C     Write each passive tracer in its own file
          WRITE(pref,'(3A,I10.10)')
     &         'PTRACER',PTRACERS_ioLabel(iTracer),'.',myIter
          iRec=1
         ENDIF
#endif /* MULTIPLE_RECORD_STATE_FILES */

#ifdef ALLOW_OPENAD
         CALL WRITE_REC_XYZ_RL(pref,
     &        pTracer(:,:,:,:,:,iTracer)%v,iRec,myIter,myThid)
#else
         CALL WRITE_REC_XYZ_RL(pref,
     &        pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
#endif

C     End of tracer loop
        ENDDO

       ENDIF

       _BARRIER
C--   Writing to files: end
      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
