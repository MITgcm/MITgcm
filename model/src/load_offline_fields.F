C $Header: /u/gcmpack/MITgcm/model/src/Attic/load_offline_fields.F,v 1.2 1998/04/24 02:05:41 cnh Exp $

#include "CPP_EEOPTIONS.h"
 
CStartOfInterface
      SUBROUTINE LOAD_OFFLINE_FIELDS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE LOAD_OFFLINE_FIELDS                           |
C     | o Control reading of fields for offline experiment.      |
C     |==========================================================|
C     | Offline experiment flow field and other field loading    |
C     | routine. This routine is called every time we want to    |
C     | load a a set of offlinhe fields. The routine decides     |
C     | which fields to load and then reads them in.             |
C     | Notes                                                    |
C     | =====                                                    |
C     | Two-dimensional and three-dimensional I/O are handled in |
C     | the following way under MITgcmUV. A master thread        |
C     | performs I/O using system calls. This threads reads data |
C     | into a temporary buffer. At present the buffer is loaded |
C     | with the entire model domain. This is probably OK for now|
C     | Each thread then copies data from the buffer to the      |
C     | region of the proper array it is responsible for.        |
C     \==========================================================/
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation's time
C     myIter - Simulation's iteration number
      INTEGER myThid
      REAL    myTime
      INTEGER myIter
CEndOfInterface
 
C     === Local variables ===
C     msgBuf      - Error message buffer
C     filNam      - Parameter used to build file names.
C     oPeriodDist - Distance in time units along offline period
C     oBand       - Offline band into which we fall
C     I           - Loop counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fSuff
      REAL oPeriodDist
      INTEGER oBand
      INTEGER I

C--   Calculate the suffix to use.
C     Get distance through pickup period
      oPeriodDist = MOD(myTime,oPeriodLength)
C     Find band into which this distance falls
C     Bands are defined lo1:hi1, lo2:hi2, ... , loN:hiN
      oBand = 0
      DO I =1, nOBands
       IF ( oPeriodDist .GE. oBandBeg(I) .AND.
     &      oPeriodDist .LT. oBandEnd(I) ) THEN
        oBand = I
       ENDIF
      ENDDO
      IF ( oBand .EQ. 0 ) THEN
       _BEGIN_MASTER(myThid)
         WRITE(msgBuf,'(A)') 
     &   'S/R LOAD_OFFLINE_FIELDS: No offline fields for this period'
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A,F)') 'S/R LOAD_OFFLINE_FIELDS: Time = ', myTime
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A,I)') 'S/R LOAD_OFFLINE_FIELDS: myThid = ', myThid
         CALL PRINT_ERROR( msgBuf , myThid)
       _END_MASTER(myThid)
      ENDIF
C     Use barrier here to make sure an error message is generated
      _BARRIER
      IF ( oBand .EQ. 0 ) THEN
       fatalError = .TRUE.
       STOP 'ABNORMAL END: S/R LOAD_OFFLINE_FIELDS'
      ENDIF

C--   Log the data loading and load data
      _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A,A)' ) '// Loading offline data for ',oBandId(oBand)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A,F)' ) '//        time = ',myTime
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A,I)' ) '//   iteration = ',myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A)' ) ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , myThid)
      _END_MASTER(myThid)

      fSuff = oBandId(oBand)
      CALL GET_OFFLINE_FIELDS( myThid, fSuff )
      _BARRIER

      END
