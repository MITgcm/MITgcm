#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: STERGLOH_OUTPUT

C !INTERFACE:
      SUBROUTINE STERGLOH_OUTPUT( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE STERGLOH_OUTPUT
C     | o Ouput the global steric height change (Greatbatch correction)
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C !INPUT PARAMETERS:
C     myTime    :: my time in simulation ( s )
C     myIter    :: my Iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_ECCO
#ifdef ALLOW_PSBAR_STERIC

C !FUNCTIONS:
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE

C--   Local variables shared by S/R within this file (stergloh_output.F)
C     IOunit_outpFile :: IO-unit of binary output file
      INTEGER IOunit_outpFile
      COMMON /STERGLOH_OUTPUT_LOCAL/ IOunit_outpFile

C !LOCAL VARIABLES:
C     fName     :: output file name
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER irecord
      INTEGER ioUnit
      _RL     tmpVar(1)
      _RS     dummyRS(1)

C-----------------------------------------------------------------
C     Save the global mean steric heigt change at every time step
C-----------------------------------------------------------------

      IF ( ecco_output_sterGloH ) THEN
        irecord = myIter - nIter0 + 1

#ifdef ALLOW_MDSIO
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fName,'(A,I10.10)') 'sterGloH_global.', nIter0
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, nIter0, myThid )
          WRITE(fName,'(A,A)') 'sterGloH_global.', suff
        ENDIF

        IF ( ecco_keepTSeriesOutp_open ) THEN
          IF ( myIter .EQ. nIter0 ) THEN
C-    to open new IO unit and keep it open
            ioUnit = -1
          ELSE
C-    to write to already open IO unit
            ioUnit = IOunit_outpFile
          ENDIF
C-    skip writing meta file (unless last time to write to file)
          IF ( myIter .NE. nEndIter ) irecord = -irecord
        ELSE
C-    to open new IO unit, write and close it all within same call
          ioUnit  = 0
        ENDIF

        tmpVar(1) = sterGloH
        CALL MDS_WRITEVEC_LOC(
     I             fName, precFloat64, ioUnit,
     I             'RL', 1, tmpVar, dummyRS,
     I             0, 0, irecord, myIter, myThid )

        IF ( ecco_keepTSeriesOutp_open ) THEN
C-      multi-threaded: only master-thread save IO-unit to shared variable
C                 (in common block) and close open file (after last write)
          _BEGIN_MASTER(myThid)
          IF ( myIter .EQ. nIter0 ) THEN
C-      save for next write to same file:
            IOunit_outpFile = ioUnit
          ENDIF
          IF ( myIter .EQ. nEndIter .AND. ioUnit .GT. 0 ) THEN
C-      after last write, close IO-unit:
            IF ( debugLevel.GE.debLevC ) THEN
              WRITE(msgBuf,'(A,I8,3A)')
     &          ' STERGLOH_OUTPUT: close ioUnit=', ioUnit,
     &          ', file: ', fName(1:26), '.data'
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
            ENDIF
            CLOSE( ioUnit )
          ELSEIF ( myIter .EQ. nEndIter .AND. debugMode ) THEN
            WRITE(msgBuf,'(2A,I10,A)')
     &          ' STERGLOH_OUTPUT: no file to close',
     &          ' (ioUnit=', ioUnit, ' )'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          _END_MASTER(myThid)
        ENDIF

#endif /* ALLOW_MDSIO */
      ENDIF

#endif /* ALLOW_PSBAR_STERIC */
#endif /* ALLOW_ECCO */

      RETURN
      END
