#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PABAR_OUTPUT

C !INTERFACE:
      SUBROUTINE PABAR_OUTPUT( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PABAR_OUTPUT
C     | o Output the global mean air pressure.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
#include "ecco.h"
#endif
#endif

C !INPUT PARAMETERS:
C     myTime    :: my time in simulation ( s )
C     myIter    :: my Iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR

C !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES:
C     fName     :: output file name
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER narr
      PARAMETER( narr = 1 )
      INTEGER irecord
      INTEGER ioUnit
      _RS     dummyRS(1)

C-----------------------------------------------------------------
C     Save the global mean pabar at every time step
C-----------------------------------------------------------------

      irecord      = myIter - nIter0 + 1

#ifdef ALLOW_MDSIO
      _BEGIN_MASTER(myThid)
C-    multi-threaded: only write it once (by master thread)
       IF ( rwSuffixType.EQ.0 ) THEN
         WRITE(fName,'(A,I10.10)') 'Pa_global.', nIter0
       ELSE
         CALL RW_GET_SUFFIX( suff, startTime, nIter0, myThid )
         WRITE(fName,'(A,A)') 'Pa_global.', suff
       ENDIF
       ioUnit  = 0
       CALL MDS_WRITEVEC_LOC(
     I           fName, precFloat64, ioUnit,
     I           'RL', narr, ploadbar, dummyRS,
     I           0, 0, irecord, myIter, myThid )
      _END_MASTER(myThid)
#endif /* ALLOW_MDSIO */

#endif /* ALLOW_ETA_IB */
#endif /* ATMOSPHERIC_LOADING */ 

      RETURN
      END
