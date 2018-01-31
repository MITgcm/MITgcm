#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
#include "OBCS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_OBCS_OUTPUT

C     !INTERFACE:
      SUBROUTINE SEAICE_OBCS_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_OBCS_OUTPUT
C     | o General routine for SEAICE Open-Boundary output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_OBCS
# include "OBCS_PARAMS.h"
# include "OBCS_SEAICE.h"
#endif

C     !INPUT PARAMETERS:
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SEAICE
#ifdef ALLOW_OBCS
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write OB aray to binary files (mainly for debugging => use "diagFreq")
      IF (
     &     DIFFERENT_MULTIPLE(diagFreq,myTime,deltaTClock)
     &   ) THEN

       _BARRIER
        prec = writeBinaryPrec
c       IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
c       ELSE
c         CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
c       ENDIF

C     Write Sea-Ice OB arrays
# ifdef ALLOW_OBCS_NORTH
        WRITE(fn,'(2A)') 'seaice_ob_N.', suff
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNa,    1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNh,    2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNsl,   3,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNsn,   4,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNuice, 5,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNvice, 6,myIter,myThid)
# endif
# ifdef ALLOW_OBCS_SOUTH
        WRITE(fn,'(2A)') 'seaice_ob_S.', suff
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSa,    1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSh,    2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSsl,   3,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSsn,   4,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSuice, 5,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSvice, 6,myIter,myThid)
# endif
# ifdef ALLOW_OBCS_EAST
        WRITE(fn,'(2A)') 'seaice_ob_E.', suff
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEa,    1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEh,    2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEsl,   3,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEsn,   4,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEuice, 5,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEvice, 6,myIter,myThid)
# endif
# ifdef ALLOW_OBCS_WEST
        WRITE(fn,'(2A)') 'seaice_ob_W.', suff
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWa,    1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWh,    2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWsl,   3,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWsn,   4,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWuice, 5,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWvice, 6,myIter,myThid)
# endif

       _BARRIER

      ENDIF

#endif /* ALLOW_OBCS */
#endif /* ALLOW_SEAICE */

      RETURN
      END
