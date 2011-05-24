C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_output.F,v 1.4 2011/05/24 14:31:14 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_OUTPUT

C     !INTERFACE:
      SUBROUTINE OBCS_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_OUTPUT
C     | o General routine for OBCS output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_FIELDS.h"

C     !INPUT PARAMETERS:
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &  DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &  .AND. OBCSprintDiags
     &  .AND. myIter.NE.nIter0
     & ) THEN

C--   Generaly only thread 1 does IO here. It can not start until
C--   all threads fields are ready.
       _BARRIER

#ifdef ALLOW_OBCS_NORTH
        CALL PLOT_FIELD_XZRL( OBNu, 'OBNu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNv, 'OBNv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNt, 'OBNt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBNs, 'OBNs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_SOUTH
        CALL PLOT_FIELD_XZRL( OBSu, 'OBSu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSv, 'OBSv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSt, 'OBSt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_XZRL( OBSs, 'OBSs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_EAST
        CALL PLOT_FIELD_YZRL( OBEu, 'OBEu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEv, 'OBEv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEt, 'OBEt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBEs, 'OBEs' , Nr, myIter, mythid )
#endif

#ifdef ALLOW_OBCS_WEST
        CALL PLOT_FIELD_YZRL( OBWu, 'OBWu' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWv, 'OBWv' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWt, 'OBWt' , Nr, myIter, mythid )
        CALL PLOT_FIELD_YZRL( OBWs, 'OBWs' , Nr, myIter, mythid )
#endif

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write OB aray to binary files (mainly for debugging => use "diagFreq")
      IF (
     &     DIFFERENT_MULTIPLE(diagFreq,myTime,deltaTClock)
     &  .AND. myIter.NE.nIter0
     &   ) THEN

       _BARRIER
        prec = writeBinaryPrec

#ifdef ALLOW_OBCS_NORTH
C     Write Northern OB arrays
        WRITE(fn,'(A,I10.10)') 'obcs_N.', myIter
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNu,1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNv,2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNt,3,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNs,4,myIter,myThid)
# ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNw,5,myIter,myThid)
       ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_SOUTH
C     Write Southern OB arrays
        WRITE(fn,'(A,I10.10)') 'obcs_S.', myIter
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSu,1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSv,2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSt,3,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSs,4,myIter,myThid)
# ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSw,5,myIter,myThid)
       ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_EAST
C     Write Eastern OB arrays
        WRITE(fn,'(A,I10.10)') 'obcs_E.', myIter
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEu,1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEv,2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEt,3,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEs,4,myIter,myThid)
# ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEw,5,myIter,myThid)
       ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_WEST
C     Write Western OB arrays
        WRITE(fn,'(A,I10.10)') 'obcs_W.', myIter
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWu,1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWv,2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWt,3,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWs,4,myIter,myThid)
# ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWw,5,myIter,myThid)
       ENDIF
# endif
#endif

       _BARRIER

      ENDIF

C--   Compute and print staicstics of dynamic fields at OB:
      CALL OBCS_MONITOR( myTime, myIter, myThid )

#endif /* ALLOW_OBCS */

      RETURN
      END
