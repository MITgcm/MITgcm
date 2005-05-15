C $Header: /u/gcmpack/MITgcm/pkg/land/Attic/land_write_diags.F,v 1.5 2005/05/15 03:04:56 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_WRITE_DIAGS
C     !INTERFACE:
      SUBROUTINE LAND_WRITE_DIAGS( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_WRITE_DIAGS
C     | o Write out Land diagnostics (time-average output)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "LAND_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_TAVE.h"
#ifdef ALLOW_AIM
#include "AIM_FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local variables ==
      INTEGER bi, bj, k, nrec
      CHARACTER*(MAX_LEN_MBUF) msgBuf, suff, fn
      LOGICAL gf

#ifdef ALLOW_AIM
      IF ( land_monFreq.NE.0. ) THEN
        CALL LAND_MONITOR( aim_landFr, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_LAND_TAVE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (land_taveFreq.LE.0.) RETURN

      IF ( myIter.EQ.nIter0 ) THEN
C      Initialize time-average arrays to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(land_grTtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_entave, land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sTtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_hStave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sAtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_eROtave,        1, bi, bj, myThid)
         DO k=1,Nr
           land_timeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
      ELSEIF (
     &  DIFFERENT_MULTIPLE(land_taveFreq,myTime,land_deltaT)
     &       ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(land_grTtave,land_timeAve,land_nLev,
     &                         bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_entave, land_timeAve,land_nLev,
     &                         bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_grWtave,land_timeAve,land_nLev,
     &                         bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_sTtave, land_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_hStave, land_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_sAtave, land_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_ROftave,land_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_eROtave,land_timeAve,1,bi,bj,myThid)
        ENDDO
       ENDDO

       _BARRIER
       _BEGIN_MASTER( myThid )

C-     find wether we are writing globalFile or tile-files:
        CALL GET_WRITE_GLOBAL_FLD( gf )

        WRITE(fn,'(A,I10.10)') 'land_tave.', myIter
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',land_nLev,land_grTtave, 1, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',land_nLev,land_entave,  2, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',land_nLev,land_grWtave, 3, myIter,myThid)
        nrec = 3*land_nLev
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL', 1 , land_sTtave,  1+nrec, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL', 1 , land_hStave,  2+nrec, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL', 1 , land_sAtave,  3+nrec, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL', 1 , land_ROftave, 4+nrec, myIter,myThid)
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL', 1 , land_eROtave, 5+nrec, myIter,myThid)

       WRITE(msgBuf,'(A,I10)')
     &  '// Land Time-average  written, t-step', myIter
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )         
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )         

       _END_MASTER( myThid )
       _BARRIER

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(land_grTtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_entave, land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sTtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_hStave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_sAtave,         1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_eROtave,        1, bi, bj, myThid)
         DO k=1,Nr
           land_timeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_LAND_TAVE */

#endif /* ALLOW_LAND */

      RETURN
      END
