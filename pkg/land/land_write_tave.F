C $Header: /u/gcmpack/MITgcm/pkg/land/Attic/land_write_tave.F,v 1.2 2004/01/18 22:49:52 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_WRITE_TAVE`
C     !INTERFACE:
      SUBROUTINE LAND_WRITE_TAVE( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_WRITE_TAVE
C     | o Write out Land time-average output
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
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C Common (shared with RD_WR_FLD routines in read_write_fld.F)
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile 

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

C     == Local variables ==
      INTEGER bi, bj, K
      CHARACTER*(MAX_LEN_MBUF) msgBuf, suff, fn
      LOGICAL gf
      gf = globalFile

#ifdef ALLOW_LAND_TAVE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (land_taveFreq.LE.0.) RETURN

      IF ( myIter.EQ.nIter0 ) THEN
C      Initialize time-average arrays to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(land_grTtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
         DO k=1,Nr
           land_timeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
      ELSEIF (
     &  DIFFERENT_MULTIPLE(land_taveFreq, myTime, myTime-land_deltaT)
     &       ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(land_grTtave,land_timeAve,land_nLev,
     &                         bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_grWtave,land_timeAve,land_nLev,
     &                         bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(land_ROftave,land_timeAve,1,bi,bj,myThid)
        ENDDO
       ENDDO

       _BARRIER
       _BEGIN_MASTER( myThid )

        WRITE(fn,'(A,I10.10)') 'landgTtave.',myIter
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',land_nLev,land_grTtave, 1,myIter,myThid)
        WRITE(fn,'(A,I10.10)') 'landgWtave.',myIter
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',land_nLev,land_grWtave, 1,myIter,myThid)
        WRITE(fn,'(A,I10.10)') 'landROtave.',myIter
        CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',        1,land_ROftave, 1,myIter,myThid)

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
         CALL TIMEAVE_RESET(land_grWtave,land_nLev, bi, bj, myThid)
         CALL TIMEAVE_RESET(land_ROftave,        1, bi, bj, myThid)
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
