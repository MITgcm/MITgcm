C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_output.F,v 1.2 2010/01/03 20:03:36 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_OUTPUT( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE SBO_OUTPUT
C     | o Do SBO diagnostic output.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SBO.h"

C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SBO

c     == Local variables ==
c     bi, bj    - loop counters
c     sbo_diag  - vector of SBO diagnostics contains:
c                 model time (s), xoamc, yoamc, zoamc,
c                 xoamp, yoamp, zoamp, mass, xcom, ycom, zcom
c     character*(8) fName
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER narr
      PARAMETER( narr = 15 )
      INTEGER irecord
c     Real*8   sbo_diag(15)
      INTEGER ioUnit
      _RL     sbo_diag(narr)
      _RS     dummyRS(1)
#ifdef ALLOW_TIMEAVE
      INTEGER bi, bj
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      _RL DDTT
      CHARACTER*(MAX_LEN_MBUF) suff
#endif

C-----------------------------------------------------------------
C     Save angular momentum and mass variables at every time step
C-----------------------------------------------------------------

      sbo_diag(1)  = myTime
      sbo_diag(2)  = xoamc
      sbo_diag(3)  = yoamc
      sbo_diag(4)  = zoamc
      sbo_diag(5)  = xoamp
      sbo_diag(6)  = yoamp
      sbo_diag(7)  = zoamp
      sbo_diag(8)  = mass
      sbo_diag(9)  = xcom
      sbo_diag(10) = ycom
      sbo_diag(11) = zcom
      sbo_diag(12) = sboarea
      sbo_diag(13) = sbobp
      sbo_diag(14) = sboempmrwet
      sbo_diag(15) = sboqnetwet

      irecord      = myIter - nIter0 + 1

c     fName        = 'SBO_DIAG'
c     CALL SBO_WRITEVECTOR(
c    I   fName,
c    I   narr,
c    I   sbo_diag,
c    I   irecord,
c    I   myIter,
c    I   myThid )

#ifdef ALLOW_MDSIO
      IF ( nThreads.EQ.1 ) THEN
C-    multi-threaded: avoid writing wrong results
C     (due to Pb with GLOBAL_SUM in sbo_calc.F, cf comments there)
       WRITE(fName,'(A,I10.10)') 'SBO_global.', nIter0
       ioUnit  = 0
       CALL MDS_WRITEVEC_LOC(
     I           fName, precFloat64, ioUnit,
     I           'RL', narr, sbo_diag, dummyRS,
     I           0, 0, irecord, myIter, myThid )
      ENDIF
#endif /* ALLOW_MDSIO */

#ifdef ALLOW_TIMEAVE
      IF ( sbo_taveFreq.GT.0. _d 0 ) THEN

C-----------------------------------------------------------------
C     Save time-averaged bottom pressure at sbo_taveFreq intervals
C-----------------------------------------------------------------

C     Initialize averages to zero
       IF ( myIter.EQ.nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET( OBPtave, 1, bi, bj, myThid )
            sbo_timeAve(bi,bj) = 0.
          ENDDO
         ENDDO
       ENDIF

C     Time Average SBO fields
       IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE(sbo_taveFreq,myTime,deltaTClock)
     &    ) THEN
         DDTT=0.5*deltaTclock
       ELSE
         DDTT=deltaTclock
       ENDIF
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( OBPtave, obp, 1, DDTT, bi,bj,myThid )
C     Keep record of how much time has been integrated over
          sbo_timeAve(bi,bj) = sbo_timeAve(bi,bj)+DDTT
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE(sbo_taveFreq,myTime,deltaTClock)
     &    ) THEN

C     Normalize by integrated time
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_NORMALIZE(OBPtave,sbo_timeAve,1,bi,bj,myThid)
          ENDDO
         ENDDO

         WRITE(suff,'(I10.10)') myIter
         CALL WRITE_FLD_XY_RL( 'OBPtave.', suff,
     &                          OBPtave, myIter, myThid )

C     Reset averages to zero
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET( OBPtave, 1, bi, bj, myThid )
            sbo_timeAve(bi,bj) = 0.
          ENDDO
         ENDDO

C     Time Average SBO fields
         DDTT=0.5*deltaTclock
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           CALL TIMEAVE_CUMULATE( OBPtave, obp, 1, DDTT,bi,bj,myThid )
C     Keep record of how much time has been integrated over
            sbo_timeAve(bi,bj) = sbo_timeAve(bi,bj)+DDTT
          ENDDO
         ENDDO

C--   end dump-files block
       ENDIF

C-    end if sbo_taveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_SBO */

      RETURN
      END
