C $Header:

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_DIAGS( myCurrentTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SBO_DIAGS                                     |
C     | o Do SBO diagnostic output.                              |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SBO.h"

C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SBO

c     == Local variables ==
c     bi, bj    - loop counters
c     sbo_diag  - vector of SBO diagnostics contains:
c                 model time (s), xoamc, yoamc, zoamc,
c                 xoamp, yoamp, zoamp, mass, xcom, ycom, zcom
      INTEGER bi, bj
      Real*8   sbo_diag(11)
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      _RL DDTT
      CHARACTER*(MAX_LEN_MBUF) suff
      character*(8) fName
      integer narr
      integer irecord, k

C-----------------------------------------------------------------
C     Save angular momentum and mass variables at every time step
C-----------------------------------------------------------------

      sbo_diag(1)  = myCurrentTime
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

      fName        = 'SBO_DIAG'
      narr         = 11
      irecord      = myCurrentTime/deltaTClock

      CALL SBO_WRITEVECTOR(
     I   fName,
     I   narr,
     I   sbo_diag,
     I   irecord,
     I   myIter,
     I   myThid )

#ifdef ALLOW_TIMEAVE

C-----------------------------------------------------------------
C     Save time-averaged bottom pressure at sbo_taveFreq intervals
C-----------------------------------------------------------------

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_RESET(OBPtave,1,bi,bj,myThid)
               DO k=1,Nr
                  sbo_TimeAve(k,bi,bj)=0.
               ENDDO
            ENDDO
         ENDDO
      ENDIF

C     Time Average SBO fields
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE
     &     (sbo_taveFreq,myCurrentTime,myCurrentTime-deltaTClock) )
     &     THEN
         DDTT=0.5*deltaTclock
      ELSE
         DDTT=deltaTclock
      ENDIF
      DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_CUMULATE(
     &           obp,OBPtave,1,DDTT,bi,bj,myThid)

C     Keep record of how much time has been integrated over
            DO k=1,Nr
               sbo_TimeAve(k,bi,bj)=sbo_TimeAve(k,bi,bj)+DDTT
            ENDDO
         ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(sbo_taveFreq,myCurrentTime,
     &     myCurrentTime-deltaTClock)
     &     ) THEN

C     Normalize by integrated time
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_NORMALIZ(OBPtave,sbo_timeave,1,bi,bj,myThid)
            ENDDO
         ENDDO

         WRITE(suff,'(I10.10)') myIter

         CALL WRITE_FLD_XY_RL('OBPtave',suff,OBPtave,
     &        myIter,myThid)

C     Reset averages to zero
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_RESET(OBPtave,1,bi,bj,myThid)
               DO k=1,Nr
                  sbo_TimeAve(k,bi,bj)=0.
               ENDDO
            ENDDO
         ENDDO

C     Time Average SBO fields
         DDTT=0.5*deltaTclock
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_CUMULATE(
     &              obp,OBPtave,1,DDTT,bi,bj,myThid)
               
C     Keep record of how much time has been integrated over
               DO k=1,Nr
                  sbo_TimeAve(k,bi,bj)=sbo_TimeAve(k,bi,bj)+DDTT
               ENDDO
            ENDDO
         ENDDO
      ENDIF

#endif ALLOW_TIMEAVE

#endif ALLOW_SBO

      RETURN
      END
