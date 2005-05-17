C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_switch_onoff.F,v 1.8 2005/05/17 00:22:00 molod Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_SWITCH_ONOFF

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SWITCH_ONOFF( myTime, myIter, myThid )

C     !DESCRIPTION:
C-----
C     Called at the beginning of the time-step,
C     to switch on/off diagnostics for snap-shot output
C-----
C      during iterations that are multiple of |freq|,
C      switch ON diagnostics (ndiag>=0) that will become active
C      and then can be written at the end of the time-step ;
C      otherwise, put diagnostics in non-active mode (ndiag=-1)
C-----

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#ifdef ALLOW_FIZHI
#include "chronos.h"
#endif

C     !INPUT PARAMETERS:
C     myTime     :: current Time of simulation ( s )
C     myIter     :: current Iteration number
C     myThid     :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C      newIter :: future iteration number
C      j,m,n   :: loop index
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
c     INTEGER newIter
      INTEGER j, m, n
      LOGICAL time4SnapShot
      _RL     phiSec, freqSec
#ifdef ALLOW_FIZHI
      integer mmdd,hhmmss,nsecf2
      logical alarm2
      character *9 tagname
#endif

      LOGICAL  DIFF_PHASE_MULTIPLE
      EXTERNAL DIFF_PHASE_MULTIPLE

      _BEGIN_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     newIter = 1 + myIter
      DO n = 1,nlists

       IF ( freq(n).LT.0. ) THEN
C--    Select diagnostics list that uses instantaneous output

        freqSec = freq(n)
        phiSec = phase(n)
        time4SnapShot = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                       myTime, deltaTclock )
#ifdef ALLOW_FIZHI
        if( useFIZHI) then
         mmdd = int(freq(n))
         hhmmss = int((freq(n) - int(freq(n)))*1.e6)
         freqSec = nsecf2(hhmmss,mmdd,nymd)
         write(tagname,'(A,I2.2)')'diagtag',n
         time4SnapShot = alarm2(tagname)
        endif
#endif

        IF ( time4SnapShot ) THEN
C--     switch ON diagnostics of output-stream # n
          DO m=1,nActive(n)
            j = jdiag(m,n)
c           IF (ndiag(j).NE.0) WRITE(0,*) myIter,j,ndiag(j),' ->',0
            ndiag(j) = 0
          ENDDO
        ELSE
C--     switch OFF diagnostics of output-stream # n
          DO m=1,nActive(n)
            j = jdiag(m,n)
c           IF (ndiag(j).NE.-1) WRITE(0,*) myIter,j,ndiag(j),' ->',-1
            ndiag(j) = -1
          ENDDO
        ENDIF

       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C-jmc: do we need a "BARRIER" at this point ?
c     _BARRIER

      RETURN
      END
