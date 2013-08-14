C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_switch_onoff.F,v 1.19 2013/08/14 00:57:33 jmc Exp $
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
      CHARACTER*(MAX_LEN_MBUF) msgBuf
c     INTEGER newIter
      INTEGER m, n, nd
      INTEGER bi, bj, ip, iSp
      LOGICAL time4SnapShot
      _RL     phiSec, freqSec
      INTEGER nInterval
      _RL     xInterval
      LOGICAL dBugFlag
      INTEGER dBugUnit
#ifdef ALLOW_FIZHI
      LOGICAL  ALARM2NEXT
      EXTERNAL ALARM2NEXT
      CHARACTER *9 tagname
#endif

      LOGICAL  DIFF_PHASE_MULTIPLE
      EXTERNAL DIFF_PHASE_MULTIPLE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      dBugFlag = debugLevel.GE.debLevE .AND. myThid.EQ.1
      dBugUnit = errorMessageUnit

C--   Track diagnostics pkg activation status:
      IF ( myIter.EQ.nIter0 ) THEN
        _BEGIN_MASTER(myThid)
c       IF ( diag_pkgStatus.NE.10 ) STOP
        diag_pkgStatus = ready2fillDiags
        _END_MASTER(myThid)
c     ELSEIF
c       IF ( diag_pkgStatus.NE.ready2fillDiags ) STOP
      ENDIF

c     newIter = 1 + myIter
      DO n = 1,nlists

       IF ( freq(n).LT.0. ) THEN
C--    Select diagnostics list that uses instantaneous output

        freqSec = freq(n)
        phiSec = phase(n)
        time4SnapShot = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                       myTime, deltaTClock )
#ifdef ALLOW_FIZHI
        IF ( useFIZHI ) THEN
         WRITE(tagname,'(A,I2.2)')'diagtag',n
         time4SnapShot = ALARM2NEXT(tagname,deltaT)
        ENDIF
#endif
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                        time4SnapShot,
     I                        myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_CAL */

        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
          IF ( time4SnapShot ) THEN
C--       switch ON diagnostics of output-stream # n
            DO m=1,nActive(n)
c             nd = jdiag(m,n)
c             IF (ndiag(nd).NE.0) WRITE(0,*) myIter,nd,ndiag(nd),' ->',0
              ip = idiag(m,n)
              IF (ip.GT.0) ndiag(ip,bi,bj) = 0
            ENDDO
          ELSE
C--       switch OFF diagnostics of output-stream # n
            DO m=1,nActive(n)
c             nd = jdiag(m,n)
c             IF (ndiag(nd).NE.-1) WRITE(0,*) myIter,nd,ndiag(nd),' ->',-1
              ip = idiag(m,n)
              IF (ip.GT.0) ndiag(ip,bi,bj) = -1
            ENDDO
          ENDIF
         ENDDO
        ENDDO

C--    list with instantaneous output: end
       ENDIF

       IF ( averageCycle(n).GT.1 ) THEN
C--    Select diagnostics list that uses periodic averaging
        xInterval = myTime + deltaTClock*0.5 _d 0 - averagePhase(n)
        xInterval = xInterval / averageFreq(n)
        IF ( xInterval.GE.0. ) THEN
          nInterval = INT(xInterval)
        ELSE
          nInterval = 1 + INT( -xInterval/FLOAT(averageCycle(n)) )
          nInterval = nInterval*averageCycle(n) + INT(xInterval) - 1
        ENDIF
        nInterval = MOD(nInterval,averageCycle(n))

C-     check future value of pdiag:
        IF (dBugFlag.AND.pdiag(n,1,1).NE.nInterval)
     &    WRITE(dBugUnit,'(A,I8,3(A,I4),F17.6)')
     &     'DIAG_SWITCH_ONOFF: at it=', myIter, ', list:', n,
     &     ' switch', pdiag(n,1,1),' ->', nInterval, xInterval
        IF ( nInterval.LT.0 .OR. nInterval.GE.averageCycle(n) ) THEN
          WRITE(msgBuf,'(2A,I3,A,I4)') 'DIAGNOSTICS_SWITCH_ONOFF:',
     &       ' error setting pdiag(n=',n,') to:', nInterval
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,I4,A,F17.6)') 'DIAGNOSTICS_SWITCH_ONOFF:',
     &       ' cycle=', averageCycle(n), ', xInt=', xInterval
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R DIAGNOSTICS_SWITCH_ONOFF'
        ENDIF

        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
           pdiag(n,bi,bj) = nInterval
         ENDDO
        ENDDO
C--    list with periodic averaging: end
       ENDIF

      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO n = 1,diagSt_nbLists

       IF ( diagSt_freq(n).LT.0. ) THEN
C--    Select diagnostics list that uses instantaneous output

        dBugFlag = debugLevel.GE.debLevE

        freqSec = diagSt_freq(n)
        phiSec = diagSt_phase(n)
        time4SnapShot = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                       myTime, deltaTClock )

#ifdef ALLOW_FIZHI
        IF ( useFIZHI ) THEN
         WRITE(tagname,'(A,I2.2)')'diagStg',n
         time4SnapShot = ALARM2NEXT(tagname,deltaT)
        ENDIF
#endif
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                        time4SnapShot,
     I                        myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_CAL */

        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
          dBugFlag = dBugFlag.AND.(bi.EQ.1.AND.bj.EQ.1.AND.myThid.EQ.1)
          IF ( time4SnapShot ) THEN
C--     switch ON diagnostics of output-stream # n
            DO m=1,diagSt_nbActv(n)
             iSp = iSdiag(m,n)
             IF (iSp.GT.0) THEN
               nd = jSdiag(m,n)
               IF (dBugFlag.AND.qSdiag(0,0,iSp,bi,bj).NE. 0.)
     &          WRITE(dBugUnit,'(A,I8,A,I6,3A,1PE10.3,A,I3)')
     &           'DIAG_SWITCH_ONOFF: at it=', myIter, ' diag:', nd,
     &           ' ',cdiag(nd),' :',qSdiag(0,0,iSp,bi,bj),' ->', 0
               qSdiag(0,0,iSp,bi,bj) = 0.
             ENDIF
            ENDDO
          ELSE
C--     switch OFF diagnostics of output-stream # n
            DO m=1,diagSt_nbActv(n)
             iSp = iSdiag(m,n)
             IF (iSp.GT.0) THEN
               nd = jSdiag(m,n)
               IF (dBugFlag.AND.qSdiag(0,0,iSp,bi,bj).NE.-1.)
     &          WRITE(dBugUnit,'(A,I8,A,I6,3A,1PE10.3,A,I3)')
     &           'DIAG_SWITCH_ONOFF: at it=', myIter, ' diag:', nd,
     &           ' ',cdiag(nd),' :',qSdiag(0,0,iSp,bi,bj),' ->', -1
               qSdiag(0,0,iSp,bi,bj) = -1.
             ENDIF
            ENDDO
          ENDIF
         ENDDO
        ENDDO

       ENDIF
      ENDDO

      RETURN
      END
