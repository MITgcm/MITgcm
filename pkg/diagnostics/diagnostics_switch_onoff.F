C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_switch_onoff.F,v 1.2 2005/02/06 23:19:02 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_SWITCH_ONOFF

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SWITCH_ONOFF( myIter, myThid )

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
C     myIter     :: current Iteration number
C     myThid     :: my Thread Id number
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C      newIter :: future iteration number
C      j,m,n   :: loop index
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER newIter
      INTEGER j, m, n

      _BEGIN_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      newIter = 1 + myIter
      DO n = 1,nlists
       IF ( freq(n).LT.0 ) THEN
C--    Select diagnostics list that uses instantaneous output

c       IF ( MOD(newIter,freq(n)).EQ.0 ) THEN
        IF ( MOD(myIter,-freq(n)).EQ.INT(-freq(n)/2) ) THEN
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
