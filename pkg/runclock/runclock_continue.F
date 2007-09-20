C $Header: /u/gcmpack/MITgcm/pkg/runclock/runclock_continue.F,v 1.2 2007/09/20 20:55:21 ce107 Exp $
C $Name:  $

#include "RUNCLOCK_OPTIONS.h"

CBOP
C     !ROUTINE: RUNCLOCK_CONTINUE
C     !INTERFACE:
      FUNCTION RUNCLOCK_CONTINUE( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | FUNCTION RUNCLOCK_CONTINUE
C     | o Function to signal time to termiante run
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "RUNCLOCK.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      LOGICAL RUNCLOCK_CONTINUE
      INTEGER myThid
CEOP

#ifdef ALLOW_RUNCLOCK

C     !LOCAL VARIABLES:
C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER RC_now_yr,RC_now_mo,RC_now_dy
      INTEGER RC_now_hr,RC_now_mi,RC_now_sc
      INTEGER dd,dh,dm,ds
      INTEGER elapsed_secs,maxtime_secs
      Real*8 tins,ETmax

      _BEGIN_MASTER(myThid)
      IF (.NOT.RUNCLOCKIsOn) THEN
       WRITE(msgBuf,'(A)') ' RUNCLOCK_CONTINUE: RUNCLOCK not turned on'
       CALL PRINT_ERROR( msgBuf, myThid ) 
      ENDIF
      _END_MASTER(myThid)

C     Get the current wall-clock time
      CALL runclock_gettime(
     &   RC_now_yr,RC_now_mo,RC_now_dy,
     &   RC_now_hr,RC_now_mi,RC_now_sc,
     &   tins)

      CALL GLOBAL_MAX_R8(tins,myThid)

c     write(0,*) 'RUNCLOCK_CONTINUE: yr mo dy hr mi sc tins',
c    &   RC_now_yr,RC_now_mo,RC_now_dy,
c    &   RC_now_hr,RC_now_mi,RC_now_sc,
c    &   tins

      ds=RC_now_sc-RC_start_sc
      dm=RC_now_mi-RC_start_mi
      dh=RC_now_hr-RC_start_hr
      dd=RC_now_dy-RC_start_dy
c     write(0,'(A,4I3)') 'RUNCLOCK_CONTINUE: dy hr mi sc',dd,dh,dm,ds

      ETmax=((dd*24.+dh)*60.+dm)*60.+ds
      CALL GLOBAL_MAX_R8(ETmax,myThid)
      elapsed_secs=ETmax
c     elapsed_secs=((dd*24.+dh)*60.+dm)*60.+ds
      maxtime_secs=(RC_MaxTime_hr*60+RC_MaxTime_mi)*60+RC_MaxTime_sc

      _BEGIN_MASTER(myThid)
c     write(0,*) ' RUNCLOCK_CONTINUE: elapsed=',elapsed_secs,maxtime_secs

c     IF (myProcID.EQ.0)
c    & write(0,'(A,4I3,2I6,F)') 'RUNCLOCK_CONTINUE: dy hr mi sc el mx',
c    &  dd,dh,dm,ds,elapsed_secs,maxtime_secs,tins-RC_start_tins

      WRITE(msgBuf,'(A,G16.6,G24.10)') ' RUNCLOCK: dT,T-To =',
     &  tins-RC_prev_tins,tins-RC_start_tins
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      RC_prev_tins=tins

      IF (maxtime_secs.ne.0 .AND. elapsed_secs.GT.maxtime_secs) THEN
       RUNCLOCK_CONTINUE=.FALSE.
       WRITE(msgBuf,'(A)') 
     & ' RUNCLOCK_CONTINUE: Wall Clock limit reached!'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') ' RUNCLOCK_CONTINUE: Ending run now ...'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       CALL PRINT_ERROR( msgBuf, myThid )
       RETURN
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_RUNCLOCK */

      RUNCLOCK_CONTINUE=.TRUE.

      RETURN
      END
