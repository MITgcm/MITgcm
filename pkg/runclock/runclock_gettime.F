C $Header: /u/gcmpack/MITgcm/pkg/runclock/runclock_gettime.F,v 1.1 2005/05/31 18:24:34 adcroft Exp $
C $Name:  $

#include "RUNCLOCK_OPTIONS.h"

CBOP
C     !ROUTINE: RUNCLOCK_GETTIME
C     !INTERFACE:
      SUBROUTINE RUNCLOCK_GETTIME( yr,mo,dy,hr,mi,sc,tins )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE RUNCLOCK_GETTIME
C     | o Routine to get wall clock time
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "RUNCLOCK.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER yr,mo,dy,hr,mi,sc
      Real*8  tins

C     !LOCAL VARIABLES:
C     == Local variables ==
C     For use with DATE_AND_TIME
      CHARACTER*(8)  DAT_DATE
      CHARACTER*(10) DAT_TIME
      CHARACTER*(5)  DAT_ZONE
      INTEGER        DAT_VALS(10)
C     For use with TIMER_GET_TIME
      Real*8 TM_userTime
      Real*8 TM_systemTime
      Real*8 TM_wallClockTime
      INTEGER ms
CEOP

#ifdef ALLOW_RUNCLOCK

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER tm

#ifdef RUNCLOCK_USES_DATE_AND_TIME
      CALL date_and_time(DAT_DATE,DAT_TIME,DAT_ZONE,DAT_VALS)
      ms=DAT_VALS(8)
      sc=DAT_VALS(7)
      mi=DAT_VALS(6)
      hr=DAT_VALS(5)
      dy=DAT_VALS(3)
      mo=DAT_VALS(3)
      yr=DAT_VALS(3)
      tins=(((((yr-05)*12+mo)*30.+dy)*24.+hr)*60.+mi)*60.+sc+0.001*ms
#else
      CALL timer_get_time(TM_userTime,TM_systemTime,TM_wallClockTime)
      tm=int( TM_wallClockTime )
       sc=mod(tm,60)
      tm=(tm-sc)/60
       mi=mod(tm,60)
      tm=(tm-mi)/60
       hr=mod(tm,24)
      tm=(tm-hr)/24
       dy=mod(tm,30)
      tm=(tm-dy)/30
       mo=mod(tm,12)
       yr=(tm-mo)/12
      ms=TM_wallClockTime-int( TM_wallClockTime )
      tins=TM_wallClockTime
#endif /* RUNCLOCK_USES_DATE_AND_TIME */

c     write(0,*) 'RUNCLOCK_GETTIME: yr ... =',yr,mo,dy,hr,mi,sc,ms,tins

#endif /* ALLOW_RUNCLOCK */

      RETURN
      END
