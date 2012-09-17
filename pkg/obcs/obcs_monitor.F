C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_monitor.F,v 1.5 2012/09/17 22:06:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C--  File obcs_monitor.F : write statistics of dynamical fields at OB section
C--   Contents
C--   o OBCS_MONITOR
C--   o OBCS_MON_WRITESTATS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_MONITOR

C     !INTERFACE:
      SUBROUTINE OBCS_MONITOR( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_MONITOR
C     | o Write out OB statistics
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     !LOCAL VARIABLES:
C     msgBuf       :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL fldStats(0:4,4)

      IF ( DIFFERENT_MULTIPLE( OBCS_monitorFreq,myTime,deltaTClock )
     &   ) THEN

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_MONITOR',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   OBCS monitor start
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

c         IF (monitor_stdio) THEN
            mon_write_stdout = .TRUE.
c         ELSE
c           mon_write_stdout = .FALSE.
c         ENDIF
          mon_write_mnc    = .FALSE.

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// Begin OBCS MONITOR field statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

C--   endif master cpu io
        ENDIF

        IF ( OBCS_monitorFreq .NE. monitorFreq
     &       .OR. .NOT.monitor_stdio ) THEN
         CALL MON_SET_PREF('obc_time_',myThid)
         CALL MON_OUT_I( 'tsnumber', myIter,mon_string_none,myThid)
         CALL MON_OUT_RL('secondsf', myTime,mon_string_none,myThid)
        ENDIF

        CALL MON_SET_PREF('obc_',myThid)

C--     Calculate and print Flow field Stats:
        IF ( OBCS_monSelect.GE.0 ) THEN

         CALL OBCS_MON_STATS_EW_RL(
     I             tileHasOBE, tileHasOBW,
     I             OB_Ie, OB_Iw, OB_indexNone,
     I             Nr, Nr, 1, uVel, hFacW, dyG, drF, maskInC,
     O             fldStats(0,1),
     I             myThid )
         CALL OBCS_MON_STATS_NS_RL(
     I             tileHasOBN, tileHasOBS,
     I             OB_Jn, OB_Js, OB_indexNone,
     I             Nr, Nr, 2, vVel, hFacS, dxG, drF, maskInC,
     O             fldStats(0,3),
     I             myThid )

         CALL OBCS_MON_WRITESTATS(
     I                             fldStats, 'Vel',
     I                             3, myIter, myThid )

        ENDIF

C--     Calculate and print Temp. field Stats:
        IF ( OBCS_monSelect.GE.1 ) THEN

         CALL OBCS_MON_STATS_EW_RL(
     I             tileHasOBE, tileHasOBW,
     I             OB_Ie, OB_Iw, OB_indexNone,
     I             Nr, Nr, 0, theta, hFacW, dyG, drF, maskInC,
     O             fldStats(0,1),
     I             myThid )
         CALL OBCS_MON_STATS_NS_RL(
     I             tileHasOBN, tileHasOBS,
     I             OB_Jn, OB_Js, OB_indexNone,
     I             Nr, Nr, 0, theta, hFacS, dxG, drF, maskInC,
     O             fldStats(0,3),
     I             myThid )

         CALL OBCS_MON_WRITESTATS(
     I                             fldStats, 'theta',
     I                             0, myIter, myThid )

        ENDIF

C--     Calculate and print Salt field Stats:
        IF ( OBCS_monSelect.GE.2 ) THEN

         CALL OBCS_MON_STATS_EW_RL(
     I             tileHasOBE, tileHasOBW,
     I             OB_Ie, OB_Iw, OB_indexNone,
     I             Nr, Nr, 0, salt, hFacW, dyG, drF, maskInC,
     O             fldStats(0,1),
     I             myThid )
         CALL OBCS_MON_STATS_NS_RL(
     I             tileHasOBN, tileHasOBS,
     I             OB_Jn, OB_Js, OB_indexNone,
     I             Nr, Nr, 0, salt, hFacS, dxG, drF, maskInC,
     O             fldStats(0,3),
     I             myThid )

         CALL OBCS_MON_WRITESTATS(
     I                             fldStats, 'salt',
     I                             0, myIter, myThid )

        ENDIF

C--   OBCS monitor finish
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// End OBCS MONITOR field statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif master cpu io
        ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_MONITOR',myThid)
#endif

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_OBCS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_MON_WRITESTATS

C     !INTERFACE:
      SUBROUTINE OBCS_MON_WRITESTATS(
     I                               arrStats, arrName,
     I                               prtSelect, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_MON_WRITESTATS
C     | o Write out OB statistics
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      _RL arrStats(0:4,4)
      CHARACTER*(*) arrName
      INTEGER prtSelect
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_MONITOR

C     !LOCAL VARIABLES:
C     msgBuf       :: Informational/error message buffer
      INTEGER j
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(12) fldName
      _RL theMin, theMax, theMean, theSD

        j = 1
        fldName = 'E_'//arrName
        IF ( MOD(prtSelect,2).EQ.1 ) fldName = 'E_u'//arrName
        IF ( arrStats(0,j).GT.0. _d 0 ) THEN
          theMean= arrStats(1,j)/arrStats(0,j)
          theSD  = arrStats(2,j)/arrStats(0,j)
          theSD  = theSD - theMean*theMean
          IF ( theSD.GT.0. _d 0 ) theSD = SQRT(theSD)
          theMin = arrStats(3,j)
          theMax = arrStats(4,j)
          CALL MON_OUT_RL( fldName, theMax,  mon_foot_max,  myThid )
          CALL MON_OUT_RL( fldName, theMin,  mon_foot_min,  myThid )
          CALL MON_OUT_RL( fldName, theMean, mon_foot_mean, myThid )
          CALL MON_OUT_RL( fldName, theSD,   mon_foot_sd,   myThid )
          IF ( prtSelect.GE.2 )
     &    CALL MON_OUT_RL( fldName, arrStats(1,j), '_Int',  myThid )
        ENDIF

        j = 2
        fldName = 'W_'//arrName
        IF ( MOD(prtSelect,2).EQ.1 ) fldName = 'W_u'//arrName
        IF ( arrStats(0,j).GT.0. _d 0 ) THEN
          theMean= arrStats(1,j)/arrStats(0,j)
          theSD  = arrStats(2,j)/arrStats(0,j)
          theSD  = theSD - theMean*theMean
          IF ( theSD.GT.0. _d 0 ) theSD = SQRT(theSD)
          theMin = arrStats(3,j)
          theMax = arrStats(4,j)
          CALL MON_OUT_RL( fldName, theMax,  mon_foot_max,  myThid )
          CALL MON_OUT_RL( fldName, theMin,  mon_foot_min,  myThid )
          CALL MON_OUT_RL( fldName, theMean, mon_foot_mean, myThid )
          CALL MON_OUT_RL( fldName, theSD,   mon_foot_sd,   myThid )
          IF ( prtSelect.GE.2 )
     &    CALL MON_OUT_RL( fldName, arrStats(1,j), '_Int',  myThid )
        ENDIF

        j = 3
        fldName = 'N_'//arrName
        IF ( MOD(prtSelect,2).EQ.1 ) fldName = 'N_v'//arrName
        IF ( arrStats(0,j).GT.0. _d 0 ) THEN
          theMean= arrStats(1,j)/arrStats(0,j)
          theSD  = arrStats(2,j)/arrStats(0,j)
          theSD  = theSD - theMean*theMean
          IF ( theSD.GT.0. _d 0 ) theSD = SQRT(theSD)
          theMin = arrStats(3,j)
          theMax = arrStats(4,j)
          CALL MON_OUT_RL( fldName, theMax,  mon_foot_max,  myThid )
          CALL MON_OUT_RL( fldName, theMin,  mon_foot_min,  myThid )
          CALL MON_OUT_RL( fldName, theMean, mon_foot_mean, myThid )
          CALL MON_OUT_RL( fldName, theSD,   mon_foot_sd,   myThid )
          IF ( prtSelect.GE.2 )
     &    CALL MON_OUT_RL( fldName, arrStats(1,j), '_Int',  myThid )
        ENDIF

        j = 4
        fldName = 'S_'//arrName
        IF ( MOD(prtSelect,2).EQ.1 ) fldName = 'S_v'//arrName
        IF ( arrStats(0,j).GT.0. _d 0 ) THEN
          theMean= arrStats(1,j)/arrStats(0,j)
          theSD  = arrStats(2,j)/arrStats(0,j)
          theSD  = theSD - theMean*theMean
          IF ( theSD.GT.0. _d 0 ) theSD = SQRT(theSD)
          theMin = arrStats(3,j)
          theMax = arrStats(4,j)
          CALL MON_OUT_RL( fldName, theMax,  mon_foot_max,  myThid )
          CALL MON_OUT_RL( fldName, theMin,  mon_foot_min,  myThid )
          CALL MON_OUT_RL( fldName, theMean, mon_foot_mean, myThid )
          CALL MON_OUT_RL( fldName, theSD,   mon_foot_sd,   myThid )
          IF ( prtSelect.GE.2 )
     &    CALL MON_OUT_RL( fldName, arrStats(1,j), '_Int',  myThid )
        ENDIF

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_OBCS */

      RETURN
      END
