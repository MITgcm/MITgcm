C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor.F,v 1.37 2006/06/20 19:33:21 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE MONITOR(
     I     myIter, myTime, myThid )

C     !DESCRIPTION:
C     Calculate bare statistics of global array ``arr''.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#include "MONITOR.h"
#include "DYNVARS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL dT
      _RL statsTemp(6)
c     INTEGER time_as_int
#ifdef ALLOW_MNC
      INTEGER i
#endif

      IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTclock) ) THEN

        IF (monitor_stdio) THEN
          mon_write_stdout = .TRUE.
        ELSE
          mon_write_stdout = .FALSE.
        ENDIF
        mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
        IF (useMNC .AND. monitor_mnc) THEN
          DO i = 1,MAX_LEN_MBUF
            mon_fname(i:i) = ' '
          ENDDO
          mon_fname(1:7) = 'monitor'
          CALL MNC_CW_APPEND_VNAME(
     &         'T', '-_-_--__-__t', 0,0, myThid)
          CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
          CALL MNC_CW_RL_W_S(
     &        'D',mon_fname,1,1,'T', myTime, myThid)
          CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
          mon_write_mnc = .TRUE.
        ENDIF
#endif /*  ALLOW_MNC  */

C       Dynamics field monitor start
#ifdef ALLOW_USE_MPI
        IF ( ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) .AND.
     &       mon_write_stdout ) THEN
#else /* ALLOW_USE_MPI */
        IF ( mon_write_stdout ) THEN
#endif /* ALLOW_USE_MPI */

          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A)') '// ==========================',
     &           '============================='
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          WRITE(msgBuf,'(A)')
     &           '// Begin MONITOR dynamic field statistics'
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &           SQUEEZE_RIGHT , 1)
          WRITE(msgBuf,'(2A)') '// ==========================',
     &           '============================='
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          _END_MASTER(myThid)

        ENDIF

C       Print the time to make grepping the stdout easier
        CALL MON_SET_PREF('time',myThid)
        CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
C       - cause floating point error on some platform (cf Martin Losch)
C       time_as_int = NINT( myTime )
C       CALL MON_OUT_I( '_secondsi', time_as_int,mon_string_none,myThid)

C       Print the basic statistics of model state variables
        CALL MON_SET_PREF('dynstat',myThid)
        CALL MON_PRINTSTATS_RL(1,EtaN,'_eta',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,uVel,'_uvel',
     &       maskW,hFacW,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,vVel,'_vvel',
     &       maskS,hFacS,rAs,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,wVel,'_wvel',
     &       maskC,maskC,rA ,drC,myThid)
        CALL MON_WRITESTATS_RL(Nr,theta,'_theta',
     &       maskC,hFacC,rA ,drF,
     &       statsTemp, myThid)
        CALL MON_PRINTSTATS_RL(Nr,salt,'_salt',
     &       maskC,hFacC,rA ,drF,myThid)

C       Print the numerical stablility parameters for current state
        CALL MON_SET_PREF('advcfl',myThid)
        dT=MAX(dTtracerLev(1),deltaTmom)
        CALL MON_ADVCFL('_uvel',uVel,recip_dxc,dT,myThid)
        CALL MON_ADVCFL('_vvel',vVel,recip_dyc,dT,myThid)
        CALL MON_ADVCFLW('_wvel',wVel,recip_drc,dT,myThid)
        CALL MON_ADVCFLW2('_W_hf',wVel,recip_hFacC,
     &       recip_drC,dT,myThid)

C       Print stats for KE
        CALL MON_KE(myIter, myThid)

C       Print stats for (relative,absolute) Vorticity AND Pot.Vort.
        CALL MON_VORT3(myIter, myThid)

C       Print stats for surface correction terms (Linear Free-Surf)
        CALL MON_SURFCOR(myThid)

C       Check that solution is within reasonable bounds
        CALL MON_SOLUTION( statsTemp, myTime, myIter, myThid )

C       Dynamics field monitor finish
#ifdef ALLOW_USE_MPI
        IF ( ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) .AND.
     &       mon_write_stdout ) THEN
#else /* ALLOW_USE_MPI */
        IF ( mon_write_stdout ) THEN
#endif /* ALLOW_USE_MPI */
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A)') '// ==========================',
     &           '============================='
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          WRITE(msgBuf,'(A)')
     &           '// End MONITOR dynamic field statistics'
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &           SQUEEZE_RIGHT , 1)
          WRITE(msgBuf,'(2A)') '// ==========================',
     &           '============================='
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          _END_MASTER(myThid)
        ENDIF

        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.

      ENDIF

      RETURN
      END
