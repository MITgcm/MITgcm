C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor_g.F,v 1.2 2006/11/09 16:26:14 heimbach Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"
#include "PACKAGES_CONFIG.h"
#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE G_MONITOR(
     I                    myIter, myTime, myThid )

C     !DESCRIPTION:
C     Monitor key dynamical variables: calculate over the full domain
C      some simple statistics (e.g., min,max,average) and write them.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "MONITOR.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#if (defined (ALLOW_TANGENTLINEAR_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL dT
      _RL statsTemp(6)
c     INTEGER time_as_int
#ifdef ALLOW_MNC
      INTEGER i
#endif

      IF ( DIFFERENT_MULTIPLE(adjmonitorFreq,myTime,deltaTClock) ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it's the only thread that uses those flags):

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
            mon_fname(1:7) = 'g_monitor'
            CALL MNC_CW_APPEND_VNAME(
     &           'T', '-_-_--__-__t', 0,0, myThid)
            CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
            CALL MNC_CW_RL_W_S(
     &          'D',mon_fname,1,1,'T', myTime, myThid)
            CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
            mon_write_mnc = .TRUE.
          ENDIF
#endif /* ALLOW_MNC */

C       Dynamics field monitor start
          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// Begin MONITOR dynamic field statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

C--   endif master cpu io
        ENDIF

C       Print the time to make grepping the stdout easier
        CALL MON_SET_PREF('g__time',myThid)
        CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
C       - cause floating point error on some platform (cf Martin Losch)
C       time_as_int = NINT( myTime )
C       CALL MON_OUT_I( '_secondsi', time_as_int,mon_string_none,myThid)

C       Print the basic statistics of model state variables
        CALL MON_SET_PREF('g__dynstat',myThid)
        CALL MON_PRINTSTATS_RL(1,g_EtaN,'_g_eta',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,g_uVel,'_g_uvel',
     &       maskW,hFacW,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,g_vVel,'_g_vvel',
     &       maskS,hFacS,rAs,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,g_wVel,'_g_wvel',
     &       maskC,maskC,rA ,drC,myThid)
        CALL MON_WRITESTATS_RL(Nr,g_theta,'_g_theta',
     &       maskC,hFacC,rA ,drF,
     &       statsTemp, myThid)
        CALL MON_PRINTSTATS_RL(Nr,g_salt,'_g_salt',
     &       maskC,hFacC,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,g_theta(1-olx,1-oly,1,1,1),'_g_sst',
     &       maskC,hFacC,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,g_salt(1-olx,1-oly,1,1,1),'_g_sss',
     &       maskC,hFacC,rA ,drF,myThid)

C       Print the basic statistics of external forcing
        CALL MON_SET_PREF('g__extforcing',myThid)
        CALL MON_PRINTSTATS_RL(1,g_Qnet,'_g_qnet',
     &       maskH,maskH,rA ,drF,myThid)
#ifdef SHORTWAVE_HEATING
        CALL MON_PRINTSTATS_RL(1,g_Qsw,'_g_qsw',
     &       maskH,maskH,rA ,drF,myThid)
#endif
        CALL MON_PRINTSTATS_RL(1,g_EmPmR,'_g_empmr',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,g_fu,'_g_fu',
     &       maskW,hFacW,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,g_fv,'_g_fv',
     &       maskS,hFacS,rAs,drF,myThid)

C       Print the numerical stablility parameters for current state
        CALL MON_SET_PREF('g__g_vcfl',myThid)
        dT=MAX(dTtracerLev(1),deltaTmom)
        CALL MON_ADVCFL('_g_uvel',g_uVel,recip_dxc,dT,myThid)
        CALL MON_ADVCFL('_g_vvel',g_vVel,recip_dyc,dT,myThid)
        CALL MON_ADVCFLW('_g_wvel',g_wVel,recip_drc,dT,myThid)
        CALL MON_ADVCFLW2('_g_W_hf',g_wVel,recip_hFacC,
     &       recip_drC,dT,myThid)

C       Dynamics field monitor finish
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it's the only thread that uses those flags):

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// End MONITOR dynamic field statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif master cpu io
        ENDIF

C     endif different multiple
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
