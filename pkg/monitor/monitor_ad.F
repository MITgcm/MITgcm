C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor_ad.F,v 1.4 2008/03/28 18:48:04 heimbach Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"
#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE ADMONITOR(
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
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
#  include "adcommon.h"
# endif
#endif

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#if (defined (ALLOW_AUTODIFF) && defined (ALLOW_AUTODIFF_MONITOR))

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
            mon_fname(1:7) = 'admonitor'
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
        CALL MON_SET_PREF('ad_time',myThid)
        CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
C       - cause floating point error on some platform (cf Martin Losch)
C       time_as_int = NINT( myTime )
C       CALL MON_OUT_I( '_secondsi', time_as_int,mon_string_none,myThid)

C       Print the basic statistics of model state variables
        CALL MON_SET_PREF('ad_dynstat',myThid)
        CALL MON_PRINTSTATS_RL(1,adEtaN,'_adeta',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,aduVel,'_aduvel',
     &       maskW,hFacW,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,advVel,'_advvel',
     &       maskS,hFacS,rAs,drF,myThid)
        CALL MON_PRINTSTATS_RL(Nr,adwVel,'_adwvel',
     &       maskC,maskC,rA ,drC,myThid)
        CALL MON_WRITESTATS_RL(Nr,adtheta,'_adtheta',
     &       maskC,hFacC,rA ,drF,
     &       statsTemp, myThid)
        CALL MON_PRINTSTATS_RL(Nr,adsalt,'_adsalt',
     &       maskC,hFacC,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,adtheta(1-olx,1-oly,1,1,1),'_adsst',
     &       maskC,hFacC,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,adsalt(1-olx,1-oly,1,1,1),'_adsss',
     &       maskC,hFacC,rA ,drF,myThid)

C       Print the basic statistics of external forcing
        CALL MON_SET_PREF('ad_extforcing',myThid)
        CALL MON_PRINTSTATS_RL(1,adQnet,'_adqnet',
     &       maskH,maskH,rA ,drF,myThid)
#ifdef SHORTWAVE_HEATING
        CALL MON_PRINTSTATS_RL(1,adQsw,'_adqsw',
     &       maskH,maskH,rA ,drF,myThid)
#endif
        CALL MON_PRINTSTATS_RL(1,adEmPmR,'_adempmr',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,adfu,'_adfu',
     &       maskW,hFacW,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,adfv,'_adfv',
     &       maskS,hFacS,rAs,drF,myThid)

C       Print the numerical stablility parameters for current state
        CALL MON_SET_PREF('ad_advcfl',myThid)
        dT=MAX(dTtracerLev(1),deltaTmom)
        CALL MON_ADVCFL('_aduvel',aduVel,recip_dxc,dT,myThid)
        CALL MON_ADVCFL('_advvel',advVel,recip_dyc,dT,myThid)
        CALL MON_ADVCFLW('_adwvel',adwVel,recip_drc,dT,myThid)
        CALL MON_ADVCFLW2('_adW_hf',adwVel,recip_hFacC,
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
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
