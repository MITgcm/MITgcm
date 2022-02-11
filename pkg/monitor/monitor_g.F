#include "MONITOR_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE G_MONITOR(
     I                      myTime, myIter, myThid )

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
      _RL myTime
      INTEGER myIter
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
c     _RL dT
      _RL dummyRL(6)
      INTEGER k

      IF ( DIFFERENT_MULTIPLE(adjMonitorFreq,myTime,deltaTClock) ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (monitor_stdio) THEN
            mon_write_stdout = .TRUE.
          ELSE
            mon_write_stdout = .FALSE.
          ENDIF
          mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
          IF (useMNC .AND. monitor_mnc) THEN
            DO k = 1,MAX_LEN_MBUF
              mon_fname(k:k) = ' '
            ENDDO
            mon_fname(1:9) = 'g_monitor'
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
     &             '// Begin TL_MONITOR dynamic field statistics'
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

C       Print the basic statistics of model state variables
        CALL MON_SET_PREF('g__dynstat',myThid)

        CALL MON_WRITESTATS_RL(  1, g_etaN, '_g_eta',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, g_uVel, '_g_uvel',
     &           hFacW, maskInW, rAw, drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, g_vVel, '_g_vvel',
     &           hFacS, maskInS, rAs, drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, g_wVel, '_g_wvel',
     &           maskC, maskInC, rA , drC, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, g_theta,'_g_theta',
     &           hFacC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, g_salt, '_g_salt',
     &           hFacC, maskInC, rA , drF, dummyRL, myThid )
        IF ( nSx.EQ.1 .AND. nSy.EQ.1 ) THEN
C-      print stats only if nSx=nSy=1 since otherwise stats are wrong
         k = 1
         IF ( usingPCoords ) k = Nr
         CALL MON_WRITESTATS_RL( 1,g_theta(1-OLx,1-OLy,k,1,1),'_g_sst',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( 1, g_salt(1-OLx,1-OLy,k,1,1),'_g_sss',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF

C       Print the basic statistics of external forcing
        IF ( monitorSelect.GE.4 ) THEN
         CALL MON_SET_PREF('g__forcing',myThid)
         CALL MON_WRITESTATS_RS( 1, g_Qnet, '_g_qnet',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
#ifdef SHORTWAVE_HEATING
         CALL MON_WRITESTATS_RS( 1, g_Qsw , '_g_qsw',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
         CALL MON_WRITESTATS_RS( 1, g_EmPmR,'_g_empmr',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, g_fu ,  '_g_fu',
     &            maskInW, maskInW, rAw, drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, g_fv ,  '_g_fv',
     &            maskInS, maskInS, rAs, drF, dummyRL, myThid )
        ENDIF

C       Print the numerical stablility parameters for current state
c       CALL MON_SET_PREF('g__g_vcfl',myThid)
c       dT = MAX(dTtracerLev(1),deltaTMom)
c       CALL MON_ADVCFL( '_g_uvel', g_uVel,recip_dxC,dT,myThid )
c       CALL MON_ADVCFL( '_g_vvel', g_vVel,recip_dyC,dT,myThid )
c       CALL MON_ADVCFLW( '_g_wvel',g_wVel,recip_drC,dT,myThid )
c       CALL MON_ADVCFLW2('_g_W_hf',g_wVel,recip_hFacC,
c    &                                     recip_drF,dT,myThid )

C       Dynamics field monitor finish
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// End TL_MONITOR dynamic field statistics'
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
