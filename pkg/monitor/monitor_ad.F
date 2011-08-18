C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor_ad.F,v 1.10 2011/08/18 09:10:26 heimbach Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"
c#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE ADMONITOR(
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
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
#  include "adcommon.h"
# endif
#endif

C     !INPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
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
      _RL dummyRL(6)
      INTEGER k

      IF ( DIFFERENT_MULTIPLE(adjmonitorFreq,myTime,deltaTClock) ) THEN

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
            mon_fname(1:9) = 'admonitor'
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

C       Print the basic statistics of model state variables
        CALL MON_SET_PREF('ad_dynstat',myThid)
        CALL MON_WRITESTATS_RL(  1, adEtaN, '_adeta',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, aduVel, '_aduvel',
     &           hFacW, maskInW, rAw, drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, advVel, '_advvel',
     &           hFacS, maskInS, rAs, drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, adwVel, '_adwvel',
     &           maskC, maskInC, rA , drC, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, adtheta,'_adtheta',
     &           hFacC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( Nr, adsalt, '_adsalt',
     &           hFacC, maskInC, rA , drF, dummyRL, myThid )
        IF ( nSx.EQ.1 .AND. nSy.EQ.1 ) THEN
C-      print stats only if nSx=nSy=1 since otherwise stats are wrong
         k = 1
         IF ( usingPCoords ) k = Nr
         CALL MON_WRITESTATS_RL( 1,adtheta(1-OLx,1-OLy,k,1,1),'_adsst',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( 1, adsalt(1-OLx,1-OLy,k,1,1),'_adsss',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF

C       Print the basic statistics of external forcing
c       IF ( monitorSelect.GE.3 ) THEN
         CALL MON_SET_PREF('ad_forcing',myThid)
         CALL MON_WRITESTATS_RS( 1, adQnet, '_adqnet',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
#ifdef SHORTWAVE_HEATING
         CALL MON_WRITESTATS_RS( 1, adQsw , '_adqsw',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
         CALL MON_WRITESTATS_RS( 1, adEmPmR,'_adempmr',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, adfu ,  '_adfu',
     &            maskInW, maskInW, rAw, drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, adfv ,  '_adfv',
     &            maskInS, maskInS, rAs, drF, dummyRL, myThid )
c       ENDIF

C       Print the numerical stablility parameters for current state
        CALL MON_SET_PREF('ad_advcfl',myThid)
        dT = MAX(dTtracerLev(1),deltaTmom)
        CALL MON_ADVCFL( '_aduvel', aduVel,recip_dxC,dT,myThid )
        CALL MON_ADVCFL( '_advvel', advVel,recip_dyC,dT,myThid )
        CALL MON_ADVCFLW('_adwvel', adwVel,recip_drC,dT,myThid )
        CALL MON_ADVCFLW2('_adW_hf',adwVel,recip_hFacC,
     &                                     recip_drF,dT,myThid )

C       Dynamics field monitor finish
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

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

#ifdef ALLOW_EXF
cph This case is handled separately in exf_adjoint_snapshots_ad.F
#endif

#ifdef ALLOW_SEAICE
      CALL ADSEAICE_MONITOR ( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PTRACERS
      CALL ADPTRACERS_MONITOR ( myTime, myIter, myThid )
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
