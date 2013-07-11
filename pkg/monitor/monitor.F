C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor.F,v 1.50 2013/07/11 23:24:59 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MONITOR

C     !INTERFACE:
      SUBROUTINE MONITOR(
     I                    myTime, myIter, myThid )

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

C     !INPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL dT
      _RL statsTemp(6)
      _RL dummyRL(6)
      _RS thickFacC(Nr), thickFacF(Nr)
      INTEGER k

      IF ( DIFFERENT_MULTIPLE(monitorFreq,myTime,deltaTClock) ) THEN

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
            mon_fname(1:7) = 'monitor'
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

C-      Set mass weighted thickness factor
        DO k=1,Nr
          thickFacC(k) = drF(k)*deepFac2C(k)*rhoFacC(k)
          thickFacF(k) = drC(k)*deepFac2F(k)*rhoFacF(k)
        ENDDO

C       Print the time to make grepping the stdout easier
        CALL MON_SET_PREF( 'time', myThid )
        CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid )
        CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid )

        IF ( monitorSelect.GE.1 ) THEN
C       Print the basic statistics of model state variables
         CALL MON_SET_PREF( 'dynstat', myThid )
         CALL MON_WRITESTATS_RL(  1, etaN, '_eta',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( Nr, uVel, '_uvel',
     &            hFacW, maskInW, rAw, thickFacC, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( Nr, vVel, '_vvel',
     &            hFacS, maskInS, rAs, thickFacC, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( Nr, wVel, '_wvel',
     &            maskC, maskInC, rA , thickFacF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( Nr, theta,'_theta',
     &            hFacC, maskInC, rA , thickFacC, statsTemp, myThid )
         CALL MON_WRITESTATS_RL( Nr, salt, '_salt',
     &            hFacC, maskInC, rA , thickFacC, dummyRL, myThid )
        ELSE
         statsTemp(1) = 1.
         statsTemp(2) = 0.
        ENDIF
        IF ( monitorSelect.GE.3 .AND.
     &       nSx.EQ.1 .AND. nSy.EQ.1 ) THEN
C-      print stats only if nSx=nSy=1 since otherwise stats are wrong
         k = 1
         IF ( usingPCoords ) k = Nr
         CALL MON_WRITESTATS_RL( 1, theta(1-OLx,1-OLy,k,1,1), '_sst',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( 1,  salt(1-OLx,1-OLy,k,1,1), '_sss',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF

C       Print the basic statistics of external forcing
        IF ( monitorSelect.GE.3 ) THEN
         CALL MON_SET_PREF( 'forcing', myThid )
         CALL MON_WRITESTATS_RS( 1, Qnet, '_qnet',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, Qsw , '_qsw',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, EmPmR,'_empmr',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, fu ,  '_fu',
     &            maskInW, maskInW, rAw, drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RS( 1, fv ,  '_fv',
     &            maskInS, maskInS, rAs, drF, dummyRL, myThid )
        ENDIF

C       Print the numerical stablility parameters for current state
        CALL MON_SET_PREF( 'advcfl', myThid )
        dT = MAX(dTtracerLev(1),deltaTmom)
        CALL MON_ADVCFL(  '_uvel', uVel, recip_dxC, dT, myThid )
        CALL MON_ADVCFL(  '_vvel', vVel, recip_dyC, dT, myThid )
        CALL MON_ADVCFLW( '_wvel', wVel, recip_drC, dT, myThid )
        CALL MON_ADVCFLW2('_W_hf', wVel, recip_hFacC,
     &                                   recip_drF, dT, myThid )

C       Print stats for KE
        CALL MON_KE(myIter, myThid)

C       Print stats for (relative,absolute) Vorticity AND Pot.Vort.
        IF ( monitorSelect.GE.2 ) CALL MON_VORT3( myIter, myThid )

C       Print stats for surface correction terms (Linear Free-Surf)
        IF ( monitorSelect.GE.2 ) CALL MON_SURFCOR( myThid )

C       Check that solution is within reasonable bounds
        CALL MON_SOLUTION( statsTemp, myTime, myIter, myThid )

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

      RETURN
      END
