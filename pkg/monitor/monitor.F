C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor.F,v 1.15 2004/04/03 21:17:10 edhill Exp $
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
#include "PARAMS.h"
#include "EESUPPORT.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "MONITOR.h"
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
      INTEGER time_as_int

      dT=deltaTclock

      IF (DIFFERENT_MULTIPLE(monitorFreq,myTime,myTime-dT)) THEN

C      Dynamics field monitor start 
       _BEGIN_MASTER(myThid)
#ifdef ALLOW_USE_MPI
       IF ( .NOT. useSingleCPUIO .OR. mpiMyId .EQ. 0 ) THEN
#endif /* ALLOW_USE_MPI */
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// Begin MONITOR dynamic field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
#ifdef ALLOW_USE_MPI
       ENDIF
#endif /* ALLOW_USE_MPI */
       _END_MASTER(myThid)

C      Print the time to make grepping the stdout easier
       CALL MON_SET_PREF('time',myThid)
       CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
       CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
C- cause floating point error on some platform (cf Martin Losch)
c      time_as_int = NINT( myTime )
c      CALL MON_OUT_I( '_secondsi', time_as_int,mon_string_none,myThid)
C      Print the basic statistics of model state variables
       CALL MON_SET_PREF('dynstat',myThid)
       CALL MON_PRINTSTATS_RL(1,EtaN,'_eta',
     &                        maskH,maskH,rA ,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,uVel,'_uvel',
     &                        maskW,hFacW,rAw,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,vVel,'_vvel',
     &                        maskS,hFacS,rAs,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,wVel,'_wvel',
     &                        maskC,maskC,rA ,drC,myThid)
       CALL MON_PRINTSTATS_RL(Nr,theta,'_theta',
     &                        maskC,hFacC,rA ,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,salt,'_salt',
     &                        maskC,hFacC,rA ,drF,myThid)

C      Print the numerical stablility parameters for current state
       CALL MON_SET_PREF('advcfl',myThid)
       dT=max(deltaTtracer,deltaTmom)
       CALL MON_ADVCFL('_uvel',uVel,recip_dxc,dT,myThid)
       CALL MON_ADVCFL('_vvel',vVel,recip_dyc,dT,myThid)
       CALL MON_ADVCFLW('_wvel',wVel,recip_drc,dT,myThid)
       CALL MON_ADVCFLW2('_W_hf',wVel,recip_hFacC,
     &                               recip_drC,dT,myThid)

C-     Print stats for KE
       CALL MON_KE(myIter, myThid)

C-     Print stats for (relative,absolute) Vorticity AND Pot.Vort. 
       CALL MON_VORT3(myIter, myThid)

C      Print stats for surface correction terms (Linear Free-Surf)
       CALL MON_SURFCOR(myThid)

C      Check that solution is within reasonable bounds
       CALL MON_SOLUTION(myIter,myTime,myThid)

C      Dynamics field monitor finish
       _BEGIN_MASTER(myThid)
#ifdef ALLOW_USE_MPI
       IF ( .NOT. useSingleCPUIO .OR. mpiMyId .EQ. 0 ) THEN
#endif /* ALLOW_USE_MPI */
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// End MONITOR dynamic field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
#ifdef ALLOW_USE_MPI
       ENDIF
#endif /* ALLOW_USE_MPI */
       _END_MASTER(myThid)

      ENDIF

      RETURN
      END
