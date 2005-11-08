C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_monitor.F,v 1.10 2005/11/08 15:53:41 cnh Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_MONITOR

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_MONITOR(
     I                myIter, myTime, myThid )

C !DESCRIPTION:
C writes out ptracer statistics

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "EESUPPORT.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_MONITOR

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  ip                   :: ptracer number
      CHARACTER*(MAX_LEN_MBUF) msgBuf   
      CHARACTER*(MAX_LEN_MBUF) suff
      _RL dT
      integer ip
CEOP

      dT=deltaTclock

      IF ( DIFFERENT_MULTIPLE( PTRACERS_monitorFreq,myTime,dT ) ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
       mon_write_stdout = .FALSE.
       mon_write_mnc    = .FALSE.
       _BARRIER

C     Ptracers field monitor start 
       _BEGIN_MASTER(myThid)
#ifdef ALLOW_USE_MPI
       IF ( .NOT. useSingleCPUIO .OR. mpiMyId .EQ. 0 ) THEN
#endif /* ALLOW_USE_MPI */
        
        IF (PTRACERS_monitor_stdio) THEN
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A)')
     &        '// Begin MONITOR ptracer field statistics'
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &        SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         mon_write_stdout = .TRUE.
        ENDIF
        
#ifdef ALLOW_USE_MPI
       ENDIF
#endif /* ALLOW_USE_MPI */
        _END_MASTER(myThid)

#ifdef ALLOW_MNC
        IF (useMNC .AND. PTRACERS_monitor_mnc) mon_write_mnc = .TRUE.
#endif /*  ALLOW_MNC  */

        IF ( PTRACERS_monitorFreq .NE. monitorFreq ) THEN
C     repeat printing of time to make grepping easier, default is not
C     to do this, because the default is to use the same monitorFreq
C     for ptracers as for the dynamics variables.
         CALL MON_SET_PREF('trctime',myThid)
         CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
         CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
        ENDIF
C       Print the basic statistics of ptracer variables
       CALL MON_SET_PREF('trcstat_',myThid)
       DO ip = 1, PTRACERS_numInUse
         _BEGIN_MASTER(myThid)
         WRITE(suff,'(A7,I2.2)') 'ptracer',ip
         _END_MASTER(myThid)
         _BARRIER
         CALL MON_PRINTSTATS_RL(
     &        Nr, ptracer(1-Olx,1-Oly,1,1,1,ip),
     &        suff, maskC,hFacC,rA ,drF,myThid)
       ENDDO

       IF (PTRACERS_monitor_stdio) THEN
C     Ptracers field monitor finish
        _BEGIN_MASTER(myThid)
#ifdef ALLOW_USE_MPI
        IF ( .NOT. useSingleCPUIO .OR. mpiMyId .EQ. 0 ) THEN
#endif /* ALLOW_USE_MPI */
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A)') 
     &        '// End MONITOR ptracers field statistics'
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &        SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
#ifdef ALLOW_USE_MPI
        ENDIF
#endif /* ALLOW_USE_MPI */

        mon_write_stdout = .FALSE.
        
        _END_MASTER(myThid)
       ENDIF

       mon_write_stdout = .FALSE.
       mon_write_mnc    = .FALSE.

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
