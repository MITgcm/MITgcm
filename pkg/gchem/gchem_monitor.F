C $Header: /u/gcmpack/MITgcm/pkg/gchem/Attic/gchem_monitor.F,v 1.4 2004/07/13 18:04:42 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_MONITOR

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_MONITOR(
     I                myIter, myTime, myThid )

C !DESCRIPTION:
C writes out ptracer statistics

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "MONITOR.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

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
#ifdef ALLOW_GCHEM

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  ip                   :: ptracer number
      CHARACTER*(MAX_LEN_MBUF) msgBuf   
      CHARACTER*(MAX_LEN_MBUF) suff
      _RL dT
      _RL tmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER time_as_int
      integer i,j,k,bi,bj
      integer ip
CEOP

      dT=deltaTclock

      IF (myIter.le.nIter0+1.or.DIFFERENT_MULTIPLE(
     &                 monitorFreq,myTime,myTime-dT)) THEN

C      Dynamics field monitor start 
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// Begin MONITOR ptracer field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
       _END_MASTER(myThid)

       CALL MON_SET_PREF('trcstat_',myThid)
       do ip=1,ptracers_num
         WRITE(suff,'(A7,I2.2)') 'ptracer',ip
         DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k=1,Nr
          DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           tmp(i,j,k,bi,bj)=ptracer(i,j,k,bi,bj,ip)
           ENDDO
           ENDDO
          ENDDO
         ENDDO
         ENDDO
         CALL MON_PRINTSTATS_RL(Nr,tmp,suff,
     &                 maskC,hFacC,rA ,drF,myThid)
       enddo
C      Dynamics field monitor finish
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// End MONITOR ptracer field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
       _END_MASTER(myThid)

      ENDIF

#endif
#endif

      RETURN
      END
