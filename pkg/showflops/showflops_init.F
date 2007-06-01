C $Header: /u/gcmpack/MITgcm/pkg/showflops/showflops_init.F,v 1.1 2007/06/01 16:41:12 heimbach Exp $
C $Name:  $

#include "SHOWFLOPS_OPTIONS.h"

CBOP
C     !ROUTINE: SHOWFLOPS_INIT
C     !INTERFACE:
      SUBROUTINE SHOWFLOPS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE showflops_init
C     | o Initialise runtime timers.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHOWFLOPS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - thread number for this instance of the routine.
      INTEGER myThid 

C     !FUNCTIONS:
C     == Functions ==
#ifdef ALLOW_RUNCLOCK
      LOGICAL RUNCLOCK_CONTINUE
      LOGICAL RC_CONT
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer iloop

CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('SHOWFLOPS_INIT',myThid)
#endif

#ifdef TIME_PER_TIMESTEP
CCE107 Initial call for timers
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utold, stold, wtold )
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS
CCE107 Initial call for PAPI
      _BEGIN_MASTER( myThid )
#ifdef USE_FLIPS
      call PAPIF_flips(real_time, proc_time, flpops, mflops, check)
#else
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
#endif
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'Mflop/s before timestepping:', mflops, ' ', mflops*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#ifdef PAPI_VERSION
      call PAPIF_ipc(real_time, proc_time, instr, ipc, check)
      WRITE(msgBuf,'(A34,F10.6,A,F10.6)')
     $     'IPC before timestepping:', ipc, ' ', ipc*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif
      _END_MASTER( myThid )
#else
#ifdef USE_PCL_FLOPS
CCE107 Initial call for PCL
      _BEGIN_MASTER( myThid )
      res = PCLstop(descr, i_result, fp_result, nevents)
      do ipcl = 1, nevents
         WRITE(msgBuf,'(A22,A26,F10.6)'),
     $        pcl_counter_name(pcl_counter_list(ipcl)),
     $        'before timestepping:', fp_result(ipcl)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      enddo
      res = PCLstart(descr, pcl_counter_list, nevents, flags)
      _END_MASTER( myThid )
#endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('SHOWFLOPS_INIT',myThid)
#endif

      END

#ifdef TIME_PER_TIMESTEP_SFP
CCE107 Initialization of common block for per timestep timing
      BLOCK DATA settimers
C     !TIMING VARIABLES
C     == Timing variables ==
      REAL*8 utnew, utold, stnew, stold, wtnew, wtold
      COMMON /timevars/ utnew, utold, stnew, stold, wtnew, wtold
      DATA utnew, utold, stnew, stold, wtnew, wtold /6*0.0D0/
      END
#endif
#ifdef USE_PAPI_FLOPS_SFP
CCE107 Initialization of common block for PAPI summary performance
      BLOCK DATA setpapis
      INTEGER*8 flpops, instr
      REAL real_time, proc_time, mflops, ipc
      COMMON /papivars/ flpops, instr, real_time, proc_time, mflops, ipc
      DATA flpops, instr, real_time, proc_time, mflops, ipc /2*0,4*0.E0/
      END
#endif
