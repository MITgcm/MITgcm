C $Header: /u/gcmpack/MITgcm/pkg/showflops/showflops_insolve.F,v 1.4 2012/07/07 00:08:09 jmc Exp $
C $Name:  $

#include "SHOWFLOPS_OPTIONS.h"

CBOP
C     !ROUTINE: SHOWFLOPS_INSOLVE
C     !INTERFACE:
      SUBROUTINE SHOWFLOPS_INSOLVE( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE showflops_insolve
C     | o Do runtime timing
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
C     note: under the multi-threaded model myiter and
C           mytime are local variables passed around as routine
C           arguments. Although this is fiddly it saves the need to
C           impose additional synchronisation points when they are
C           updated.
C     myThid - thread number for this instance of the routine.
      integer iloop
      INTEGER myThid

C     !FUNCTIONS:
C     == Functions ==
#ifdef ALLOW_RUNCLOCK
      LOGICAL RUNCLOCK_CONTINUE
      LOGICAL RC_CONT
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==

CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('SHOWFLOPS_INSOLVE',myThid)
#endif

#ifdef TIME_PER_TIMESTEP_SFP
CCE107 Time per timestep information
      _BEGIN_MASTER( myThid )
      CALL TIMER_GET_TIME( utnew, stnew, wtnew )
C Only output timing information after the 1st timestep
      IF ( wtold .NE. 0.0D0 ) THEN
        WRITE(msgBuf,'(A34,3F10.6)')
     $        'User, system and wallclock time:', utnew - utold,
     $        stnew - stold, wtnew - wtold
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      ENDIF
      utold = utnew
      stold = stnew
      wtold = wtnew
      _END_MASTER( myThid )
#endif
#ifdef USE_PAPI_FLOPS_SFP
CCE107 PAPI summary performance
      _BEGIN_MASTER( myThid )
#ifdef USE_FLIPS
      call PAPIF_flips(real_time, proc_time, flpops, mflops, check)
#else
      call PAPIF_flops(real_time, proc_time, flpops, mflops, check)
#endif
      WRITE(msgBuf,'(A34,F12.6,A,F12.6)')
     $     'Mflop/s during this timestep:', mflops, ' ', mflops
     $     *proc_time/(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#ifdef PAPI_VERSION
      call PAPIF_ipc(real_time, proc_time, instr, ipc, check)
      WRITE(msgBuf,'(A34,F12.6,A,F12.6)')
     $     'IPC during this timestep:', ipc, ' ', ipc*proc_time
     $     /(real_time + 1E-36)
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
#endif
      _END_MASTER( myThid )
#else
#ifdef USE_PCL_FLOPS_SFP
CCE107 PCL summary performance
      _BEGIN_MASTER( myThid )
      PCLstop(descr, i_result, fp_result, nevents)
      do ipcl = 1, nevents
         WRITE(msgBuf,'(A22,A26,F12.6)'),
     $        pcl_counter_name(pcl_counter_list(ipcl)),
     $        'during this timestep:', fp_results(ipcl)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      enddo
      PCLstart(descr, pcl_counter_list, nevents, flags)
      _END_MASTER( myThid )
#endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('SHOWFLOPS_INSOLVE',myThid)
#endif

      END

