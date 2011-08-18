C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addummy_in_dynamics.F,v 1.3 2011/08/18 09:10:26 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: addummy_in_stepping
C     !INTERFACE:
      subroutine addummy_in_dynamics( mytime, myiter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE addummy_in_stepping                           |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE addummy_in_stepping                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "adcommon.h"
#endif

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter - iteration counter for this thread
C     myTime - time counter for this thread
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myiter
      _RL     mytime

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==
CEOP

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,mytime,deltaTClock)
     & ) THEN

#ifdef AUTODIFF_TAMC_COMPATIBILITY
C
# else /* ndfef AUTODIFF_TAMC_COMPATIBILITY */
#   ifdef ALLOW_AUTODIFF_MONITOR_DIAG
       call adexch_xyz_rl( adrhoinsitu, mythid )
       call adexch_xyz_rl( adtotphihyd, mythid )
#   endif
#endif /* AUTODIFF_TAMC_COMPATIBILITY */

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJrhoinsitu.',suff, adrhoinsitu, myIter, myThid)
cph( is set to zero in addiags_phi_hyd
c        CALL WRITE_FLD_XYZ_RL(
c     &       'ADJtotphihyd.',suff, adtotphihyd, myIter, myThid)
cph)
#endif

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END









