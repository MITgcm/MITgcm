C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_dump_ad.F,v 1.1 2013/06/12 21:30:22 dgoldberg Exp $
C $Name:  $

#include "AD_CONFIG.h"
#include "PACKAGES_CONFIG.h"
#include "STREAMICE_OPTIONS.h"

CBOP
C     !ROUTINE: adstreamice_dump
C     !INTERFACE:
      subroutine adstreamice_dump( mytime, myiter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE adstreamice_dump                              |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
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

      double precision adb_glen(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      double precision adarea_shelf_streamice(1-olx:snx+olx,1-oly:sny+
     $oly,nsx,nsy)
      double precision adbdot_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
      double precision adc_basal_friction(1-olx:snx+olx,1-oly:sny+oly,
     $nsx,nsy)
      double precision adh_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
      double precision adsurf_el_streamice(1-olx:snx+olx,1-oly:sny+oly,
     $nsx,nsy)
      double precision adtau_beta_eff_streamice(1-olx:snx+olx,1-oly:sny+
     $oly,nsx,nsy)
      double precision adu_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
      double precision adv_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
      double precision advisc_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
#ifdef USE_ALT_RLOW
      double precision adr_low_si(1-olx:snx+olx,1-oly:sny+oly,nsx,
     $nsy)
      common /adstreamice_fields_rl/ adh_streamice, adu_streamice,
     $adv_streamice, advisc_streamice, adtau_beta_eff_streamice,
     $adsurf_el_streamice, adarea_shelf_streamice, adc_basal_friction,
     $adb_glen, adbdot_streamice, adr_low_si
#else
      common /adstreamice_fields_rl/ adh_streamice, adu_streamice,
     $adv_streamice, advisc_streamice, adtau_beta_eff_streamice,
     $adsurf_el_streamice, adarea_shelf_streamice, adc_basal_friction,
     $adb_glen, adbdot_streamice
c
      double precision adr_low(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adgrid_rs/ adr_low
#endif

c     == end of interface ==
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(streamice_adjDump,mytime,deltaTClock)
     & ) THEN

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        CALL WRITE_REC_3D_RL(
     &       'ADJc_basal_friction.'//suff, writeBinaryPrec,
     &       1,  adc_basal_friction, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJh_streamice.'//suff, writeBinaryPrec,
     &       1,  adh_streamice, 1, myIter, myThid )
#ifdef USE_ALT_RLOW
        CALL WRITE_REC_3D_RL(
     &       'ADJr_low.'//suff, writeBinaryPrec,
     &       1,  adr_low_si, 1, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL(
     &       'ADJr_low.'//suff, writeBinaryPrec,
     &       1,  adr_low, 1, myIter, myThid )
#endif
        CALL WRITE_REC_3D_RL(
     &       'ADJb_glen.'//suff, writeBinaryPrec,
     &       1,  adb_glen, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJbdot.'//suff, writeBinaryPrec,
     &       1,  adbdot_streamice, 1, myIter, myThid )

      ENDIF

#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
