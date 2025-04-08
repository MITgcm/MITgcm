#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: adstreamice_dump
C     !INTERFACE:
      subroutine adstreamice_dump( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE adstreamice_dump                              |
C     *==========================================================*
C     Extract adjoint variable from TAF-generated
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
C     myTime - time counter for this thread
C     myIter - iteration counter for this thread
C     myThid - Thread number for this instance of the routine.
      _RL     myTime
      integer myIter
      integer myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))

C     !LOCAL VARIABLES:
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      double precision :: area_shelf_streamice_ad(1-OLx:sNx+OLx,1-OLy:
     $sNy+OLy,nSx,nSy)
      double precision :: b_glen_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      double precision :: bdot_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     $nSx,nSy)
      double precision :: c_basal_friction_ad(1-OLx:sNx+OLx,1-OLy:sNy+
     $OLy,nSx,nSy)
      double precision :: float_frac_streamice_ad(1-OLx:sNx+OLx,1-OLy:
     $sNy+OLy,nSx,nSy)
      double precision :: h_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     $nSx,nSy)
      double precision :: surf_el_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+
     $OLy,nSx,nSy)
      double precision :: tau_beta_eff_streamice_ad(1-OLx:sNx+OLx,1-OLy:
     $sNy+OLy,nSx,nSy)
      double precision :: u_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     $nSx,nSy)
      double precision :: v_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     $nSx,nSy)
      double precision :: visc_streamice_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     $nSx,nSy)
      common /streamice_fields_rl_ad/ h_streamice_ad, u_streamice_ad,
     $v_streamice_ad, visc_streamice_ad, tau_beta_eff_streamice_ad,
     $float_frac_streamice_ad, surf_el_streamice_ad,
     $area_shelf_streamice_ad, c_basal_friction_ad, b_glen_ad,
     $bdot_streamice_ad

#ifdef USE_ALT_RLOW
      double precision :: r_low_si_ad(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,
     $nSy)
      common /streamice_rlow_ad/ r_low_si_ad
#endif
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(streamice_adjDump,myTime,deltaTClock)
     & ) THEN

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        CALL WRITE_REC_3D_RL(
     &       'ADJc_basal_friction.'//suff, writeBinaryPrec,
     &       1,  c_basal_friction_ad, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJh_streamice.'//suff, writeBinaryPrec,
     &       1,  h_streamice_ad, 1, myIter, myThid )
#ifdef USE_ALT_RLOW
        CALL WRITE_REC_3D_RL(
     &       'ADJr_low.'//suff, writeBinaryPrec,
     &       1,  r_low_si_ad, 1, myIter, myThid )
#endif
        CALL WRITE_REC_3D_RL(
     &       'ADJb_glen.'//suff, writeBinaryPrec,
     &       1,  b_glen_ad, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJbdot.'//suff, writeBinaryPrec,
     &       1,  bdot_streamice_ad, 1, myIter, myThid )

      ENDIF

#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
