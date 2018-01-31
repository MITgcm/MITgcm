#include "AUTODIFF_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: addummy_in_stepping
C     !INTERFACE:
      subroutine addummy_in_stepping( mytime, myiter, myThid )

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
#include "adcommon.h"

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

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      IF (
     &  DIFFERENT_MULTIPLE(ADJdumpFreq,mytime,deltaTClock)
     & ) THEN

         write(*,*) 'myIter= ',myiter

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

Cml        CALL WRITE_FLD_XY_RL ( 'ADJtaux.',suff, adfu, myIter, myThid)
Cml        CALL WRITE_FLD_XY_RL ( 'ADJtauy.',suff, adfv, myIter, myThid)
Cml        CALL WRITE_FLD_XY_RL ( 'ADJqnet.',suff, adqnet, myIter, myThid)
Cml        CALL WRITE_FLD_XY_RL ( 'ADJempr.',suff, adempmr, myIter, myThid)
c
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJgtnm1.',suff, adgtnm1, myIter, myThid)
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJgsnm1.',suff, adgsnm1, myIter, myThid)
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJgunm1.',suff, adgunm1, myIter, myThid)
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJgvnm1.',suff, adgvnm1, myIter, myThid)
Cml#ifdef ALLOW_PASSIVE_TRACER
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJgtr1nm1. ',suff, adgtr1nm1, myIter, myThid)
Cml#endif
c
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJtheta.',suff, adtheta, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJsalt.',suff, adsalt, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJuvel.',suff, aduvel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJvvel.',suff, advvel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJwvel.',suff, adwvel, myIter, myThid)

Cml#ifdef ALLOW_PASSIVE_TRACER
Cml        CALL WRITE_FLD_XYZ_RL(
Cml     &       'ADJtr1. ',suff, adtr1, myIter, myThid)
Cml#endif
Cml#ifdef ALLOW_DIFFKR_CONTROL
Cml        CALL WRITE_FLD_XYZ_RL ( 'ADJdiffkr.',suff, addiffkr,
Cml     &       myIter, myThid)
Cml#endif
Cml#ifdef ALLOW_KAPGM_CONTROL
Cml        CALL WRITE_FLD_XYZ_RL ( 'ADJkagm.',suff, adkapgm,
Cml     &       myIter, myThid)
Cml#endif
#ifdef ALLOW_DEPTH_CONTROL
CML(  Does not make sense at this point
CML        CALL WRITE_FLD_XY_RL ( 'ADJxx_r_low.',suff,
CML     &       adxx_r_low, myIter, myThid)
CML)
        CALL WRITE_FLD_XYZ_RL ( 'ADJhFacC.',suff,
     &       adhfacc, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJhFacS.',suff,
     &       adhfacs, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJhFacW.',suff,
     &       adhfacw, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJrhFacC.',suff,
     &       adrecip_hfacc, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJrhFacS.',suff,
     &       adrecip_hfacs, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJrhFacW.',suff,
     &       adrecip_hfacw, myIter, myThid)
#endif /* ALLOW_DEPTH_CONTROL */

cph        CALL WRITE_FLD_XY_RL( 'ADJ_sst.',suff, adsst, myIter, myThid)
cph        CALL WRITE_FLD_XY_RL( 'ADJ_sss.',suff, adsss, myIter, myThid)

C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R ADDUMMY_IN_STEPPING'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out model state'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')
     &    '// Model state written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
