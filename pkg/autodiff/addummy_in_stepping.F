C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addummy_in_stepping.F,v 1.44 2008/05/30 02:47:50 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "AUTODIFF_OPTIONS.h"

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

       call adexch_xy_rl( mythid,adetan)
       call adexch_xyz_rl( mythid,adtheta)
       call adexch_xyz_rl( mythid,adsalt)
       call adexch_xyz_rl( mythid,adwvel )
       call adexch_uv_xyz_rl(  .true. ,mythid,aduvel,advvel )
c
       call adexch_xy_rl( mythid,adfu )
       call adexch_xy_rl( mythid,adfv )
       call adexch_xy_rl( mythid,adqnet )
       call adexch_xy_rl( mythid,adempmr )
#ifdef ALLOW_EDDYPSI_CONTROL
       call adexch_xyz_rl( mythid,adeddypsix )
       call adexch_xyz_rl( mythid,adeddypsiy )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
       call adexch_xyz_rl( mythid,addiffkr )
#endif
#ifdef ALLOW_KAPGM_CONTROL
       call adexch_xyz_rl( mythid,adkapgm )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
       call adexch_xyz_rl( mythid,adkapredi )
#endif
#ifdef ALLOW_SST0_CONTROL
       call adexch_xy_rl( mythid,adsst )
#endif
#ifdef ALLOW_SSS0_CONTROL
       call adexch_xy_rl( mythid,adsss )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
       call adexch_xy_rl( mythid,adbottomdragfld)
#endif

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

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
        CALL WRITE_FLD_XY_RL(
     &       'ADJetan.',suff, adetan, myIter, myThid)

        if ( .NOT. useSEAICE .AND. .NOT. useEXF ) then
        CALL WRITE_FLD_XY_RL ( 'ADJtaux.',suff, adfu, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJtauy.',suff, adfv, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJqnet.',suff, adqnet, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJempr.',suff, adempmr, myIter, myThid)
#ifdef SHORTWAVE_HEATING
        CALL WRITE_FLD_XY_RL ( 'ADJqsw.',suff, adqsw, myIter, myThid)
#endif
        endif

#ifdef ALLOW_DIFFKR_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJdiffkr.',suff, addiffkr,
     &       myIter, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJkapgm.',suff, adkapgm,
     &       myIter, myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJkapredi.',suff, adkapredi,
     &       myIter, myThid)
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJeddypsix. ',suff, adeddypsix, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJeddypsiy. ',suff, adeddypsiy, myIter, myThid)
#endif
#ifdef ALLOW_SST0_CONTROL
        CALL WRITE_FLD_XY_RL( 'ADJsst.',suff, adsst, myIter, myThid)
#endif
#ifdef ALLOW_SSS0_CONTROL
        CALL WRITE_FLD_XY_RL( 'ADJsss.',suff, adsss, myIter, myThid)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
        CALL WRITE_FLD_XY_RL ( 'ADJbottomdrag.',suff,
     & adbottomdragfld, myIter, myThid)
#endif

#ifdef ALLOW_EXF
cph        IF ( useEXF ) CALL EXF_AD_DUMP( mytime, myiter, myThid )
#endif

#ifdef ALLOW_SEAICE
        IF ( useSEAICE ) CALL SEAICE_AD_DUMP( mytime, myiter, myThid )
#endif

       _END_MASTER( myThid )
       _BARRIER

#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN
c
          CALL MNC_CW_SET_UDIM('adstate', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adstate',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
          CALL MNC_CW_I_W_S('I','adstate',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adstate',0,0,'model_time',myTime,
     &         myThid)
c
          CALL MNC_CW_RL_W('D','adstate',0,0,'adU', aduVel, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adV', advVel, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adT', adtheta, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adS', adsalt, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEta', adetaN, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adW', adwVel, myThid)

          CALL MNC_CW_RL_W('D','adstate',0,0,'adQnet', adQnet, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEmpmr', adEmpmr, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adFu', adfu, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adFv', adfv, myThid)

#ifdef ALLOW_SST0_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,'adSST', adsst, myThid)
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,'adSSS', adsss, myThid)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &         'adDiffkr', addiffkr, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &         'adkapgm', adkapgm, myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &         'adkapredi', adkapredi, myThid)
#endif

        ENDIF
#endif /* ALLOW_MNC */

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      end









