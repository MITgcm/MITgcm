C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addummy_in_stepping.F,v 1.34 2006/06/03 02:38:10 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

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

       write(*,*) 'myIter= ',myiter

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
#ifdef ALLOW_EDTAUX_CONTROL
       call adexch_xyz_rl( mythid,adeddytaux )
#endif
#ifdef ALLOW_EDTAUY_CONTROL
       call adexch_xyz_rl( mythid,adeddytauy )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
       call adexch_xyz_rl( mythid,addiffkr )
#endif
#ifdef ALLOW_KAPGM_CONTROL
       call adexch_xyz_rl( mythid,adkapgm )
#endif
#ifdef ALLOW_SST0_CONTROL
       call adexch_xy_rl( mythid,adsst )
#endif
#ifdef ALLOW_SSS0_CONTROL
       call adexch_xy_rl( mythid,adsss )
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
        CALL WRITE_FLD_XY_RL ( 'ADJtaux.',suff, adfu, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJtauy.',suff, adfv, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJqnet.',suff, adqnet, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJempr.',suff, adempmr, myIter, myThid)
c
#ifdef ALLOW_EXF
        CALL WRITE_FLD_XY_RL ( 'ADJustress.',
     &       suff, adustress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJvstress.',
     &       suff, advstress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJhflux.',
     &       suff, adhflux, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJsflux.',
     &       suff, adsflux, myIter, myThid)
# ifdef ALLOW_ATM_TEMP
        CALL WRITE_FLD_XY_RL ( 'ADJatemp.',
     &       suff, adatemp, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJaqh.',
     &       suff, adaqh, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJprecip.',
     &       suff, adprecip, myIter, myThid)
# endif
# ifdef ALLOW_ATM_WIND
        CALL WRITE_FLD_XY_RL ( 'ADJuwind.',
     &       suff, aduwind, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJvwind.',
     &       suff, advwind, myIter, myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_FLD_XY_RL ( 'ADJswdown.',
     &       suff, adswdown, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsst.',
     &       suff, adclimsst, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsss.',
     &       suff, adclimsss, myIter, myThid)
# endif
#endif

#ifdef ALLOW_SEAICE
        CALL MDSWRITEFIELD( 
     &       'ADJarea', writeBinaryPrec, globalFiles, 'RL', 
     &       3,  adarea, 1, myIter, myThid )
        CALL MDSWRITEFIELD( 
     &       'ADJheff', writeBinaryPrec, globalFiles, 'RL', 
     &       3,  adheff, 1, myIter, myThid )
        CALL MDSWRITEFIELD( 
     &       'ADJhsnow', writeBinaryPrec, globalFiles, 'RL', 
     &       1,  adhsnow, 1, myIter, myThid )
# ifdef SEAICE_ALLOW_DYNAMICS
        CALL MDSWRITEFIELD( 
     &       'ADJuice', writeBinaryPrec, globalFiles, 'RL', 
     &       3,  aduice, 1, myIter, myThid )
        CALL MDSWRITEFIELD( 
     &       'ADJvice', writeBinaryPrec, globalFiles, 'RL', 
     &       3,  advice, 1, myIter, myThid )
# endif
#endif
c
#ifdef ALLOW_DIFFKR_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJdiffkr.',suff, addiffkr, 
     &       myIter, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJkagm.',suff, adkapgm, 
     &       myIter, myThid)
#endif
#ifdef ALLOW_EDTAUX_CONTROL
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJedtaux. ',suff, adeddytaux, myIter, myThid)
#endif
#ifdef ALLOW_EDTAUY_CONTROL
        CALL WRITE_FLD_XYZ_RL(
     &       'ADJedtauy. ',suff, adeddytauy, myIter, myThid)
#endif
#ifdef ALLOW_SST0_CONTROL
        CALL WRITE_FLD_XY_RL( 'ADJsst.',suff, adsst, myIter, myThid)
#endif
#ifdef ALLOW_SSS0_CONTROL
        CALL WRITE_FLD_XY_RL( 'ADJsss.',suff, adsss, myIter, myThid)
#endif
c

       _END_MASTER( myThid )
       _BARRIER

        print *, 'ph-mnc in addummy param ', useMNC
#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN
c
          print *, 'ph-mnc in addummy myiter ', myiter
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

c
#ifdef ALLOW_EXF
          CALL MNC_CW_SET_UDIM('adexf', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adexf', 0, myThid)
          CALL MNC_CW_I_W_S('I','adexf',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'model_time',myTime,
     &         myThid)
c
          CALL MNC_CW_RL_W('D','adexf',0,0,'adustress', 
     &         adustress, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'advstress',
     &         advstress, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adhflux',
     &         adhflux, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adsflux',
     &         adsflux, myThid)
# ifdef ALLOW_ATM_TEMP
          CALL MNC_CW_RL_W('D','adexf',0,0,'adatemp',
     &         adatemp, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adaqh',
     &         adaqh, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adprecip',
     &         adprecip, myThid)
# endif
# ifdef ALLOW_ATM_WIND
          CALL MNC_CW_RL_W('D','adexf',0,0,'aduwind',
     &         aduwind, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'advwind',
     &         advwind, myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adswdown',
     &         adswdown, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adclimsst',
     &         adclimsst, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adclimsss',
     &         adclimsss, myThid)
# endif
#endif
c
#ifdef ALLOW_SEAICE
c
          CALL MNC_CW_SET_UDIM('adseaice', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adseaice',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adseaice', 0, myThid)
          CALL MNC_CW_I_W_S('I','adseaice',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adseaice',0,0,'model_time',myTime,
     &         myThid)
c
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adarea',
     &         adarea, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adheff',
     &         adheff, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adhsnow',
     &         adhsnow, myThid)
# ifdef SEAICE_ALLOW_DYNAMICS
          CALL MNC_CW_RL_W('D','adseaice',0,0,'aduice',
     &         aduice, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'advice',
     &         advice, myThid)
# endif
c
#endif
        ENDIF
#endif /* ALLOW_MNC */

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      end









