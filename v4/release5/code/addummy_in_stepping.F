C $Header: /u/gcmpack/MITgcm_contrib/ecco_utils/ecco_v4_release3_devel/code/addummy_in_stepping.F,v 1.1 2017/05/04 17:46:37 ou.wang Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: ADDUMMY_IN_STEPPING
C     !INTERFACE:
      SUBROUTINE ADDUMMY_IN_STEPPING( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_IN_STEPPING                           |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_IN_STEPPING                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C#ifdef ALLOW_OPENAD
C      use OAD_active
C      use OAD_rev
C      use OAD_tape
C      use OAD_cp
C#endif
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_OPENAD
# include "DYNVARS.h"
# include "FFIELDS.h"
#endif
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
c#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF.h"
# ifndef ALLOW_OPENAD
#  include "adcommon.h"
# endif /* ALLOW_OPENAD */
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff    :: Hold suffix part of a filename
C     msgBuf  :: Error message buffer
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL var2Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var2Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL foo2D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var3Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL var3Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL foo3D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS dumRS(1)
      _RL dumRL(1)
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     & ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

c increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecMn=dumpAdRecMn+1
c#ifdef ALLOW_DEBUG
c      IF ( debugMode ) print*,'dumpAdRecMn',dumpAdRecMn
c#endif

        IF ( dumpAdVarExch.EQ.1 ) THEN
#ifdef ALLOW_OPENAD
C--   need to all the correct OpenAD EXCH S/R ; left empty for now
#else /* ALLOW_OPENAD */

#ifdef AUTODIFF_TAMC_COMPATIBILITY

         call adexch_xy_rl( myThid,adetan)
         call adexch_xyz_rl( myThid,adtheta)
         call adexch_xyz_rl( myThid,adsalt)
         call adexch_xyz_rl( myThid,adwvel )
         call adexch_uv_xyz_rl(  .true. ,myThid,aduvel,advvel )

         call adexch_uv_xy_rs( .true., myThid, adfu, adfv )
         call adexch_xy_rs( myThid,adqnet )
         call adexch_xy_rs( myThid,adempmr )
# ifdef ALLOW_EDDYPSI_CONTROL
         call adexch_uv_xyz_rs(  .true. ,myThid,adeddypsix,adeddypsiy )
# endif
# ifdef ALLOW_DIFFKR_CONTROL
         call adexch_xyz_rl( myThid,addiffkr )
# endif
# ifdef ALLOW_KAPGM_CONTROL
         call adexch_xyz_rl( myThid,adkapgm )
# endif
# ifdef ALLOW_KAPREDI_CONTROL
         call adexch_xyz_rl( myThid,adkapredi )
# endif
# ifdef ALLOW_SST0_CONTROL
         call adexch_xy_rl( myThid,adsst )
# endif
# ifdef ALLOW_SSS0_CONTROL
         call adexch_xy_rl( myThid,adsss )
# endif
# ifdef ALLOW_BOTTOMDRAG_CONTROL
         call adexch_xy_rl( myThid,adbottomdragfld)
# endif

# else /* ndfef AUTODIFF_TAMC_COMPATIBILITY */

         CALL ADEXCH_3D_RL( adEtaN, 1 , myThid )
#  ifndef ALLOW_BULK_OFFLINE
         CALL ADEXCH_3D_RL( adTheta,Nr, myThid )
         CALL ADEXCH_3D_RL( adSalt, Nr, myThid )
         CALL ADEXCH_3D_RL( adwVel, Nr, myThid )
         CALL ADEXCH_UV_3D_RL( aduVel,advVel, .TRUE., Nr, myThid )
#  endif
         CALL ADEXCH_UV_XY_RS( adFu, adFv, .TRUE., myThid )
         CALL ADEXCH_XY_RS( adQnet, myThid )
         CALL ADEXCH_XY_RS( adEmPmR, myThid )
# ifdef ALLOW_EDDYPSI_CONTROL
         CALL ADEXCH_UV_XYZ_RS( adEddyPsiX, adEddyPsiY, .TRUE., myThid )
# endif
# ifdef ALLOW_DIFFKR_CONTROL
         CALL ADEXCH_3D_RL( adDiffKr, Nr, myThid )
# endif
# ifdef ALLOW_KAPGM_CONTROL
         CALL ADEXCH_3D_RL( adKapGM, Nr, myThid )
# endif
# ifdef ALLOW_KAPREDI_CONTROL
         CALL ADEXCH_3D_RL( adKapRedi, Nr, myThid )
# endif
# ifdef ALLOW_SST0_CONTROL
         CALL ADEXCH_XY_RS( adSST, myThid )
# endif
# ifdef ALLOW_SSS0_CONTROL
         CALL ADEXCH_XY_RS( adSSS, myThid )
# endif
# ifdef ALLOW_BOTTOMDRAG_CONTROL
         CALL ADEXCH_3D_RL( adBottomDragFld, 1 , myThid )
# endif

#endif /* AUTODIFF_TAMC_COMPATIBILITY */
#endif /* ALLOW_OPENAD */
        ENDIF

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF
C ==>> Resetting run-time parameter writeBinaryPrec in the middle of a run
C ==>>  is very very very nasty !!!
c       writeBinaryPrec = writeStatePrec
C <<==  If you really want to mess-up with this at your own risk,
C <<==  uncomment the line above

C-----------------------------------------------------------------------
#ifndef ALLOW_OPENAD
C-----------------------------------------------------------------------

        IF ( ( dumpAdVarExch.NE.2 ).AND.(.NOT.dumpAdByRec) ) THEN
          CALL WRITE_FLD_XYZ_RL( 'ADJtheta.', suff,
     &                           adTheta, myIter, myThid )
          CALL WRITE_FLD_XYZ_RL( 'ADJsalt.', suff,
     &                           adSalt, myIter, myThid )
          CALL WRITE_FLD_XYZ_RL( 'ADJuvel.', suff,
     &                           aduVel, myIter, myThid )
          CALL WRITE_FLD_XYZ_RL( 'ADJvvel.', suff,
     &                           advVel, myIter, myThid )
          CALL WRITE_FLD_XYZ_RL( 'ADJwvel.', suff,
     &                           adwVel, myIter, myThid )
          CALL WRITE_FLD_XY_RL ( 'ADJetan.', suff,
     &                           adEtaN, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           CALL WRITE_FLD_XY_RS('ADJtaux.',suff, adFu, myIter, myThid )
           CALL WRITE_FLD_XY_RS('ADJtauy.',suff, adFv, myIter, myThid )
           CALL WRITE_FLD_XY_RS('ADJqnet.',suff, adQnet,myIter,myThid )
           CALL WRITE_FLD_XY_RS('ADJempr.',suff,adEmPmR,myIter,myThid )
#ifdef SHORTWAVE_HEATING
           CALL WRITE_FLD_XY_RS('ADJqsw.', suff, adQsw,myIter, myThid )
#endif
          ENDIF

#ifdef ALLOW_GGL90
c         CALL WRITE_FLD_XYZ_RL ( 'ADJggl90diffkr.',suff,
c    &                             adGGL90DiffKr, myIter, myThid )
          CALL WRITE_FLD_XYZ_RL( 'ADJggl90tke.', suff,
     &                           adGGL90TKE, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL WRITE_FLD_XYZ_RL( 'ADJdiffkr.', suff,
     &                           adDiffKr, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL WRITE_FLD_XYZ_RL( 'ADJkapgm.', suff,
     &                           adKapGM, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL WRITE_FLD_XYZ_RL( 'ADJkapredi.', suff,
     &                           adKapRedi, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          CALL WRITE_FLD_XYZ_RS( 'ADJeddypsix.', suff,
     &                           adEddyPsiX, myIter, myThid )
          CALL WRITE_FLD_XYZ_RS( 'ADJeddypsiy.', suff,
     &                           adEddyPsiY, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          CALL WRITE_FLD_XY_RS( 'ADJsst.',suff, adSST, myIter, myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL WRITE_FLD_XY_RS( 'ADJsss.',suff, adSSS, myIter, myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL WRITE_FLD_XY_RL ( 'ADJbottomdrag.', suff,
     &                           adBottomDragFld, myIter, myThid )
#endif

        ELSEIF ( ( dumpAdVarExch.NE.2 ).AND.(dumpAdByRec) ) THEN

          CALL WRITE_REC_XYZ_RL( 'ADJtheta',
     &                           adTheta, dumpAdRecMn, myIter,myThid )
          CALL WRITE_REC_XYZ_RL( 'ADJsalt',
     &                           adSalt, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XYZ_RL( 'ADJuvel',
     &                           aduVel, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XYZ_RL( 'ADJvvel',
     &                           advVel, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XYZ_RL( 'ADJwvel',
     &                           adwVel, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XY_RL ( 'ADJetan',
     &                           adEtaN, dumpAdRecMn, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           CALL WRITE_REC_XY_RS('ADJtaux',
     &          adFu, dumpAdRecMn, myIter, myThid )
           CALL WRITE_REC_XY_RS('ADJtauy',
     &          adFv, dumpAdRecMn, myIter, myThid )
           CALL WRITE_REC_XY_RS('ADJqnet',
     &          adQnet,dumpAdRecMn, myIter,myThid )
           CALL WRITE_REC_XY_RS('ADJempr',
     &          adEmPmR,dumpAdRecMn, myIter,myThid )
#ifdef SHORTWAVE_HEATING
           CALL WRITE_REC_XY_RS('ADJqsw',
     &          adQsw,dumpAdRecMn, myIter, myThid )
#endif
          ENDIF

#ifdef ALLOW_GGL90
c         CALL WRITE_REC_XYZ_RL ( 'ADJggl90diffkr',
c    &         adGGL90DiffKr, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XYZ_RL( 'ADJggl90tke',
     &         adGGL90TKE, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL WRITE_REC_XYZ_RL( 'ADJdiffkr',
     &         adDiffKr, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL WRITE_REC_XYZ_RL( 'ADJkapgm',
     &         adKapGM, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL WRITE_REC_XYZ_RL( 'ADJkapredi',
     &         adKapRedi, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          CALL WRITE_REC_XYZ_RS( 'ADJeddypsix',
     &         adEddyPsiX, dumpAdRecMn, myIter, myThid )
          CALL WRITE_REC_XYZ_RS( 'ADJeddypsiy',
     &         adEddyPsiY, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          CALL WRITE_REC_XY_RS( 'ADJsst',
     &         adSST, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL WRITE_REC_XY_RS( 'ADJsss',
     &         adSSS, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL WRITE_REC_XY_RL ( 'ADJbottomdrag',
     &         adBottomDragFld, dumpAdRecMn, myIter, myThid )
#endif

        ELSE
C       case dumpAdVarExch = 2
          CALL COPY_ADVAR_OUTP( dumRS, adTheta,var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJtheta.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJtheta',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, adSalt, var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJsalt.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJsalt',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_AD_UV_OUTP( dumRS, dumRS, aduVel, advVel,
     &                                 var3Du, var3Dv, Nr, 34, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJuvel.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJuvel',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJvvel.',
     &                           suff, var3Dv, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJvvel',
     &                           var3Dv, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, adwVel, var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJwvel.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJwvel',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, adEtaN, var2Du, 1 , 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL(  'ADJetan.',
     &                           suff, var2Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL(  'ADJetan',
     &                           var2Du, dumpAdRecMn, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           CALL COPY_AD_UV_OUTP( adFu, adFv, dumRL, dumRL,
     &                                  var2Du, var2Dv, 1, 33, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJtaux.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJtaux',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJtauy.',
     &                           suff,var2Dv,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJtauy',
     &                           var2Dv,dumpAdRecMn, myIter,myThid )
           CALL COPY_ADVAR_OUTP( adQnet, dumRL, var2Du, 1, 11, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJqnet.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJqnet',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
           CALL COPY_ADVAR_OUTP( adEmPmR,dumRL, var2Du, 1, 11, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJempr.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJempr',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#ifdef SHORTWAVE_HEATING
           CALL COPY_ADVAR_OUTP( adQsw,  dumRL, var2Du, 1, 11, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJqsw.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJqsw',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
          ENDIF
#ifdef ALLOW_GGL90
          CALL COPY_ADVAR_OUTP( dumRS,adGGL90TKE,var3Du,Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJggl90tke.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJggl90tke',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adDiffKr,var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJdiffkr.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJdiffkr',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adKapGM, var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJkapgm.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJkapgm',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS,adKapRedi,var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJkapredi.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJkapredi',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          CALL COPY_AD_UV_OUTP( adEddyPsiX, adEddyPsiY, dumRL, dumRL,
     &                                 var3Du, var3Dv, Nr, 33, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJeddypsix.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJeddypsix',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJeddypsiy.',
     &                           suff, var3Dv, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJeddypsiy',
     &                           var3Dv, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          CALL COPY_ADVAR_OUTP( adSST, dumRL, var2Du, 1, 11, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJsst.',
     &                           suff,var2Du,myIter,myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJsst',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL COPY_ADVAR_OUTP( adSSS, dumRL, var2Du, 1, 11, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJsss.',
     &                           suff,var2Du,myIter,myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJsss',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adBottomDragFld,
     &                                        var2Du, 1, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJbottomdrag.',
     &                           suff, var2Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJbottomdrag',
     &                           var2Du, dumpAdRecMn, myIter, myThid )
#endif
C       end if dumpAdVarExch = 2
        ENDIF

C-----------------------------------------------------------------------
#else /* ndef ALLOW_OPENAD */
C-----------------------------------------------------------------------

        IF ( ( dumpAdVarExch.NE.2 ).AND.(.NOT.dumpAdByRec) ) THEN
          foo3D = theta%d
          CALL WRITE_FLD_XYZ_RL( 'ADJtheta.', suff,
     &                           foo3D, myIter, myThid )
          foo3D = salt%d
          CALL WRITE_FLD_XYZ_RL( 'ADJsalt.', suff,
     &                           foo3D, myIter, myThid )
          foo3D = uvel%d
          CALL WRITE_FLD_XYZ_RL( 'ADJuvel.', suff,
     &                           foo3D, myIter, myThid )
          foo3D = vvel%d
          CALL WRITE_FLD_XYZ_RL( 'ADJvvel.', suff,
     &                           foo3D, myIter, myThid )
          foo3D = wvel%d
          CALL WRITE_FLD_XYZ_RL( 'ADJwvel.', suff,
     &                           foo3D, myIter, myThid )
          foo2D = etaN%d
          CALL WRITE_FLD_XY_RL ( 'ADJetan.', suff,
     &                           foo2D, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           foo2D = Fu%d
           CALL WRITE_FLD_XY_RS('ADJtaux.',suff, foo2d, myIter, myThid )
           foo2D = Fv%d
           CALL WRITE_FLD_XY_RS('ADJtauy.',suff, foo2d, myIter, myThid )
           foo2D = Qnet%d
           CALL WRITE_FLD_XY_RS('ADJqnet.',suff, foo2d,myIter,myThid )
           foo2D = EmPmR%d
           CALL WRITE_FLD_XY_RS('ADJempr.',suff,foo2d,myIter,myThid )
#ifdef SHORTWAVE_HEATING
cc           foo2D = Qsw%d
cc           CALL WRITE_FLD_XY_RS('ADJqsw.', suff, foo2d,myIter, myThid )
#endif
          ENDIF

#ifdef ALLOW_GGL90
c         CALL WRITE_FLD_XYZ_RL ( 'ADJggl90diffkr.',suff,
c    &                             adGGL90DiffKr, myIter, myThid )
cc          foo3D = GGL90TKE%d
cc          CALL WRITE_FLD_XYZ_RL( 'ADJggl90tke.', suff,
cc     &                           foo3d, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          foo3D = DiffKr%d
          CALL WRITE_FLD_XYZ_RL( 'ADJdiffkr.', suff,
     &                           foo3d, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          foo3D = KapGM%d
          CALL WRITE_FLD_XYZ_RL( 'ADJkapgm.', suff,
     &                           foo3d, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          foo3D = KapRedi%d
          CALL WRITE_FLD_XYZ_RL( 'ADJkapredi.', suff,
     &                           foo3d, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          foo3D = EddyPsiX%d
          CALL WRITE_FLD_XYZ_RS( 'ADJeddypsix.', suff,
     &                           foo3d, myIter, myThid )
          foo3D = EddyPsiY%d
          CALL WRITE_FLD_XYZ_RS( 'ADJeddypsiy.', suff,
     &                           foo3d, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          foo2D = sst%d
          CALL WRITE_FLD_XY_RS( 'ADJsst.',suff, adSST, myIter, myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          foo2D = sss%d
          CALL WRITE_FLD_XY_RS( 'ADJsss.',suff, adSSS, myIter, myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          foo2D = BottomDragFld%d
          CALL WRITE_FLD_XY_RL ( 'ADJbottomdrag.', suff,
     &                           adBottomDragFld, myIter, myThid )
#endif

        ELSEIF ( ( dumpAdVarExch.NE.2 ).AND.(dumpAdByRec) ) THEN

          foo3D = theta%d
          CALL WRITE_REC_XYZ_RL( 'ADJtheta',
     &                           foo3d, dumpAdRecMn, myIter,myThid )
          foo3D = salt%d
          CALL WRITE_REC_XYZ_RL( 'ADJsalt',
     &                           foo3d, dumpAdRecMn, myIter, myThid )
          foo3D = uvel%d
          CALL WRITE_REC_XYZ_RL( 'ADJuvel',
     &                           foo3d, dumpAdRecMn, myIter, myThid )
          foo3D = vvel%d
          CALL WRITE_REC_XYZ_RL( 'ADJvvel',
     &                           foo3d, dumpAdRecMn, myIter, myThid )
          foo3D = wvel%d
          CALL WRITE_REC_XYZ_RL( 'ADJwvel',
     &                           foo3d, dumpAdRecMn, myIter, myThid )
          foo2D = etan%d
          CALL WRITE_REC_XY_RL ( 'ADJetan',
     &                           foo2d, dumpAdRecMn, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           foo2D = fu%d
           CALL WRITE_REC_XY_RS('ADJtaux',
     &          foo2d, dumpAdRecMn, myIter, myThid )
           foo2D = fv%d
           CALL WRITE_REC_XY_RS('ADJtauy',
     &          foo2d, dumpAdRecMn, myIter, myThid )
           foo2D = Qnet%d
           CALL WRITE_REC_XY_RS('ADJqnet',
     &          foo2d,dumpAdRecMn, myIter,myThid )
           foo2D = EmPmR%d
           CALL WRITE_REC_XY_RS('ADJempr',
     &          foo2d,dumpAdRecMn, myIter,myThid )
#ifdef SHORTWAVE_HEATING
cc           foo2D = Qsw%d
cc           CALL WRITE_REC_XY_RS('ADJqsw',
cc     &          foo2d,dumpAdRecMn, myIter, myThid )
#endif
          ENDIF

#ifdef ALLOW_GGL90
c         CALL WRITE_REC_XYZ_RL ( 'ADJggl90diffkr',
c    &         adGGL90DiffKr, dumpAdRecMn, myIter, myThid )
cc          foo3D = GGL90TKE%d
cc          CALL WRITE_REC_XYZ_RL( 'ADJggl90tke',
cc     &         foo3d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          foo3D = Diffkr%d
          CALL WRITE_REC_XYZ_RL( 'ADJdiffkr',
     &         foo3d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          foo3D = KapGM%d
          CALL WRITE_REC_XYZ_RL( 'ADJkapgm',
     &         foo3d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          foo3D = KapRedi%d
          CALL WRITE_REC_XYZ_RL( 'ADJkapredi',
     &         foo3d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          foo3D = EddyPsiX%d
          CALL WRITE_REC_XYZ_RS( 'ADJeddypsix',
     &         foo3d, dumpAdRecMn, myIter, myThid )
          foo3D = EddyPsiY%d
          CALL WRITE_REC_XYZ_RS( 'ADJeddypsiy',
     &         foo3d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          foo2D = sst%d
          CALL WRITE_REC_XY_RS( 'ADJsst',
     &         foo2d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          foo2D = sss%d
          CALL WRITE_REC_XY_RS( 'ADJsss',
     &         foo2d, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          foo2D = BottomDragFld%d
          CALL WRITE_REC_XY_RL ( 'ADJbottomdrag',
     &         foo2d, dumpAdRecMn, myIter, myThid )
#endif

        ELSE
C       case dumpAdVarExch = 2

          CALL COPY_ADVAR_OUTP( dumRS, theta%d,var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJtheta.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJtheta',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, salt%d, var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJsalt.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJsalt',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_AD_UV_OUTP( dumRS, dumRS, uVel%d, vVel%d,
     &                                 var3Du, var3Dv, Nr, 34, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJuvel.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJuvel',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJvvel.',
     &                           suff, var3Dv, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJvvel',
     &                           var3Dv, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, wVel%d, var3Du, Nr, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJwvel.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJwvel',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          CALL COPY_ADVAR_OUTP( dumRS, EtaN%d, var2Du, 1 , 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL(  'ADJetan.',
     &                           suff, var2Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL(  'ADJetan',
     &                           var2Du, dumpAdRecMn, myIter, myThid )

          IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
           CALL COPY_AD_UV_OUTP( Fu%d, Fv%d, dumRL, dumRL,
     &                                  var2Du, var2Dv, 1, 33, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJtaux.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJtaux',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJtauy.',
     &                           suff,var2Dv,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJtauy',
     &                           var2Dv,dumpAdRecMn, myIter,myThid )
           CALL COPY_ADVAR_OUTP( Qnet%d, dumRL, var2Du, 1, 11, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJqnet.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJqnet',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
           CALL COPY_ADVAR_OUTP( EmPmR%d,dumRL, var2Du, 1, 11, myThid )
           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJempr.',
     &                           suff,var2Du,myIter,myThid )
           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJempr',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#ifdef SHORTWAVE_HEATING
cc           CALL COPY_ADVAR_OUTP( Qsw%d,  dumRL, var2Du, 1, 11, myThid )
cc           IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJqsw.',
cc     &                           suff,var2Du,myIter,myThid )
cc           IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJqsw',
cc     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
          ENDIF
#ifdef ALLOW_GGL90
cc          CALL COPY_ADVAR_OUTP( dumRS,GGL90TKE%d,var3Du,Nr, 12,myThid )
cc          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJggl90tke.',
cc     &                           suff, var3Du, myIter, myThid )
cc          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJggl90tke',
cc     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, DiffKr%d,var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJdiffkr.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJdiffkr',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, KapGM%d, var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJkapgm.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJkapgm',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS,KapRedi%d,var3Du, Nr, 12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJkapredi.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJkapredi',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_EDDYPSI_CONTROL
          CALL COPY_AD_UV_OUTP( EddyPsiX%d, EddyPsiY%d, dumRL, dumRL,
     &                                 var3Du, var3Dv, Nr, 33, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJeddypsix.',
     &                           suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJeddypsix',
     &                           var3Du, dumpAdRecMn, myIter, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJeddypsiy.',
     &                           suff, var3Dv, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJeddypsiy',
     &                           var3Dv, dumpAdRecMn, myIter, myThid )
#endif
#ifdef ALLOW_SST0_CONTROL
          CALL COPY_ADVAR_OUTP( SST%d, dumRL, var2Du, 1, 11, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJsst.',
     &                           suff,var2Du,myIter,myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJsst',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL COPY_ADVAR_OUTP( SSS%d, dumRL, var2Du, 1, 11, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJsss.',
     &                           suff,var2Du,myIter,myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJsss',
     &                           var2Du,dumpAdRecMn, myIter,myThid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, BottomDragFld%d,
     &                                        var2Du, 1, 12, myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJbottomdrag.',
     &                           suff, var2Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJbottomdrag',
     &                           var2Du, dumpAdRecMn, myIter, myThid )
#endif

C       end if dumpAdVarExch = 2
        ENDIF

C-----------------------------------------------------------------------
#endif /* ndef ALLOW_OPENAD */
C-----------------------------------------------------------------------

C-- NOW NMC output

#ifndef ALLOW_OPENAD
#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN
         CALL MNC_CW_SET_UDIM('adstate', -1, myThid)
         CALL MNC_CW_RL_W_S('D','adstate',0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
         CALL MNC_CW_I_W_S('I','adstate',0,0,'iter',myIter,myThid)
         CALL MNC_CW_RL_W_S('D','adstate',0,0,'model_time',myTime,
     &        myThid)

         IF ( dumpAdVarExch.EQ.2 ) THEN
          CALL COPY_AD_UV_OUTP( dumRS, dumRS, aduVel, advVel,
     &                          var3Du, var3Dv, Nr, 34, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adU', var3Du, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adV', var3Dv, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adTheta,var3Du, Nr, 12, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adT', var3Du, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adSalt,var3Du, Nr, 12, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adS', var3Du, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adEtaN, var2Du, 1 , 12, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEta', var2Du, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adwVel, var3Du, Nr, 12, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adW', var3Du, myThid)

          CALL COPY_ADVAR_OUTP( adQnet, dumRL, var2Du, 1, 11, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adQnet', var2Du, myThid)
          CALL COPY_ADVAR_OUTP( adEmPmR,dumRL, var2Du, 1, 11, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEmpmr', var2Du, myThid)
          CALL COPY_AD_UV_OUTP( adFu, adFv, dumRL, dumRL,
     &                          var2Du, var2Dv, 1, 33, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adFu', var2Du, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adFv', var2Dv, myThid)

#ifdef ALLOW_SST0_CONTROL
          CALL COPY_ADVAR_OUTP( adSST, dumRL, var2Du, 1, 11, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adSST', var2Du, myThid)
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL COPY_ADVAR_OUTP( adSSS, dumRL, var2Du, 1, 11, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,'adSSS', var2Du, myThid)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adBottomDragFld,
     &                                        var2Du, 1, 12, myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adBottomDrag', var2Du, myThid)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adDiffKr,var3Du, Nr, 12,myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adDiffkr', var3Du, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS, adKapGM, var3Du, Nr, 12,myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adkapgm', var3Du, myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL COPY_ADVAR_OUTP( dumRS,adKapRedi,var3Du, Nr, 12,myThid )
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adkapredi', var3Du, myThid)
#endif
         ELSE
C     dumpAdVarExch.NE.2
          CALL MNC_CW_RL_W('D','adstate',0,0,'adU', aduVel, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adV', advVel, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adT', adtheta, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adS', adsalt, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEta', adetaN, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adW', adwVel, myThid)

          CALL MNC_CW_RS_W('D','adstate',0,0,'adQnet', adQnet, myThid)
          CALL MNC_CW_RS_W('D','adstate',0,0,'adEmpmr', adEmpmr, myThid)
          CALL MNC_CW_RS_W('D','adstate',0,0,'adFu', adfu, myThid)
          CALL MNC_CW_RS_W('D','adstate',0,0,'adFv', adfv, myThid)

#ifdef ALLOW_SST0_CONTROL
          CALL MNC_CW_RS_W('D','adstate',0,0,'adSST', adsst, myThid)
#endif
#ifdef ALLOW_SSS0_CONTROL
          CALL MNC_CW_RS_W('D','adstate',0,0,'adSSS', adsss, myThid)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adBottomDrag', adbottomdragfld, myThid)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adDiffkr', addiffkr, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adkapgm', adkapgm, myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adkapredi', adkapredi, myThid)
#endif
         ENDIF
C     endif mnc
        ENDIF
#endif /* ALLOW_MNC */
#endif /* ALLOW_OPENAD */

#ifdef ALLOW_EXF
cph        IF ( useEXF ) CALL EXF_AD_DUMP( myTime, myIter, myThid )
#endif

#ifdef ALLOW_SEAICE
        IF ( useSEAICE )
     &   CALL SEAICE_AD_DUMP( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS )
     &   CALL PTRACERS_AD_DUMP( myTime, myIter, myThid )
#endif

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
