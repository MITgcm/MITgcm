#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
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
#ifdef ALLOW_OPENAD
      use OAD_active
      use OAD_rev
      use OAD_tape
      use OAD_cp
#endif
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_OPENAD
C- Note: Since OpenAD uses modules, the ordering of included headers matters
#  include "DYNVARS.h"
#  include "FFIELDS.h"
#  ifdef ALLOW_GGL90
#   include "GGL90.h"
#  endif
# endif /* ALLOW_OPENAD */
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# ifndef ALLOW_OPENAD
#  include "adcommon.h"
# endif /* ALLOW_OPENAD */
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
# ifdef ALLOW_SHELFICE
#  include "SHELFICE_OPTIONS.h"
# endif
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
C     == local variables ==
C     suff    :: Hold suffix part of a filename
C     msgBuf  :: Error message buffer
c     CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      _RL var2Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var2Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var3Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL var3Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RS dumRS(1)
      _RL dumRL(1)
      LOGICAL doExch
      LOGICAL doDump
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL modelEnd
#endif
CEOP

C--- Do dump?
      doDump = DIFFERENT_MULTIPLE( adjDumpFreq, myTime, deltaTClock )

C--- Test for adexch based on dumpAdVarExch flag
      doExch = doDump
#ifdef ALLOW_DIAGNOSTICS
      doExch = doDump .OR. useDiag4AdjOutp
#endif
      doExch = doExch .AND. ( dumpAdVarExch.EQ.1 )

C--- If necessary, do the exchanges
      IF ( doExch ) THEN
#ifdef ALLOW_OPENAD
C--   need to all the correct OpenAD EXCH S/R ; left empty for now
#else /* ALLOW_OPENAD */

#ifdef AUTODIFF_TAMC_COMPATIBILITY
        call adexch_xyz_rl( myThid,adTheta)
        call adexch_xyz_rl( myThid,adSalt)
        call adexch_xyz_rl( myThid,adwVel )
        call adexch_uv_xyz_rl(  .true. ,myThid,aduVel,advVel )

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
# ifdef ALLOW_SHELFICE
#  ifndef SHI_ALLOW_GAMMAFRICT
        CALL ADEXCH_XY_RL( adshiTransCoeffT, myThid )
        CALL ADEXCH_XY_RL( adshiTransCoeffS, myThid )
#  else
        CALL ADEXCH_XY_RL( adshiCDragFld, myThid )
#  endif
# endif

#endif /* AUTODIFF_TAMC_COMPATIBILITY */
#endif /* ALLOW_OPENAD */
      ENDIF

C=======================================================================
C--  Dump AD-variable to binary files and fill diagnostics
C=======================================================================
      IF ( doDump ) THEN
C     Increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecMn = dumpAdRecMn + 1
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#ifndef ALLOW_OPENAD
C========================
C--  TAF AD-variables:
C========================
C-----------------------------------------------------------------------
C--- State variables
C-----------------------------------------------------------------------
      CALL DUMP_ADJ_XYZ(dumRS, adTheta, 'ADJtheta', 'ADJtheta.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ(dumRS, adSalt, 'ADJsalt ', 'ADJsalt.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ(dumRS, adWvel, 'ADJwvel ', 'ADJwvel.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ_UV(
     &     dumRS, adUvel, 'ADJuvel ', 'ADJuvel.',
     &     dumRS, adVvel, 'ADJvvel ', 'ADJvvel.',
     &     34, doDump, dumpAdRecMn, myTime, myIter,myThid)

C-----------------------------------------------------------------------
C--- Surface Forcing
C-----------------------------------------------------------------------
      IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
       CALL DUMP_ADJ_XY_UV(
     &      adFu, dumRL, 'ADJtaux ', 'ADJtaux.',
     &      adFv, dumRL, 'ADJtauy ', 'ADJtauy.',
     &      33, doDump, dumpAdRecMn, myTime, myIter,myThid)
        CALL DUMP_ADJ_XY(adQnet, dumRL, 'ADJqnet ', 'ADJqnet.',
     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
        CALL DUMP_ADJ_XY(adEmPmR, dumRL, 'ADJempmr', 'ADJempr.',
     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)

# ifdef SHORTWAVE_HEATING
        CALL DUMP_ADJ_XY(adQsw, dumRL, 'ADJqsw  ', 'ADJqsw.',
     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
      ENDIF
# ifdef ALLOW_SST0_CONTROL
      CALL DUMP_ADJ_XY(adSST, dumRL, 'ADJsst  ', 'ADJsst.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_SSS0_CONTROL
      CALL DUMP_ADJ_XY(adSSS, dumRL, 'ADJsss  ', 'ADJsss.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif

C-----------------------------------------------------------------------
C---Various packages and parameters
C-----------------------------------------------------------------------
# ifdef ALLOW_BOTTOMDRAG_CONTROL
      CALL DUMP_ADJ_XY(dumRS, adBottomDragFld,
     &                 'ADJbtdrg','ADJbottomdrag.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_GGL90
      CALL DUMP_ADJ_XYZ(dumRS, adGGL90TKE, 'ADJtke90', 'ADJggl90tke.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_DIFFKR_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, adDiffKr, 'ADJdifkr', 'ADJdiffkr.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_KAPGM_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, adKapGM, 'ADJkapgm', 'ADJkapgm.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_KAPREDI_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, adKapRedi, 'ADJkapre', 'ADJkapredi.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif

# ifdef ALLOW_EDDYPSI_CONTROL
      CALL DUMP_ADJ_XYZ_UV(
     &     adEddyPsiX,dumRL,'ADJepsix','ADJeddipsix.',
     &     adEddyPsiY,dumRL,'ADJepsiy','ADJeddipsiy.',
     &     33, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_SHELFICE
#  ifndef SHI_ALLOW_GAMMAFRICT
      CALL DUMP_ADJ_XY(dumRS, adshiTransCoeffT,
     &                 'ADJshict','ADJshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
      CALL DUMP_ADJ_XY(dumRS, adshiTransCoeffS,
     &                 'ADJshics','ADJshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  else
      CALL DUMP_ADJ_XY(dumRS, adshiCDragFld,
     &                 'ADJshicd','ADJshicd.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  endif
# endif

#else /* ALLOW_OPENAD */
C========================
C--  OpenAD AD-variable:
C========================
C-----------------------------------------------------------------------
C--- State variables
C-----------------------------------------------------------------------
      CALL DUMP_ADJ_XYZ(dumRS, theta%d, 'ADJtheta', 'ADJtheta.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ(dumRS, salt%d, 'ADJsalt ', 'ADJsalt.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ(dumRS, wvel%d, 'ADJwvel ', 'ADJwvel.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)

      CALL DUMP_ADJ_XYZ_UV(
     &     dumRS, uvel%d, 'ADJuvel ', 'ADJuvel.',
     &     dumRS, vvel%d, 'ADJvvel ', 'ADJvvel.',
     &     34, doDump, dumpAdRecMn, myTime, myIter,myThid)

C-----------------------------------------------------------------------
C--- Surface Forcing
C-----------------------------------------------------------------------
      IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
       CALL DUMP_ADJ_XY_UV(
     &      Fu%d, dumRL, 'ADJtaux ', 'ADJtaux.',
     &      Fv%d, dumRL, 'ADJtauy ', 'ADJtauy.',
     &      33, doDump, dumpAdRecMn, myTime, myIter,myThid)
        CALL DUMP_ADJ_XY(Qnet%d, dumRL, 'ADJqnet ', 'ADJqnet.',
     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
        CALL DUMP_ADJ_XY(EmPmR%d, dumRL, 'ADJempmr', 'ADJempr.',
     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)

# ifdef SHORTWAVE_HEATING
C     Commented out since only works when Qsw is active
c       CALL DUMP_ADJ_XY(Qsw%d, dumRL, 'ADJqsw  ', 'ADJqsw.',
c    &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
      ENDIF
# ifdef ALLOW_SST0_CONTROL
      CALL DUMP_ADJ_XY(SST%d, dumRL, 'ADJsst  ', 'ADJsst.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_SSS0_CONTROL
      CALL DUMP_ADJ_XY(SSS%d, dumRL, 'ADJsss  ', 'ADJsss.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif

C-----------------------------------------------------------------------
C---Various packages and parameters
C-----------------------------------------------------------------------
# ifdef ALLOW_BOTTOMDRAG_CONTROL
      CALL DUMP_ADJ_XY(dumRS, BottomDragFld%d,
     &                 'ADJbtdrg','ADJbottomdrag.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_GGL90
      CALL DUMP_ADJ_XYZ(dumRS, GGL90TKE%d, 'ADJtke90', 'ADJggl90tke.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_DIFFKR_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, DiffKr%d, 'ADJdifkr', 'ADJdiffkr.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_KAPGM_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, KapGM%d, 'ADJkapgm', 'ADJkapgm.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_KAPREDI_CONTROL
      CALL DUMP_ADJ_XYZ(dumRS, KapRedi%d, 'ADJkapre', 'ADJkapredi.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif

# ifdef ALLOW_EDDYPSI_CONTROL
      CALL DUMP_ADJ_XYZ_UV(
     &     EddyPsiX%d,dumRL,'ADJepsix','ADJeddipsix.',
     &     EddyPsiY%d,dumRL,'ADJepsiy','ADJeddipsiy.',
     &     33, doDump, dumpAdRecMn, myTime, myIter,myThid)
# endif
# ifdef ALLOW_SHELFICE
#  ifndef SHI_ALLOW_GAMMAFRICT
      CALL DUMP_ADJ_XY(dumRS, shiTransCoeffT%d,
     &                 'ADJshict','ADJshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
      CALL DUMP_ADJ_XY(dumRS, shiTransCoeffS%d,
     &                 'ADJshics','ADJshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  else
      CALL DUMP_ADJ_XY(dumRS, shiCDragFld%d,
     &                 'ADJshicd','ADJshicd.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  endif
# endif
#endif /* ALLOW_OPENAD */

C=======================================================================
C--  Dump TAF AD-variable to NetCdF files
C=======================================================================
      IF ( doDump ) THEN
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
          CALL MNC_CW_RL_W('D','adstate',0,0,'adT', adTheta, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adS', adSalt, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adW', adwVel, myThid)

          CALL MNC_CW_RS_W('D','adstate',0,0,'adQnet', adQnet, myThid)
          CALL MNC_CW_RS_W('D','adstate',0,0,'adEmpmr',adEmpmr, myThid)
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
#ifdef ALLOW_SHIELFICE
# ifndef SHI_ALLOW_GAMMAFRICT
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adshict', adshiTransCoeffT, myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adshics', adshiTransCoeffS, myThid)
# else
          CALL MNC_CW_RL_W('D','adstate',0,0,
     &                     'adshicd', adshiCDragFld, myThid)
# endif
#endif
         ENDIF
C     endif mnc
        ENDIF
#endif /* ALLOW_MNC */
#endif /* ALLOW_OPENAD */

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

C=======================================================================
C--  Output AD-variable & fill Diagnostics with pkg specific interface
C=======================================================================
#ifdef ALLOW_SEAICE
        IF ( useSEAICE )
     &   CALL SEAICE_AD_DUMP( myTime, myIter, myThid )
#endif

#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS )
     &   CALL PTRACERS_AD_DUMP( myTime, myIter, myThid )
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
