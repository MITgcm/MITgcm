#include "SEAICE_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_AD_DUMP
C     !INTERFACE:
      SUBROUTINE SEAICE_AD_DUMP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE seaice_ad_dump
C     *==========================================================*
C     Extract adjoint variable from TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE seaice_ad_dump
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# include "adcommon.h"
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_P2SHARE.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
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
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
c     CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      _RL var2Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var2Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      _RS dumRS(1)
      LOGICAL doExch
      LOGICAL doDump
CEOP

C--- do dump?
      doDump = DIFFERENT_MULTIPLE( adjDumpFreq, myTime, deltaTClock )

C--- Test for adexch based on dumpAdVarExch flag
      doExch = doDump
#ifdef ALLOW_DIAGNOSTICS
      doExch = doDump .OR. useDiag4AdjOutp
#endif
      doExch = doExch .AND. ( dumpAdVarExch.EQ.1 )

C--- If necessary, do the exchanges
      IF ( doExch ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
#ifdef ALLOW_OPENAD
C--   need to all the correct OpenAD EXCH S/R ; left empty for now
#else /* ALLOW_OPENAD */

# ifdef AUTODIFF_TAMC_COMPATIBILITY
        CALL ADEXCH_XY_RL( myThid, adarea )
        CALL ADEXCH_XY_RL( myThid, adheff )
        CALL ADEXCH_XY_RL( myThid, adhsnow )
        CALL ADEXCH_UV_XYZ_RL( .TRUE., myThid, aduice,advice )
# else
        CALL ADEXCH_XY_RL( adarea, myThid )
        CALL ADEXCH_XY_RL( adheff, myThid )
        CALL ADEXCH_XY_RL( adhsnow, myThid )
        CALL ADEXCH_UV_3D_RL( aduice,advice, .TRUE., 1, myThid )
# endif /* AUTODIFF_TAMC_COMPATIBILITY */
#endif /* ALLOW_OPENAD */
        ENDIF

C-----------------------------------------------------------------------
C--- Turn diagnostics on/off
C    <-- This has already been done in autodiff_inadmode_set_ad.F

C-----------------------------------------------------------------------
C--- do dump?
C-----------------------------------------------------------------------
      IF ( doDump ) THEN
c increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecSi=dumpAdRecSi+1
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

C-----------------------------------------------------------------------
C--- Seaice state variables
C-----------------------------------------------------------------------
      CALL DUMP_ADJ_XY( dumRS, adarea, 'ADJarea ', 'ADJarea.',
     &     12, doDump, dumpAdRecSi, myTime, myIter, myThid )
      CALL DUMP_ADJ_XY( dumRS, adheff, 'ADJheff ', 'ADJheff.',
     &     12, doDump, dumpAdRecSi, myTime, myIter, myThid )
      CALL DUMP_ADJ_XY( dumRS, adhsnow, 'ADJhsnow', 'ADJhsnow.',
     &     12, doDump, dumpAdRecSi, myTime, myIter, myThid )
# if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
C     IF (SEAICEuseDYNAMICS) THEN
      CALL DUMP_ADJ_XY_UV(
     &     dumRS, aduice, 'ADJuice ', 'ADJuice.',
     &     dumRS, advice, 'ADJvice ', 'ADJvice.',
     &     34, doDump, dumpAdRecSi, myTime, myIter, myThid)
C     ENDIF
# endif

      IF ( doDump ) THEN
#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN

         CALL MNC_CW_SET_UDIM('adseaice', -1, myThid)
         CALL MNC_CW_RL_W_S('D','adseaice',0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM('adseaice', 0, myThid)
         CALL MNC_CW_I_W_S('I','adseaice',0,0,'iter',myIter,myThid)
         CALL MNC_CW_RL_W_S('D','adseaice',0,0,'model_time',myTime,
     &        myThid)
c
         IF (dumpAdVarExch.EQ.2) THEN
          CALL COPY_ADVAR_OUTP( dumRS, adarea, var2Du, 1 , 12, myThid )
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adarea',
     &         var2Du, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adheff, var2Du, 1 , 12, myThid )
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adheff',
     &         var2Du, myThid)
          CALL COPY_ADVAR_OUTP( dumRS, adhsnow, var2Du, 1 , 12, myThid )
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adhsnow',
     &         var2Du, myThid)
# if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
C          IF (SEAICEuseDYNAMICS) THEN
          CALL COPY_AD_UV_OUTP( dumRS, dumRS, aduice, advice,
     &         var2Du, var2Dv, 1, 34, myThid )
          CALL MNC_CW_RL_W('D','adseaice',0,0,'aduice',
     &         var2Du, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'advice',
     &         var2Dv, myThid)
C          ENDIF
# endif
         ELSE
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adarea',
     &         adarea, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adheff',
     &         adheff, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'adhsnow',
     &         adhsnow, myThid)
# if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
C          IF (SEAICEuseDYNAMICS) THEN
          CALL MNC_CW_RL_W('D','adseaice',0,0,'aduice',
     &         aduice, myThid)
          CALL MNC_CW_RL_W('D','adseaice',0,0,'advice',
     &         advice, myThid)
C          ENDIF
# endif
         ENDIF

        ENDIF
#endif /* ALLOW_MNC */

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
