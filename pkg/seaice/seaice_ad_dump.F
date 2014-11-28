C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_dump.F,v 1.13 2014/11/28 14:17:13 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: seaice_ad_dump
C     !INTERFACE:
      subroutine seaice_ad_dump( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE seaice_ad_dump
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
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
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL var2Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var2Dv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dumRS(1)
      _RL dumRL(1)
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     &   ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

c increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecSi=dumpAdRecSi+1
c#ifdef ALLOW_DEBUG
c      IF ( debugMode ) print*,'dumpAdRecSi',dumpAdRecSi
c#endif

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
C ==>> Resetting run-time parameter writeBinaryPrec in the middle of a run
C ==>>  is very very very nasty !!!
c       writeBinaryPrec = writeStatePrec
C <<==  If you really want to mess-up with this at your own risk,
C <<==  uncomment the line above

        IF ( dumpAdVarExch.EQ.1 ) THEN

         CALL ADEXCH_XY_RL( adarea, myThid )
         CALL ADEXCH_XY_RL( adheff, myThid )
         CALL ADEXCH_XY_RL( adhsnow, myThid )
         CALL ADEXCH_UV_3D_RL( aduice,advice, .TRUE., 1, myThid )

        ENDIF

        IF (dumpAdVarExch.NE.2 .AND. .NOT.dumpAdByRec) THEN

         CALL WRITE_REC_3D_RL(
     &        'ADJarea.'//suff, writeBinaryPrec,
     &        1,  adarea, 1, myIter, myThid )
         CALL WRITE_REC_3D_RL(
     &        'ADJheff.'//suff, writeBinaryPrec,
     &        1,  adheff, 1, myIter, myThid )
         CALL WRITE_REC_3D_RL(
     &        'ADJhsnow.'//suff, writeBinaryPrec,
     &        1,  adhsnow, 1, myIter, myThid )
# ifdef SEAICE_ALLOW_DYNAMICS
cph        IF ( SEAICEuseDynamics ) THEN
          CALL WRITE_REC_3D_RL(
     &        'ADJuice.'//suff, writeBinaryPrec,
     &        1,  aduice, 1, myIter, myThid )
          CALL WRITE_REC_3D_RL(
     &         'ADJvice.'//suff, writeBinaryPrec,
     &         1,  advice, 1, myIter, myThid )
cph        ENDIF
# endif

        ELSEIF (dumpAdVarExch.NE.2 .AND. .NOT.dumpAdByRec) THEN

         CALL WRITE_REC_3D_RL(
     &        'ADJarea', writeBinaryPrec,
     &        1,  adarea, dumpAdRecSi, myIter, myThid )
         CALL WRITE_REC_3D_RL(
     &        'ADJheff', writeBinaryPrec,
     &        1,  adheff, dumpAdRecSi, myIter, myThid )
         CALL WRITE_REC_3D_RL(
     &        'ADJhsnow', writeBinaryPrec,
     &        1,  adhsnow, dumpAdRecSi, myIter, myThid )
# ifdef SEAICE_ALLOW_DYNAMICS
cph        IF ( SEAICEuseDynamics ) THEN
          CALL WRITE_REC_3D_RL(
     &       'ADJuice', writeBinaryPrec,
     &       1,  aduice, dumpAdRecSi, myIter, myThid )
          CALL WRITE_REC_3D_RL(
     &       'ADJvice', writeBinaryPrec,
     &       1,  advice, dumpAdRecSi, myIter, myThid )
cph        ENDIF
# endif
        ELSE
C       case dumpAdVarExch = 2
         CALL COPY_ADVAR_OUTP( dumRS, adarea, var2Du, 1 , 12, myThid )
         IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL(  'ADJarea.',
     &                         suff, var2Du, myIter, myThid )
         IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL(  'ADJarea',
     &                         var2Du, dumpAdRecMn, myIter, myThid )
         CALL COPY_ADVAR_OUTP( dumRS, adheff, var2Du, 1 , 12, myThid )
         IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL(  'ADJheff.',
     &                         suff, var2Du, myIter, myThid )
         IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL(  'ADJheff',
     &                         var2Du, dumpAdRecMn, myIter, myThid )
         CALL COPY_ADVAR_OUTP( dumRS, adhsnow, var2Du, 1 , 12, myThid )
         IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL(  'ADJhsnow.',
     &                         suff, var2Du, myIter, myThid )
         IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL(  'ADJhsnow',
     &                         var2Du, dumpAdRecMn, myIter, myThid )

# ifdef SEAICE_ALLOW_DYNAMICS
         CALL COPY_AD_UV_OUTP( dumRS, dumRS, aduice, advice,
     &                        var2Du, var2Dv, 1, 34, myThid )
         IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJuice.',
     &                         suff, var2Du, myIter, myThid )
         IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJuice',
     &                         var2Du, dumpAdRecMn, myIter, myThid )
         IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XY_RL( 'ADJvice.',
     &                         suff, var2Dv, myIter, myThid )
         IF (  dumpAdByRec   ) CALL WRITE_REC_XY_RL( 'ADJvice',
     &                         var2Dv, dumpAdRecMn, myIter, myThid )
# endif
        ENDIF

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
# ifdef SEAICE_ALLOW_DYNAMICS
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
# ifdef SEAICE_ALLOW_DYNAMICS
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
