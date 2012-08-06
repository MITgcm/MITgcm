C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_output.F,v 1.6 2012/08/06 16:56:59 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#undef  MULTIPLE_RECORD_KPP_STATE_FILES

CBOP
C     !ROUTINE: KPP_OUTPUT

C     !INTERFACE:
      SUBROUTINE KPP_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Create the KPP diagnostic output.
C
C     The following CPP flag (MULTIPLE_RECORD_KPP_STATE_FILES) is
C     #define/#undefed here since it is specific to this routine and
C     very user-preference specific.
C
C     If #undefed (default) the state files are written as in all
C     versions prior to checkpoint32, where a file is created per
C     variable, per time and per tile. This *has* to be the default
C     because most users use this mode and all utilities and scripts
C     (diagnostic) assume this form.  It is also robust, as explained
C     below.
C
C     If #defined, subsequent snap-shots are written as records in the
C     same file (no iteration number in filenames).
C
C     Advantages:
C     - fewer files
C     - for small problems, is easy to copy the output around
C     Disadvantages:
C     - breaks a lot of diagnostic scripts
C     - for large or long problems this creates huge files
C     - is an unexpected, unsolicited change in behaviour which came
C     as a surprise (in c32) and inconvenience to several users
C     - can not accomodate changing the frequency of output
C     after a pickup (this is trivial in previous method
C     but needs new code and parameters in this new method)

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "KPP_TAVE.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

C     !LOCAL VARIABLES:
C     kpp_drctrec  :: next record to dump for KPP files
      INTEGER kpp_drctrec
      COMMON / KPP_RECORDNUM1 / kpp_drctrec
C     local variable:
      CHARACTER*(MAX_LEN_MBUF) suff
#if (defined ALLOW_TIMEAVE) || (defined ALLOW_DIAGNOSTICS)
      INTEGER bi, bj
      INTEGER i, j, k
      _RL tmpFac
#endif
#ifdef ALLOW_TIMEAVE
      _RL DDTT
      LOGICAL dumpFiles
#endif
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpLoc(1:sNx,1:sNy,Nr)
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Initialize record numbers in KPP_TAVE
      IF ( myIter.EQ.nIter0 ) THEN
        _BEGIN_MASTER( myThid )
        kpp_drctrec = 1
#ifdef ALLOW_TIMEAVE
        kpp_drctrecTave = 1
#endif
        _END_MASTER( myThid )
        _BARRIER
      ENDIF

C----------------------------------------------------------------
C     Dump snapshot of KPP variables.

      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(kpp_dumpFreq,myTime,deltaTClock)
     &     ) THEN

        IF (KPPwriteState .AND. snapshot_mdsio) THEN
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C         Write each snap-shot as a new record in one file per variable
C         - creates relatively few files but these files can become huge
C         NOTE: file size and number problems are *SOLVED* by MNC
          CALL WRITE_REC_XYZ_RL('KPPviscAz',KPPviscAz,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzT',KPPdiffKzT,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzS',KPPdiffKzS,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPghat',KPPghat,kpp_drctrec,
     &         myIter,myThid)
          CALL WRITE_REC_XY_RL('KPPhbl',KPPhbl,kpp_drctrec,
     &         myIter,myThid)
C--     Increment record counter
          _BARRIER
          _BEGIN_MASTER( myThid )
          kpp_drctrec = kpp_drctrec + 1
          _END_MASTER( myThid )
          _BARRIER
#else /* MULTIPLE_RECORD_KPP_STATE_FILES */
C         Write each snap-shot as a new file - creates many files but
C         for large configurations is easier to transfer
C         NOTE: file size and number problems are *SOLVED* by MNC
          WRITE(suff,'(I10.10)') myIter
          CALL WRITE_FLD_XYZ_RL('KPPviscAz.',suff,KPPviscAz,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzT.',suff,KPPdiffKzT,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzS.',suff,KPPdiffKzS,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPghat.',suff,KPPghat,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('KPPhbl.',suff,KPPhbl,
     &         myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */
        ENDIF

#ifdef ALLOW_MNC
        IF (KPPwriteState .AND. useMNC .AND. snapshot_mnc) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
          CALL MNC_CW_SET_UDIM('kpp_state', -1, myThid)
          CALL MNC_CW_RL_W_S('D','kpp_state',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('kpp_state', 0, myThid)
          CALL MNC_CW_I_W_S('I','kpp_state',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPviscAz', KPPviscAz, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPdiffKzT', KPPdiffKzT, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPdiffKzS', KPPdiffKzS, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPGHAT', KPPghat, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_state',0,0,
     &         'KPPHBL', KPPhbl, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */

      ENDIF

C----------------------------------------------------------------
C     Do KPP time averaging.

#ifdef ALLOW_TIMEAVE
      IF ( KPP_taveFreq .GT. 0. _d 0 ) THEN

C     Initialize averages to zero
       IF ( myIter.EQ.nIter0 ) THEN

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPghatKStave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
            KPP_timeAve(bi,bj) = 0.
          ENDDO
        ENDDO

       ELSE

C     Time Average KPP fields
        DDTT=deltaTclock
        IF ( useGMRedi .AND. KPP_ghatUseTotalDiffus ) THEN
          tmpFac = 1. _d 0
        ELSE
          tmpFac = 0. _d 0
        ENDIF
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPviscAztave, KPPviscAz, Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPdiffKzTtave,KPPdiffKzT,Nr,DDTT,bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(
     &         KPPdiffKzStave,KPPdiffKzS,Nr,DDTT,bi,bj,myThid)
          DO k=2,Nr
           DO j=1,sNy
            DO i=1,sNx
              KPPghatKStave(i,j,k,bi,bj) = KPPghatKStave(i,j,k,bi,bj)
     &          + ( KPPdiffKzS(i,j,k,bi,bj)
#ifdef ALLOW_GMREDI
     &             +tmpFac*Kwz(i,j,k,bi,bj)
#endif
     &            )*KPPghat(i,j,k-1,bi,bj)*DDTT
            ENDDO
           ENDDO
          ENDDO
          CALL TIMEAVE_CUMULATE(
     &         KPPhbltave,    KPPhbl,    1, DDTT,bi,bj,myThid)
C         Keep record of how much time has been integrated over
          KPP_timeAve(bi,bj) = KPP_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO

       ENDIF

C     Dump files and restart average computation if needed
       dumpFiles = .FALSE.
       IF ( myIter .NE. nIter0 ) THEN
        dumpFiles =
     &     DIFFERENT_MULTIPLE(KPP_taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( zeroRL, KPP_taveFreq, deltaTClock,
     U                        dumpFiles,
     I                        myTime, myIter, myThid )
        ENDIF
#endif
       ENDIF

       IF (dumpFiles) THEN
C       Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_NORMALIZE( KPPviscAztave,
     &                              KPP_timeAve, Nr, bi, bj, myThid )
            CALL TIMEAVE_NORMALIZE( KPPdiffKzTtave,
     &                              KPP_timeAve, Nr, bi, bj, myThid )
            CALL TIMEAVE_NORMALIZE( KPPghatKStave,
     &                              KPP_timeAve, Nr, bi, bj, myThid )
            CALL TIMEAVE_NORMALIZE( KPPdiffKzStave,
     &                              KPP_timeAve, Nr, bi, bj, myThid )
            CALL TIMEAVE_NORMALIZE( KPPhbltave,
     &                              KPP_timeAve, 1 , bi, bj, myThid )
          ENDDO
        ENDDO

        IF (KPPwriteState .AND. timeave_mdsio) THEN
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C         Write each tave output as a new record in one file per variable
C         - creates relatively few files but these files can become huge
C         NOTE: file size and number problems are *SOLVED* by MNC
          CALL WRITE_REC_XYZ_RL('KPPviscAz-T',KPPviscAztave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzT-T',KPPdiffKzTtave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPdiffKzS-T',KPPdiffKzStave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XYZ_RL('KPPghatKS-T',KPPghatKStave,
     &         kpp_drctrecTave,myIter,myThid)
          CALL WRITE_REC_XY_RL('KPPhbl-T',KPPhbltave,
     &         kpp_drctrecTave,myIter,myThid)
C--       Increment record counter
          _BARRIER
          _BEGIN_MASTER( myThid )
          kpp_drctrecTave = kpp_drctrecTave + 1
          _END_MASTER( myThid )
          _BARRIER
#else /* MULTIPLE_RECORD_KPP_STATE_FILES */
C         Write each tave output as a new file - creates many files but for
C         large configurations is easier to transfer
C         NOTE: file size and number problems are *SOLVED* by MNC
          WRITE(suff,'(I10.10)') myIter
          CALL WRITE_FLD_XYZ_RL('KPPviscAz-T.',suff,KPPviscAztave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzT-T.',suff,KPPdiffKzTtave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPdiffKzS-T.',suff,KPPdiffKzStave,
     &         myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('KPPghatKS-T.',suff,KPPghatKStave,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('KPPhbl-T.',suff,KPPhbltave,
     &         myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */
        ENDIF

#ifdef ALLOW_MNC
        IF (KPPwriteState .AND. useMNC .AND. timeave_mnc) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
          CALL MNC_CW_SET_UDIM('kpp_timeave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','kpp_timeave',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('kpp_timeave', 0, myThid)
          CALL MNC_CW_I_W_S('I','kpp_timeave',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPviscAz', KPPviscAztave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPdiffKzT', KPPdiffKzTtave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPdiffKzS', KPPdiffKzStave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPghatKS', KPPghatKStave, myThid)
          CALL MNC_CW_RL_W(pf,'kpp_timeave',0,0,
     &         'KPPHBL', KPPhbltave, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */

C       Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPghatKStave, Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
            CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
            KPP_timeAve(bi,bj) = 0.
          ENDDO
        ENDDO

C--   end if dumpFiles
       ENDIF

C--   end if KPP_taveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
C     do not fill during call from INITIALISE_VARIA
      IF ( useDiagnostics .AND. myIter.NE.nIter0 ) THEN
       CALL DIAGNOSTICS_FILL(KPPviscAz ,'KPPviscA',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPdiffKzS,'KPPdiffS',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPdiffKzT,'KPPdiffT',0,Nr,0,1,1,myThid)
       IF ( DIAGNOSTICS_IS_ON( 'KPPghatK', myThid ) ) THEN
        IF ( useGMRedi .AND. KPP_ghatUseTotalDiffus ) THEN
          tmpFac = 1. _d 0
        ELSE
          tmpFac = 0. _d 0
        ENDIF
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
            DO i=1,sNx
              tmpLoc(i,j,1) = 0. _d 0
            ENDDO
          ENDDO
          DO k=2,Nr
           DO j=1,sNy
            DO i=1,sNx
              tmpLoc(i,j,k) = KPPghat(i,j,k-1,bi,bj)*
     &                      ( KPPdiffKzS(i,j,k,bi,bj)
#ifdef ALLOW_GMREDI
     &                      + tmpFac*Kwz(i,j,k,bi,bj)
#endif
     &                      )
            ENDDO
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmpLoc,'KPPghatK',0,Nr,3,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF
       CALL DIAGNOSTICS_FILL(KPPhbl    ,'KPPhbl  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KPPfrac   ,'KPPfrac ',0,1 ,0,1,1,myThid)
#ifdef ALLOW_SALT_PLUME
       CALL DIAGNOSTICS_FILL(KPPplumefrac,'KPPpfrac',0,1 ,0,1,1,myThid)
#endif /* ALLOW_SALT_PLUME */
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_KPP */

      RETURN
      END
