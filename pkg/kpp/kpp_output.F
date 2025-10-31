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
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_OFFLINE
# include "OFFLINE_SWITCH.h"
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
C     variable in local common block:
C     kpp_drctrec  :: next record to dump for KPP files
      INTEGER kpp_drctrec
      COMMON / KPP_RECORDNUM1 / kpp_drctrec
C     local variable:
      CHARACTER*(10) suff
#ifdef ALLOW_DIAGNOSTICS
      INTEGER bi, bj
      INTEGER i, j, k
      _RL tmpFac
      _RL tmpLoc(1:sNx,1:sNy,Nr)
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_OFFLINE
      IF ( .NOT.(useOffLine.AND.offlineLoadKPP) ) THEN
#else
      IF ( .TRUE. ) THEN
#endif

C     Initialize record numbers
      IF ( myIter.EQ.nIter0 ) THEN
        _BEGIN_MASTER( myThid )
        kpp_drctrec = 1
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
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I10.10)') myIter
          ELSE
            CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
          ENDIF
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

C--   end if useOffLine
      ENDIF

#endif /* ALLOW_KPP */

      RETURN
      END
