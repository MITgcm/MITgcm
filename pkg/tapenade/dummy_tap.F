#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   File dummy_tap.F: Tapenade output hooks and adjoint-mode switches
C--
C--   Hand-written derivatives of the hooks through which a Tapenade-
C--   generated adjoint or tangent-linear model writes the ADJ* and G_J*
C--   output that pkg/autodiff writes from a TAF-generated one, and of
C--   the two adjoint-mode switches (AUTODIFF_INADMODE_SET / _UNSET).
C--
C--   TAF forces its hand-written adjoint of a hook such as
C--   DUMMY_IN_STEPPING( myTime, myIter, myThid ) into the reverse sweep
C--   with the ADNAME and REQUIRED flow directives. Tapenade has no such
C--   directive: an external whose arguments are all passive is absent
C--   from the generated adjoint. The Tapenade hooks below therefore
C--   carry the field they act on as an argument, and
C--   tools/TAP_support/flow_tap declares that argument read-then-
C--   written; Tapenade then generates the call to the hook's _B
C--   (adjoint), _D (tangent) or _FWD/_BWD (adjoint when the hook itself
C--   is differentiated without checkpointing, as -defaultnocheckpoint
C--   does) routine at the mirror image of the forward call, with the
C--   derivative of the field as an extra argument. The routines here
C--   are those derivatives; every forward hook is a no-op. They are
C--   called from DUMMY_IN_STEPPING_TAP (dummy_in_stepping_tap.F, the
C--   routine Tapenade differentiates), from INTEGR_CONTINUITY (the etaN
C--   hook) and from FORWARD_STEP (the mode switches).
C--
C--   One field per hook call keeps the argument list of every generated
C--   call fixed, whatever the configuration. Tapenade generates the
C--   derivative call of an external only when one of its inputs is
C--   active, and may leave the derivative of an argument out of that
C--   call when it finds the adjoint unnecessary; a hook carrying several
C--   fields, some of which exist or are active only in some
C--   configurations, would therefore be called with a
C--   configuration-dependent argument list, which Fortran 77 cannot
C--   detect. With one field per call, a passive field loses its call
C--   entirely and an active one is always passed with its derivative.
C--
C--   Contents:
C--    o  DUMMY_IN_STEPPING_TAP_PREP, DUMMY_IN_STEPPING_TAP_TLM: helpers
C--    and, each with a forward no-op and its _B, _D, _FWD, _BWD:
C--    o  DUMMY_IN_STEPPING_XYZ_RL     3-D _RL field at cell centre
C--    o  DUMMY_IN_STEPPING_XY_RS      2-D _RS field at cell centre
C--    o  DUMMY_IN_STEPPING_UV_XYZ_RL  3-D _RL C-grid vector pair
C--    o  DUMMY_IN_STEPPING_UV_XY_RS   2-D _RS C-grid vector pair
C--    o  DUMMY_FOR_ETAN_TAP           free-surface height (own counter)
C--    o  AUTODIFF_INADMODE_SET_TAP    enter adjoint mode
C--    o  AUTODIFF_INADMODE_UNSET_TAP  leave adjoint mode
C--
C--   The option headers below give the routines their package switches
C--   and AD_CONFIG.h the ALLOW_ADJOINT_RUN / ALLOW_TANGENTLINEAR_RUN
C--   guards; CPP_EEOPTIONS.h is included at the top, as in the tree's file.

#include "TAPENADE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_TAP_PREP
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_TAP_PREP(
     I                    myTime, myIter, myThid,
     O                    doDump, doExch )
C     !DESCRIPTION:
C     Common preamble of the DUMMY_IN_STEPPING_*_B routines: decide
C     whether this reverse-sweep step writes ADJ* output (doDump) and
C     whether the tile halos of the adjoint field are folded into the
C     interior first (doExch), exactly as ADDUMMY_IN_STEPPING does, and
C     advance the record counter dumpAdRecMn once per dumped iteration,
C     however many field hooks are called for it.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_P2SHARE.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */
C     !INPUT/OUTPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      LOGICAL doDump
      LOGICAL doExch
#ifdef ALLOW_AUTODIFF_MONITOR
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
C     !LOCAL VARIABLES:
C     lastDumpIter :: iteration whose dump last advanced dumpAdRecMn
      INTEGER lastDumpIter
      SAVE    lastDumpIter
      DATA    lastDumpIter / -1 /
CEOP

      doDump = DIFFERENT_MULTIPLE( adjDumpFreq, myTime, deltaTClock )
      doExch = doDump
# ifdef ALLOW_DIAGNOSTICS
      doExch = doDump .OR. useDiag4AdjOutp
# endif
      doExch = doExch .AND. ( dumpAdVarExch.EQ.1 )

C     Record number in file (used only if dumpAdByRec): one increment per
C     dumped iteration, taken by the first field hook of that iteration.
      IF ( doDump .AND. myIter.NE.lastDumpIter ) THEN
        dumpAdRecMn  = dumpAdRecMn + 1
        lastDumpIter = myIter
      ENDIF
#else /* ALLOW_AUTODIFF_MONITOR */
      doDump = .FALSE.
      doExch = .FALSE.
#endif /* ALLOW_AUTODIFF_MONITOR */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_TAP_TLM
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_TAP_TLM(
     I                    dumpName, myTime, myIter, myThid,
     O                    doDump, tlmName, suff )
C     !DESCRIPTION:
C     Common preamble of the tangent-linear (_D) routines: decide whether
C     this step writes G_J* output, and derive the file name and suffix
C     from the adjoint dump name as G_DUMMY_IN_STEPPING does
C     ('ADJtheta.' becomes 'G_Jtheta.').
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff
#ifdef ALLOW_AUTODIFF_MONITOR
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
CEOP

      doDump = DIFFERENT_MULTIPLE( adjDumpFreq, myTime, deltaTClock )
      tlmName = 'G_J'//dumpName(4:)
      IF ( rwSuffixType.EQ.0 ) THEN
        WRITE(suff,'(I10.10)') myIter
      ELSE
        CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
      ENDIF
#else /* ALLOW_AUTODIFF_MONITOR */
      doDump  = .FALSE.
      tlmName = ' '
      suff    = ' '
#endif /* ALLOW_AUTODIFF_MONITOR */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_XYZ_RL
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_XYZ_RL(
     I                    fld, diagName, dumpName,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION:
C     Output hook for a 3-D _RL cell-centred field: a no-op in the forward
C     model. Its adjoint (_B) folds the tile halos of fldb and writes it
C     as the ADJ* file and diagnostic named by dumpName and diagName; its
C     tangent (_D) writes fldd as the G_J* file.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
C     fld      :: the field whose derivative is written
C     diagName :: name of the adjoint diagnostic (8 characters)
C     dumpName :: prefix of the adjoint output file
      _RL fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XYZ_RL_B(
     I                    fld, fldb, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
#endif
      _RL fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR
      _RS dumRS(1)
      LOGICAL doDump, doExch

      CALL DUMMY_IN_STEPPING_TAP_PREP( myTime, myIter, myThid,
     O                                 doDump, doExch )
      IF ( doExch ) CALL ADEXCH_3D_RL( fldb, Nr, myThid )
      IF ( doDump )
     &  CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      CALL DUMP_ADJ_XYZ( dumRS, fldb, diagName, dumpName, 12,
     &                   doDump, dumpAdRecMn, myTime, myIter, myThid )
      IF ( doDump )
     &  CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XYZ_RL_D(
     I                    fld, fldd, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      _RL fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff

      CALL DUMMY_IN_STEPPING_TAP_TLM( dumpName, myTime, myIter, myThid,
     O                                doDump, tlmName, suff )
      IF ( doDump ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
        CALL WRITE_FLD_XYZ_RL( tlmName, suff, fldd, myIter, myThid )
        CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XYZ_RL_FWD(
     I                    fld, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XYZ_RL_BWD(
     I                    fld, fldb, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL fldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL DUMMY_IN_STEPPING_XYZ_RL_B( fld, fldb, diagName, dumpName,
     &                                 myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_XY_RS
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_XY_RS(
     I                    fld, diagName, dumpName,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION:
C     Output hook for a 2-D _RS cell-centred field (surface forcing):
C     a no-op in the forward model, see DUMMY_IN_STEPPING_XYZ_RL.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RS fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XY_RS_B(
     I                    fld, fldb, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
#endif
      _RS fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR
      _RL dumRL(1)
      LOGICAL doDump, doExch

      CALL DUMMY_IN_STEPPING_TAP_PREP( myTime, myIter, myThid,
     O                                 doDump, doExch )
#ifndef AUTODIFF_EXCLUDE_ADEXCH_RS
      IF ( doExch ) CALL ADEXCH_XY_RS( fldb, myThid )
#endif
      IF ( doDump )
     &  CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      CALL DUMP_ADJ_XY( fldb, dumRL, diagName, dumpName, 11,
     &                  doDump, dumpAdRecMn, myTime, myIter, myThid )
      IF ( doDump )
     &  CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XY_RS_D(
     I                    fld, fldd, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      _RS fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff

      CALL DUMMY_IN_STEPPING_TAP_TLM( dumpName, myTime, myIter, myThid,
     O                                doDump, tlmName, suff )
      IF ( doDump ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
        CALL WRITE_FLD_XY_RS( tlmName, suff, fldd, myIter, myThid )
        CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XY_RS_FWD(
     I                    fld, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RS fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_XY_RS_BWD(
     I                    fld, fldb, diagName, dumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RS fld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL DUMMY_IN_STEPPING_XY_RS_B( fld, fldb, diagName, dumpName,
     &                                myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_UV_XYZ_RL
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_UV_XYZ_RL(
     I                    uFld, vFld,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION:
C     Output hook for a 3-D _RL C-grid vector pair (uVel, vVel): a no-op
C     in the forward model, see DUMMY_IN_STEPPING_XYZ_RL. The pair is
C     carried together because its halo fold (ADEXCH_UV_3D_RL) needs both
C     components.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XYZ_RL_B(
     I                    uFld, uFldb, vFld, vFldb,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
#endif
      _RL uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR
      _RS dumRS(1)
      LOGICAL doDump, doExch

      CALL DUMMY_IN_STEPPING_TAP_PREP( myTime, myIter, myThid,
     O                                 doDump, doExch )
      IF ( doExch )
     &  CALL ADEXCH_UV_3D_RL( uFldb, vFldb, .TRUE., Nr, myThid )
      IF ( doDump )
     &  CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      CALL DUMP_ADJ_XYZ_UV(
     &     dumRS, uFldb, uDiagName, uDumpName,
     &     dumRS, vFldb, vDiagName, vDumpName,
     &     34, doDump, dumpAdRecMn, myTime, myIter, myThid )
      IF ( doDump )
     &  CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XYZ_RL_D(
     I                    uFld, uFldd, vFld, vFldd,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      _RL uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uFldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff

      CALL DUMMY_IN_STEPPING_TAP_TLM( uDumpName, myTime, myIter, myThid,
     O                                doDump, tlmName, suff )
      IF ( doDump ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
        CALL WRITE_FLD_XYZ_RL( tlmName, suff, uFldd, myIter, myThid )
        CALL DUMMY_IN_STEPPING_TAP_TLM( vDumpName,
     I                                  myTime, myIter, myThid,
     O                                  doDump, tlmName, suff )
        CALL WRITE_FLD_XYZ_RL( tlmName, suff, vFldd, myIter, myThid )
        CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XYZ_RL_FWD(
     I                    uFld, vFld,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XYZ_RL_BWD(
     I                    uFld, uFldb, vFld, vFldb,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL DUMMY_IN_STEPPING_UV_XYZ_RL_B( uFld, uFldb, vFld, vFldb,
     &     uDiagName, uDumpName, vDiagName, vDumpName,
     &     myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_UV_XY_RS
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_UV_XY_RS(
     I                    uFld, vFld,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION:
C     Output hook for a 2-D _RS C-grid vector pair (wind stress): a no-op
C     in the forward model, see DUMMY_IN_STEPPING_UV_XYZ_RL.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RS uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XY_RS_B(
     I                    uFld, uFldb, vFld, vFldb,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
#endif
      _RS uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS uFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR
      _RL dumRL(1)
      LOGICAL doDump, doExch

      CALL DUMMY_IN_STEPPING_TAP_PREP( myTime, myIter, myThid,
     O                                 doDump, doExch )
#ifndef AUTODIFF_EXCLUDE_ADEXCH_RS
      IF ( doExch )
     &  CALL ADEXCH_UV_XY_RS( uFldb, vFldb, .TRUE., myThid )
#endif
      IF ( doDump )
     &  CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      CALL DUMP_ADJ_XY_UV(
     &     uFldb, dumRL, uDiagName, uDumpName,
     &     vFldb, dumRL, vDiagName, vDumpName,
     &     33, doDump, dumpAdRecMn, myTime, myIter, myThid )
      IF ( doDump )
     &  CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XY_RS_D(
     I                    uFld, uFldd, vFld, vFldd,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      _RS uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS uFldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFldd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff

      CALL DUMMY_IN_STEPPING_TAP_TLM( uDumpName, myTime, myIter, myThid,
     O                                doDump, tlmName, suff )
      IF ( doDump ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
        CALL WRITE_FLD_XY_RS( tlmName, suff, uFldd, myIter, myThid )
        CALL DUMMY_IN_STEPPING_TAP_TLM( vDumpName,
     I                                  myTime, myIter, myThid,
     O                                  doDump, tlmName, suff )
        CALL WRITE_FLD_XY_RS( tlmName, suff, vFldd, myIter, myThid )
        CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XY_RS_FWD(
     I                    uFld, vFld,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RS uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE DUMMY_IN_STEPPING_UV_XY_RS_BWD(
     I                    uFld, uFldb, vFld, vFldb,
     I                    uDiagName, uDumpName, vDiagName, vDumpName,
     I                    myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RS uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS uFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS vFldb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(8) uDiagName, vDiagName
      CHARACTER*(*) uDumpName, vDumpName
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL DUMMY_IN_STEPPING_UV_XY_RS_B( uFld, uFldb, vFld, vFldb,
     &     uDiagName, uDumpName, vDiagName, vDumpName,
     &     myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_FOR_ETAN_TAP
C     !INTERFACE:
      SUBROUTINE DUMMY_FOR_ETAN_TAP( etaN, myTime, myIter, myThid )
C     !DESCRIPTION:
C     Tapenade counterpart of the DUMMY_FOR_ETAN hook, called beside it
C     from INTEGR_CONTINUITY: a no-op in the forward model whose adjoint
C     (_B) writes the adjoint of the free-surface height as ADJetan,
C     exactly as ADDUMMY_FOR_ETAN does (no halo fold, own record counter
C     dumpAdRecEt), and whose tangent (_D) writes G_Jetan.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE DUMMY_FOR_ETAN_TAP_B( etaN, etaNb,
     I                                 myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      _RS dumRS(1)
      LOGICAL doDump

      doDump = DIFFERENT_MULTIPLE( adjDumpFreq, myTime, deltaTClock )

      IF ( doDump ) THEN
C       increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecEt = dumpAdRecEt + 1
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

      CALL DUMP_ADJ_XY( dumRS, etaNb, 'ADJetan ', 'ADJetan.', 12,
     &                  doDump, dumpAdRecEt, myTime, myIter, myThid )

      IF ( doDump ) THEN
# ifdef ALLOW_MNC
        IF ( useMNC .AND. autodiff_mnc ) THEN
          CALL MNC_CW_SET_UDIM('adstate', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adstate',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
          CALL MNC_CW_I_W_S('I','adstate',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adstate',0,0,'model_time',myTime,
     &         myThid)
          CALL MNC_CW_RL_W('D','adstate',0,0,'adEta', etaNb, myThid)
        ENDIF
# endif /* ALLOW_MNC */
        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_FOR_ETAN_TAP_D( etaN, etaNd,
     I                                 myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR
      LOGICAL doDump
      CHARACTER*(MAX_LEN_FNAM) tlmName
      CHARACTER*(10) suff

      CALL DUMMY_IN_STEPPING_TAP_TLM( 'ADJetan.',
     I                                myTime, myIter, myThid,
     O                                doDump, tlmName, suff )
      IF ( doDump ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
        CALL WRITE_FLD_XY_RL( tlmName, suff, etaNd, myIter, myThid )
        CALL TIMER_STOP ('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */
      RETURN
      END

      SUBROUTINE DUMMY_FOR_ETAN_TAP_FWD( etaN, myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE DUMMY_FOR_ETAN_TAP_BWD( etaN, etaNb,
     I                                   myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL DUMMY_FOR_ETAN_TAP_B( etaN, etaNb, myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AUTODIFF_INADMODE_SET_TAP
C     !INTERFACE:
      SUBROUTINE AUTODIFF_INADMODE_SET_TAP( etaN,
     I                                      myTime, myIter, myThid )
C     !DESCRIPTION:
C     Tapenade counterpart of AUTODIFF_INADMODE_SET, called beside it at
C     the end of FORWARD_STEP: a no-op in the forward model whose adjoint
C     (_B) therefore runs at the START of each backward step and calls
C     ADAUTODIFF_INADMODE_SET, the TAF-named routine that switches the
C     inAd* parameters on (and the adjoint diagnostics). etaN is only the
C     active argument that makes Tapenade generate the call; its adjoint
C     is left untouched.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_SET_TAP_B( etaN, etaNb,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL ADAUTODIFF_INADMODE_SET( myTime, myIter, myThid )
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_SET_TAP_D( etaN, etaNd,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_SET_TAP_FWD( etaN,
     I                                      myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_SET_TAP_BWD( etaN, etaNb,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL AUTODIFF_INADMODE_SET_TAP_B( etaN, etaNb,
     &                                  myTime, myIter, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AUTODIFF_INADMODE_UNSET_TAP
C     !INTERFACE:
      SUBROUTINE AUTODIFF_INADMODE_UNSET_TAP( etaN,
     I                                        myTime, myIter, myThid )
C     !DESCRIPTION:
C     Tapenade counterpart of AUTODIFF_INADMODE_UNSET, called beside it
C     at the start of FORWARD_STEP: a no-op in the forward model whose
C     adjoint (_B) therefore runs at the END of each backward step and
C     calls ADAUTODIFF_INADMODE_UNSET, restoring the forward-mode
C     parameters before the next re-forward. See
C     AUTODIFF_INADMODE_SET_TAP.
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_UNSET_TAP_B( etaN, etaNb,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL ADAUTODIFF_INADMODE_UNSET( myTime, myIter, myThid )
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_UNSET_TAP_D( etaN, etaNd,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_UNSET_TAP_FWD( etaN,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      RETURN
      END

      SUBROUTINE AUTODIFF_INADMODE_UNSET_TAP_BWD( etaN, etaNb,
     I                                        myTime, myIter, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
      _RL etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaNb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CALL AUTODIFF_INADMODE_UNSET_TAP_B( etaN, etaNb,
     &                                    myTime, myIter, myThid )
      RETURN
      END
