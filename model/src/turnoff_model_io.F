C $Header: /u/gcmpack/MITgcm/model/src/turnoff_model_io.F,v 1.1 2010/05/09 16:17:45 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TURNOFF_MODEL_IO
C     !INTERFACE:
      SUBROUTINE TURNOFF_MODEL_IO( seqFlag, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TURNOFF_MODEL_IO
C     | o Turn off some of the model output flags
C     *==========================================================*
C     | Used in adjoint simulation (and called after the first
C     |  forward sweep) to avoid writing outputmultiple times (if
C     |  recomputations and/or grdchk) with the same iter number.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
c#ifdef ALLOW_EXF
c# include "EXF_PARAM.h"
c#endif
c#ifdef ALLOW_SEAICE
c# include "SEAICE_PARAMS.h"
c#endif


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     seqFlag :: flag that indicates where this S/R is called from:
C             :: =0 called at the end of S/R COST_FINAL
C             :: =1 called at initialisation when using DIVA
C     myThid  :: My Thread Id number
      INTEGER seqFlag
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER ks
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BARRIER

      _BEGIN_MASTER( myThid )
C--   Set output freq. to zero to avoid re-write of
C     averaged fields in reverse checkpointing loops
      monitorFreq  = 0.
      dumpFreq     = 0.
      taveFreq     = 0.
c     chkPtFreq    = 0.
      pChkPtFreq   = 0.
c     dumpInitAndLast = .FALSE.

#ifdef ALLOW_MNC
      monitor_mnc  = .FALSE.
      snapshot_mnc = .FALSE.
      timeave_mnc  = .FALSE.
#endif

c#ifdef ALLOW_EXF
c     exf_monFreq = 0.
c#endif
c#ifdef ALLOW_SEAICE
c     SEAICE_monFreq  = 0.
c     SEAICE_dumpFreq = 0.
c     SEAICE_taveFreq = 0.
c#endif

C--   Disable SBO output
      useSBO       = .FALSE.

C--   For now, also disable diagnostics output (might need to change
C     this if we want to use diagnostics to output adjoint-variables)
      useDiagnostics = .FALSE.

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
