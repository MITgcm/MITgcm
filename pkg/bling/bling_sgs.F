#include "BLING_OPTIONS.h"

CBOP
      SUBROUTINE BLING_SGS(
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_sgs
C     | o Placeholder for sub-grid scale sediment stuff
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===

#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     === Output ===

#ifdef ALLOW_BLING
C     === Local variables ===
C     i,j,k         :: loop indices
c     INTEGER i,j,k

CEOP

c ---------------------------------------------------------------------
c  Initialize output and diagnostics

c---------------------------------------------------------------------
c SUBGRIDSCALE SEDIMENT FLUXES
c
c The subgridscale sediment produces remineralization fluxes from sinking particles
c throughout the water column, and leads to 3-dimensional fields for benthic
c iron fluxes and benthic denitrification.
c Note that the remineralization of particle flux intercepting the subgridscale
c sediment is implicit in the reminp terms.

#ifdef use_sgs_sed
c!!
#endif

c ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN
      END
