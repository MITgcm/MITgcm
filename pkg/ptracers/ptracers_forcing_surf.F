C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_forcing_surf.F,v 1.2 2004/07/13 16:47:49 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FORCING_SURF

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FORCING_SURF(
     I                            bi, bj, iMin, iMax, jMin, jMax,
     I                            myTime,myIter,myThid )

C !DESCRIPTION:
C     Precomputes surface forcing term for pkg/ptracers.
C     Precomputation is needed because of non-local KPP transport term,
C     routine KPP_TRANSPORT_PTR.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i, j, iTracer
CEOP

C Example of how to add forcing at the surface
       DO iTracer=1,PTRACERS_numInUse
          DO j = jMin, jMax
             DO i = iMin, iMax
                surfaceTendencyPtr(i,j,bi,bj,iTracer) =
     &               surfaceTendencyS(i,j,bi,bj)
             ENDDO
          ENDDO
       ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
