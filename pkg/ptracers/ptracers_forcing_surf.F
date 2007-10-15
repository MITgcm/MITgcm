C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_forcing_surf.F,v 1.6 2007/10/15 15:28:58 jmc Exp $
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
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
c #include "DYNVARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

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
C  iTrc                 :: tracer index
C  ks                   :: surface level index
      INTEGER i, j
      INTEGER iTrc, ks
CEOP

      IF ( usingPCoords ) THEN
        ks = Nr
      ELSE
        ks = 1
      ENDIF

C Example of how to add forcing at the surface
      DO iTrc=1,PTRACERS_numInUse
          DO j = jMin, jMax
           DO i = iMin, iMax
             surfaceForcingPtr(i,j,bi,bj,iTrc) =
     &               0. _d 0
c    &               surfaceForcingS(i,j,bi,bj)
           ENDDO
          ENDDO
      ENDDO

#ifdef EXACT_CONSERV
      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN

       DO iTrc=1,PTRACERS_numInUse

c-  NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes
c   the water column height ; temp., salt, (tracer) flux associated
c   with this input/output of water is added here to the surface tendency.
c
         IF (PtRACERS_EvPrRn(iTrc).NE.UNSET_RL) THEN
          DO j = jMin, jMax
           DO i = iMin, iMax
             surfaceForcingPtr(i,j,bi,bj,iTrc) =
     &          surfaceForcingPtr(i,j,bi,bj,iTrc)
     &        + PmEpR(i,j,bi,bj)
     &          *( PTRACERS_EvPrRn(iTrc) - pTracer(i,j,ks,bi,bj,iTrc) )
     &          *mass2rUnit
           ENDDO
          ENDDO
         ENDIF

       ENDDO
      ENDIF
#endif /* EXACT_CONSERV */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
