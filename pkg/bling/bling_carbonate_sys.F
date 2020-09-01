#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_CARBONATE_SYS(
     I           PTR_DIC, PTR_ALK, PTR_PO4,
#ifdef USE_SIBLING
     I           PTR_SI,
#endif
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_carbonate_sys
C     | o Calculate carbonate fluxes
C     |   Also update pH (3d field)
C     =================================================================

      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C     PTR_DIC              :: dissolved inorganic carbon
C     PTR_ALK              :: alkalinity
C     PTR_PO4              :: phosphate
C     myThid               :: thread Id. number
C     myIter               :: current timestep
C     myTime               :: current time
      INTEGER myThid
      INTEGER myIter
      _RL  myTime
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL  PTR_SI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER imin, imax, jmin, jmax, bi, bj

#ifdef ALLOW_PTRACERS

C     == Local variables ==
C     i,j,k             :: loop indices
C     carbonate         :: local value of calcium carbonate
C     calcium           :: local value of Ca
C     diclocal          :: local value of DIC
C     alklocal          :: local value of ALK
C     pCO2local         :: local value of pCO2
C     pHlocal           :: local value of pH
C     CO3iter           :: iterations counter for CO3 ion calculation
C     CO3iterMax        :: total number of iterations
C     silicaDEEP        :: subsurface silica concentration
       INTEGER i,j,k
       _RL carbonate
       _RL calcium
       _RL po4local
       _RL diclocal
       _RL alklocal
       _RL pCO2local
       _RL pHlocal
       _RL silicaDEEP

       _RL ttmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       _RL stmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

c      INTEGER CO3iter
c      INTEGER CO3iterMax
CEOP

C  Since pH is now a 3D field and is solved for at every time step
C  few iterations are needed
c      CO3iterMax = 1

C determine carbonate ion concentration through full domain
C determine calcite saturation state

C$TAF LOOP = parallel
       DO k=1,Nr

          DO j=jMin,jMax
            DO i=iMin,iMax
             ttmp(i,j) = theta(i,j,k,bi,bj)
             stmp(i,j) = salt(i,j,k,bi,bj)
            ENDDO
          ENDDO

#ifdef CARBONCHEM_SOLVESAPHE
cav  I'm using ttmp and stmp instead of surftemp and surfsalt like
cav  Jonathan / they are not surf when k>1
cav  Does new carbon_chem in DICpkg need theta (3d) or ttmp (2d)???

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL(
     &   'DIC_COEFFS_DEEP',myThid)
#endif
C Calculate carbon coefficients
        CALL DIC_COEFFS_SURF(
     I                       ttmp,stmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)

C Now correct the coefficients for pressure dependence
        CALL DIC_COEFFS_DEEP(
     I                       ttmp,stmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,
     I                       k,myThid)

#else

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL(
     &   'CARBON_COEFFS_PRESSURE_DEP',myThid)
#endif

C  Get coefficients for carbonate calculations
          CALL CARBON_COEFFS_PRESSURE_DEP(
     I                       ttmp, stmp,
     I                       bi, bj, imin, imax, jmin, jmax,
     I                       k, myThid)
#endif

C--------------------------------------------------

C$TAF LOOP = parallel
          DO j=jMin,jMax
C$TAF LOOP = parallel
           DO i=iMin,iMax

             IF ( hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
C$TAF init dic_caco3 = static, 2

#ifdef CARBONCHEM_SOLVESAPHE
             calcium = cat(i,j,bi,bj)
#else
C  Estimate calcium concentration from salinity
             calcium = 1.028 _d -2*salt(i,j,k,bi,bj)/35. _d 0
#endif

             po4local = PTR_PO4(i,j,k)
             diclocal = PTR_DIC(i,j,k)
             alklocal = PTR_ALK(i,j,k)
             pHlocal  = pH(i,j,k,bi,bj)

C  Assume constant deep silica value
C  30 micromol = 0.03 mol m-3
C  unless SIBLING is used
#ifdef USE_SIBLING
             silicaDEEP = PTR_SI(i,j,k)
#else
             silicaDEEP = 0.03 _d 0
#endif

#ifdef CARBONCHEM_SOLVESAPHE
      IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to calculate pH and pCO2
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('AHINI_FOR_AT',myThid)
#endif
CAV since we carry pH, no need for an initial guess
C call AHINI_FOR_AT to get better initial guess of pH
c               CALL AHINI_FOR_AT(alklocal*permil,
c     I           diclocal*permil,
c     I           bt(i,j,bi,bj),
c     O           pHlocal,
c     I           i,j,k,bi,bj,myIter,myThid )
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('CALC_PCO2_SOLVESAPHE',myThid)
#endif
            CALL CALC_PCO2_SOLVESAPHE(
     I          theta(i,j,k,bi,bj),salt(i,j,k,bi,bj),
     I          diclocal, po4local,
     I          silicaDEEP,alklocal,
     U          pHlocal,pCO2local,carbonate,
     I          i,j,k,bi,bj,myIter,myThid )
      ELSE
C Use the original Follows et al. (2006) solver
#endif /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('CALC_PCO2_APPROX',myThid)
#endif

C  Evaluate carbonate (CO3) ions concentration
C  iteratively

c             DO CO3iter = 1, CO3iterMax

C--------------------------------------------------

               CALL CALC_PCO2_APPROX(
     I          ttmp(i,j),stmp(i,j),
     I          diclocal, po4local,
     I          silicaDEEP,alklocal,
     I          ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I          ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I          aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I          aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I          ak0(i,j,bi,bj),fugf(i,j,bi,bj),ff(i,j,bi,bj),
     I          bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U          pHlocal,pCO2local,carbonate,
     I          i,j,k,bi,bj,myIter,myThid )
c             ENDDO

#ifdef CARBONCHEM_SOLVESAPHE
      ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */

              pH(i,j,k,bi,bj) = pHlocal

C  Calculate calcium carbonate (calcite and aragonite)
C  saturation state
             omegaC(i,j,k,bi,bj) = calcium * carbonate /
     &                          Ksp_TP_Calc(i,j,bi,bj)
             omegaAr(i,j,k,bi,bj) = calcium * carbonate /
     &                          Ksp_TP_Arag(i,j,bi,bj)

           else

             pH(i,j,k,bi,bj) = 8. _d 0
             omegaC(i,j,k,bi,bj)  = 0. _d 0
             omegaAr(i,j,k,bi,bj) = 0. _d 0

           endif

         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_PTRACERS */
       RETURN
       END
