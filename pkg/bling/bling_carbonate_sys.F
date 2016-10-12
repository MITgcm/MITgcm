C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_carbonate_sys.F,v 1.3 2016/10/12 22:34:27 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_CARBONATE_SYS( 
     I           PTR_DIC, PTR_ALK, PTR_PO4,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myIter, myTime, myThid)

C     =================================================================
C     | subroutine bling_carbonate_sys
C     | o Calculate carbonate fluxes
C     |   Also update pH (3d field)
C     =================================================================

      implicit none
      
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
C     CO3ITER           :: iterations counter for CO3 ion calculation
C     CO3ITERmax        :: total number of iterations 
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

       INTEGER CO3ITER
       INTEGER CO3ITERmax
CEOP


C  Assume constant deep silica value
C  30 micromol = 0.03 mol m-3
C  This is temporary until SiBLING is included

C  Since pH is now a 3D field and is solved for at every time step
C  few iterations are needed
       CO3itermax = 1

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

C  Get coefficients for carbonate calculations
        CALL CARBON_COEFFS_PRESSURE_DEP(
     I                       ttmp, stmp,
     I                       bi, bj, imin, imax, jmin, jmax,
     I                       k, myThid)

C--------------------------------------------------

C$TAF LOOP = parallel
           DO j=jMin,jMax
C$TAF LOOP = parallel
           DO i=iMin,iMax

             IF ( hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
C$TAF init dic_caco3 = static, 2


C  Estimate calcium concentration from salinity
             calcium = 1.028 _d -2*salt(i,j,k,bi,bj)/35. _d 0

             po4local = PTR_PO4(i,j,k)
             diclocal = PTR_DIC(i,j,k)
             alklocal = PTR_ALK(i,j,k)
             pHlocal  = pH(i,j,k,bi,bj)
             silicaDEEP = 0.03 _d 0

C  Evaluate carbonate (CO3) ions concentration
C  iteratively

c             DO CO3iter = 1, CO3itermax

C--------------------------------------------------

               CALL CALC_PCO2_APPROX(
     I          ttmp(i,j),stmp(i,j),
     I          diclocal, po4local,
     I          silicaDEEP,alklocal,
     I          ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I          ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I          aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I          aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I          ak0(i,j,bi,bj), fugf(i,j,bi,bj), ff(i,j,bi,bj),
     I          bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U          pHlocal,pCO2local,carbonate,
     I          i,j,k,bi,bj,myIter,myThid )
c             ENDDO

              pH(i,j,k,bi,bj) = pHlocal

C  Calculate calcium carbonate (calcite and aragonite) 
C  saturation state
             omegaC(i,j,k,bi,bj) = calcium * carbonate /
     &                          Ksp_TP_Calc(i,j,bi,bj)
             omegaAr(i,j,k,bi,bj) = calcium * carbonate /
     &                          Ksp_TP_Arag(i,j,bi,bj)

           else

             pH(i,j,k,bi,bj) = 0. _d 0
             omegaC(i,j,k,bi,bj)  = 0. _d 0
             omegaAr(i,j,k,bi,bj) = 0. _d 0

           endif

         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_PTRACERS */
       RETURN
       END
