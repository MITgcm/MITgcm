C $Header: /u/gcmpack/MITgcm/pkg/ebm/Attic/ebm_forcing_surf.F,v 1.1 2004/05/14 21:10:34 heimbach Exp $
C $Name:  $

#include "EBM_OPTIONS.h"
 
CBOP
C     !ROUTINE: EBM_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE EBM_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EBM_FORCING_SURF                          
C     | o Determines forcing terms based on external fields       
C     |   relaxation terms etc.                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_EBM
#include "EBM.h"
#endif
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

#ifdef ALLOW_EBM

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
C     number of surface interface layer
      INTEGER kSurface
CEOP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface        = Nr 
      else
       kSurface        = 1
      endif

C--   Surface Fluxes :

      DO j = jMin, jMax
         DO i = iMin, iMax

c     Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = 
     &      surfaceTendencyU(i,j,bi,bj) + winPert(i,j,bi,bj)
c     Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = 
     &      surfaceTendencyV(i,j,bi,bj)
c     Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = 
     &      -Qnet(i,j,bi,bj)*recip_Cp*horiVertRatio*recip_rhoConst
     &         *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
     &      -lambdaThetaZonRelax*maskC(i,j,kSurface,bi,bj)*
     &         (theta(i,j,kSurface,bi,bj)-ZonalMeanSST(j))

C     Net Salt Flux : 
          surfaceTendencyS(i,j,bi,bj) = 
     &      EmPmR(i,j,bi,bj)*convertFW2Salt*convertEmP2rUnit
     &         *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
     &      +Run(i,j,bi,bj)*scale_runoff
     &         *convertFW2Salt*convertEmP2rUnit
     &         *recip_drF(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
     &      -lambdaSaltClimRelax*maskC(i,j,kSurface,bi,bj)*
     &         (salt(i,j,kSurface,bi,bj)-SSS(i,j,bi,bj))

         ENDDO
       ENDDO

#endif /* ALLOW_EBM */


      RETURN
      END
