C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_heatflux.F,v 1.4 2004/12/04 00:13:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_HEATFLUX( bi,bj,
     I                               myTime, 
     I                               myIter, 
     I                               myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_HEATFLUX                            |
C     | o Routine for saving atmos. lower boundary heat flux for |
C     |   export to coupling layer.                              |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
C     == Global variables (main model)
#include "FFIELDS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"


C     == Routine arguments ==
C     bi,bj  - Tile index
C     myThid - Thread number for this instance of the routine
C     myIter - Current timestep number
C     myTime - Current model time
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j - Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate heat flux that will be exported to the coupling layer.
C        Heat flux at lower boundary is sum of the upward long-wave radiation 
C       (variable SLR in COMMON/FLUXES/, com_physvar.h), downward short-wave 
C       radiation (variable SSR in COMMON/FLUXES/, com_physvar.h), downward 
C       sensible heat flux (variable SHF in COMMON/FLUXES/, com_physvar.h) 
C       and latent heat flux due to evaporation (variable EVAP in 
C       COMMON/FLUXES/, com_physvar.h). Index 2 of SHF and EVAP give fluxes 
C       over sea. SLR is the flux that is radiated by the ocean/land. SSR
C       is the incoming short-wave radiation that is absorbed by the ocean/land. 
C       Variable ALHC is the latent heat of evaporation/condensation (see 
C       COMMON/PHYCON/, com_phycon.h).
C        HeatFlux is defined with positive into the atmosphere, units are W/m^2 

       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       HeatFluxTime(bi,bj) = HeatFluxTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          HeatFlux(i,j,bi,bj) = HeatFlux(i,j,bi,bj)
     &                        + Qnet(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
