C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_heatflux.F,v 1.2 2004/04/18 15:38:45 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_HEATFLUX( bi,bj,
     I                               myCurrentTime, 
     I                               myCurrentIter, 
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
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"
C     == AIMPHYS specific global data ==
#include "com_physcon.h"
#include "com_physvar.h"


C     == Routine arguments ==
C     bi,bj         - Tile index
C     myThid        - Thread number for this instance of the routine
C     myCurrentIter - Current timestep number
C     myCurrentTime - Current model time
      INTEGER bi, bj
      _RL     myCurrentTime
      INTEGER myCurrentIter
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K,II - Loop counters
      INTEGER I,J,K,II
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
C       (* deltaTtracer).
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        HeatFluxTime(bi,bj) = HeatFluxTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          II = (sNx)*(J-1)+I
          HeatFlux(I,J,bi,bj) = 
     &    HeatFlux(I,J,bi,bj) + 
     &             ( EVAP(II,2,myThid)*ALHC
     &           +    SHF(II,2,myThid)
     &           +    SLR(II,2,myThid)
     &           -    SSR(II,2,myThid) )*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
