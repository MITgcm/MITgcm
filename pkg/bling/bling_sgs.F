C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_sgs.F,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_SGS( 
     I           PTR_NO3, PTR_PO4, PTR_FE, 
     I           PTR_O2, PTR_DON, PTR_DOP,
     O           G_NO3, G_PO4, G_FE, 
     O           G_O2, G_DON, G_DOP, G_CACO3,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myIter, myTime, myThid )

C     =================================================================
C     | subroutine bling_prod
C     | o Nutrient uptake and partitioning between organic pools.
C     | - Phytoplankton biomass-specific growth rate is calculated 
C     |   as a function of light, nutrient limitation, and  
C     |   temperature. 
C     | - Biomass growth xxx
C     | o Organic matter export, remineralization, and recycling.
C     | - Sinking particulate flux and diel migration contribute to 
C     |   export. 
C     | - Denitrification xxx
C     =================================================================

      implicit none
      
C     === Global variables ===
C     P_sm          :: Small phytoplankton biomass
C     P_lg          :: Large phytoplankton biomass
C     P_diaz        :: Diazotroph phytoplankton biomass
C     irr_mem       :: Phyto irradiance memory

#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

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
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_FE        :: iron concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_O2        :: oxygen concentration
      _RL     PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     === Output ===
C     G_xxx         :: Tendency of xxx
      _RL     G_NO3     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_PO4     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_FE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_O2      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DON     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DOP     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_CACO3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
 
#ifdef ALLOW_BLING
C     === Local variables ===
C     i,j,k         :: loop indices

      INTEGER i,j,k        

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
