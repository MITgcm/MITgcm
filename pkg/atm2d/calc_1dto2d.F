#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE CALC_1DTO2D( myThid )
C     *==========================================================*
C     | - Takes 1D atmos data, regrid to 2D ocean grid. This     |
C     |   involves totalling runoff into bands and redistributing|
C     |   and using derivates dF/dT and dH/dT to compute         |
C     |   local variations in evap and heat flux.                |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

C     === Global SeaIce Variables ===
#include "THSICE_VARS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j           ! loop counters across ocean grid
      INTEGER ib,ibj1,ibj2  ! runoff band variables
      _RL run_b(sNy)        ! total runoff in a band

      CALL INIT_2DFLD(myThid)

C     Accumulate runoff into bands (runoff bands are on the ocean grid)
      DO ib=1,numBands
        ibj1=1
        IF (ib.GT.1) ibj1= rband(ib-1)+1
        ibj2=sNy
        IF (ib.LT.numBands) ibj2= rband(ib)
        run_b(ib)=0. _d 0
        DO j=ibj1,ibj2
          run_b(ib)=run_b(ib) + 
     &              atm_runoff(atm_oc_ind(j))*atm_oc_frac1(j) +
     &              atm_runoff(atm_oc_ind(j)+1)*atm_oc_frac2(j)
        ENDDO
      ENDDO
     
      DO j=1,sNy
        DO i=1,sNx

          IF (maskC(i,j,1,1,1).EQ.1.) THEN
         
            runoff_2D(i,j) = run_b(runIndex(j)) * 
     &                       runoffVal(i,j)/rA(i,j,1,1)
  
            CALL CALC_WGHT2D(i,j,atm_oc_ind(j),atm_oc_wgt(j))

            IF (atm_oc_wgt(j).LT.1. _d 0)
     &          CALL CALC_WGHT2D(i, j, atm_oc_ind(j)+1,
     &                           1. _d 0-atm_oc_wgt(j))

C  Tabulate following diagnostic fluxes from atmos model only
            qnet_atm(i,j)= qnet_atm(i,j) +
     &          qneti_2D(i,j)*dtatmo*iceMask(i,j,1,1) +
     &          qneto_2D(i,j)*dtatmo*(1. _d 0-iceMask(i,j,1,1))
            evap_atm(i,j)= evap_atm(i,j) +
     &          evapi_2D(i,j)*dtatmo*iceMask(i,j,1,1) +
     &          evapo_2D(i,j)*dtatmo*(1. _d 0-iceMask(i,j,1,1))
            precip_atm(i,j)= precip_atm(i,j) +
     &           precipi_2D(i,j)*dtatmo*iceMask(i,j,1,1) +
     &           precipo_2D(i,j)*dtatmo*(1. _d 0-iceMask(i,j,1,1))
            runoff_atm(i,j)= runoff_atm(i,j) +
     &           runoff_2D(i,j)*dtatmo
         ENDIF

        ENDDO
      ENDDO
 
      RETURN
      END


C--------------------------------------------------------------------------

#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE CALC_WGHT2D( i, j, ind, wgt)
C     *==========================================================*
C     | Use atmos grid cell 1D value and weight to convert to 2D.|
C     | Variations from zonal mean computed used derivative dH/dT|
C     | and dF/dT  for heat flux and evap terms.                 |
C     |                                                          |
C     | Fluxes/values over seaice computed only if seaice present|
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Global SeaIce Variables ===
#include "THSICE_VARS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     i,j   - coordinates of point on ocean grid
C     ind   - index into the atmos grid array
C     wght  - weight of this atmos cell for total
      INTEGER    i, j
      INTEGER    ind 
      _RL        wgt

      precipo_2D(i,j)= precipo_2D(i,j) + atm_precip(ind)*wgt
      solarnet_ocn_2D(i,j)=solarnet_ocn_2D(i,j) + atm_solar_ocn(ind)*wgt
      slp_2D(i,j)= slp_2D(i,j) + atm_slp(ind)*wgt
      pCO2_2D(i,j)= pCO2_2D(i,j) + atm_pco2(ind)*wgt
      wspeed_2D(i,j)= wspeed_2D(i,j) + atm_windspeed(ind)*wgt
      fu_2D(i,j)= fu_2D(i,j) + atm_tauu(ind)*wgt
      fv_2D(i,j)= fv_2D(i,j) + atm_tauv(ind)*wgt

      qneto_2D(i,j)= qneto_2D(i,j) + atm_qnet_ocn(ind)*wgt
      evapo_2D(i,j)= evapo_2D(i,j) + atm_evap_ocn(ind)*wgt
      IF (evapo_2D(i,j).GT.0. _d 0) THEN  !convert negative evap. to precip
        precipo_2D(i,j)= precipo_2D(i,j) - evapo_2D(i,j)
        evapo_2D(i,j)=0. _d 0
      ENDIF

      IF (iceMask(i,j,1,1) .GT. 0. _d 0) THEN
        qneti_2D(i,j)= qneti_2D(i,j) + atm_qnet_ice(ind)*wgt
        precipi_2D(i,j)= precipi_2D(i,j) + atm_precip(ind)*wgt
        evapi_2D(i,j)= evapi_2D(i,j) + atm_evap_ice(ind)*wgt
        IF (evapi_2D(i,j).GT.0. _d 0) THEN  !convert negative evap. to precip
          precipi_2D(i,j)= precipi_2D(i,j) - evapi_2D(i,j)
          evapi_2D(i,j)=0. _d 0
        ENDIF
        dFdT_ice_2D(i,j)= dFdT_ice_2D(i,j) + atm_dFdT_ice(ind)*wgt
        Tair_2D(i,j)= Tair_2D(i,j) + atm_Tair(ind)*wgt
        solarinc_2D(i,j)= solarinc_2D(i,j) + atm_solarinc(ind)*wgt
      ENDIF

      IF (useAltDeriv) THEN
        qneto_2D(i,j)= qneto_2D(i,j) + atm_dFdt_ocnq(ind)*
     &             (sstFromOcn(i,j)-ctocn(ind))*wgt
        evapo_2D(i,j)= evapo_2D(i,j) + atm_dLdt_ocnq(ind)*
     &             (sstFromOcn(i,j)-ctocn(ind))*wgt
        IF (iceMask(i,j,1,1) .GT. 0. _d 0) THEN
          qneti_2D(i,j)=qneti_2D(i,j)+atm_dFdt_iceq(ind)*
     &             (Tsrf(i,j,1,1)-ctice(ind))*wgt
          evapi_2D(i,j)=evapi_2D(i,j)+atm_dLdt_iceq(ind)*
     &             (Tsrf(i,j,1,1)-ctice(ind))*wgt
        ENDIF
      ELSE
        qneto_2D(i,j)= qneto_2D(i,j) + atm_dFdt_ocn(ind)*
     &             (sstFromOcn(i,j)-ctocn(ind))*wgt
        evapo_2D(i,j)= evapo_2D(i,j) + atm_dLdt_ocn(ind)*
     &             (sstFromOcn(i,j)-ctocn(ind))*wgt
        IF (iceMask(i,j,1,1) .GT. 0. _d 0) THEN
          qneti_2D(i,j)= qneti_2D(i,j) + atm_dFdt_ice(ind)*
     &             (Tsrf(i,j,1,1)-ctice(ind))*wgt
          evapi_2D(i,j)= evapi_2D(i,j)+atm_dLdt_ice(ind)*
     &             (Tsrf(i,j,1,1)-ctice(ind))*wgt
        ENDIF
      ENDIF


      RETURN
      END

C--------------------------------------------------------------------------

#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE INIT_2DFLD( myThid)
C     *==========================================================*
C     | Zero out the 2D fields; called prior to doing any of the |
C     | 1D->2D calculation.                                      |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j

      DO i=1,sNx
        DO j=1,sNy

          precipo_2D(i,j)= 0. _d 0
          precipi_2D(i,j)= 0. _d 0
          solarnet_ocn_2D(i,j)= 0. _d 0
          slp_2D(i,j)= 0. _d 0
          pCO2_2D(i,j)= 0. _d 0
          wspeed_2D(i,j)= 0. _d 0
          fu_2D(i,j)= 0. _d 0
          fv_2D(i,j)= 0. _d 0
          qneto_2D(i,j)= 0. _d 0
          evapo_2D(i,j)= 0. _d 0
          qneti_2D(i,j)= 0. _d 0
          evapi_2D(i,j)= 0. _d 0
          dFdT_ice_2D(i,j)= 0. _d 0
          Tair_2D(i,j)= 0. _d 0
          solarinc_2D(i,j)= 0. _d 0
          runoff_2D(i,j)= 0. _d 0

        ENDDO
      ENDDO

      RETURN
      END
