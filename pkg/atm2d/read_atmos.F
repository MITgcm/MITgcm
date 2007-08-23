#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE READ_ATMOS(inMonth,myThid )
C     *==========================================================*
C     | o Takes atmos data on atmos grid, converts to ocean      |
C     |   model units, and combines the polar cap atmos cell     |
C     |   with its neighbor to the north or south.               |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmos/Ocean/Seaice Interface Variables ===
#include "ATMSIZE.h"
#include "AGRID.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATM2D_VARS.h"

      _RL secDay1000
      PARAMETER (secDay1000= 86400000.D0)

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     inMonth - current month (or forcing period)
C     myThid - Thread no. that called this routine.
      INTEGER inMonth
      INTEGER myThid

C     LOCAL VARIABLES:
      _RL a1,a2
      INTEGER j_atm

C Keep track of (raw) atmos variables for diagnostics
      DO j_atm=1,jm0
        sum_tauu_ta(j_atm,inMonth)= sum_tauu_ta(j_atm,inMonth) +
     &                           tauu(j_atm)*dtatmo
        sum_tauv_ta(j_atm,inMonth)= sum_tauv_ta(j_atm,inMonth) + 
     &                           tauv(j_atm)*dtatmo
        sum_wsocean_ta(j_atm,inMonth)= sum_wsocean_ta(j_atm,inMonth) + 
     &                           wsocean(j_atm)*dtatmo
        sum_ps4ocean_ta(j_atm,inMonth)= sum_ps4ocean_ta(j_atm,inMonth) + 
     &                           ps4ocean(j_atm)*dtatmo
      ENDDO

C
C put atmospheric data onto local arrays and convert units for ocean model
C
      DO j_atm=1,jm0

        atm_tauu(j_atm) = tauu(j_atm)
        atm_tauv(j_atm) = tauv(j_atm)
        atm_tair(j_atm) = tempr(j_atm)
        atm_precip(j_atm)    = -precip(j_atm)/secDay1000
        atm_runoff(j_atm)    = -arunoff(j_atm)/secDay1000
        atm_evap_ice(j_atm)  = -evai(j_atm)/secDay1000
        atm_evap_ocn(j_atm)  = -evao(j_atm)/secDay1000
        atm_qnet_ice(j_atm)  = -hfluxi(j_atm)
        atm_qnet_ocn(j_atm)  = -hfluxo(j_atm)
        atm_dFdt_ice(j_atm)  = -dhfidtg(j_atm)
        atm_dFdt_ocn(j_atm)  = -dhfodtg(j_atm)
C       Ice feels evap from model, no change with temperature
        atm_dLdt_ice(j_atm)  = 0. _d 0  ! -devidtg(j_atm)/secDay1000
        atm_dLdt_ocn(j_atm)  = -devodtg(j_atm)/secDay1000
        atm_dFdt_iceq(j_atm) = -dhfidtgeq(j_atm)
        atm_dFdt_ocnq(j_atm) = -dhfodtgeq(j_atm)
        atm_dLdt_iceq(j_atm) = 0. _d 0  ! -devidtgeq(j_atm)/secDay1000
        atm_dLdt_ocnq(j_atm) = -devodtgeq(j_atm)/secDay1000
        atm_solarinc(j_atm)  = solarinc_ice(j_atm)
        atm_solar_ocn(j_atm) = solarnet_ocean(j_atm)
        atm_solar_ice(j_atm) = solarnet_ice(j_atm)
        atm_windspeed(j_atm) = wsocean(j_atm)
        atm_slp(j_atm) = ps4ocean(j_atm)*1013.25/984.0 - 1.01325 _d 5
        atm_pco2(j_atm) = co24ocean(j_atm)

      ENDDO

      IF (cflan(2).NE.1. _d 0) 
     &          CALL COMBINE_ENDS(endwgt1,endwgt2,1,2,rsumwgt)
      IF (cflan(jm0-1).NE.1. _d 0) 
     &          CALL COMBINE_ENDS(endwgt1,endwgt2,jm0,jm0-1,rsumwgt)

      RETURN
      END

C--------------------------------------------------------------------------
#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"


      SUBROUTINE COMBINE_ENDS(a1,a2,ind1,ind2,rsuma)
C     *==========================================================*
C     | Subroutine used to combine the atmos model data points at|
C     | the poles with their neighboring value, area weighted.   |
C     | The calcuated new value is overwritten into ind2.        |
C     *==========================================================*
      IMPLICIT NONE


C     === Global Atmos/Ocean/Seaice Interface Variables ===
#include "ATMSIZE.h"
#include "AGRID.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     a1 - weighting of first index
C     a2 - weighting of second index
C     ind1 - first index into atmos data array
C     ind2 - first index into atmos data array
C     rsuma - recip of sum of ind1+ind2
      _RL     a1
      _RL     a2
      INTEGER ind1
      INTEGER ind2
      _RL  rsuma

C     LOCAL VARIABLES:

C      atm_tauu(ind2)= (a1*atm_tauu(ind1) + a2*atm_tauu(ind2))*rsuma
C      atm_tauv(ind2)= (a1*atm_tauv(ind1) + a2*atm_tauv(ind2))*rsuma
C Tau variables not combined - zero at atm pole point

      atm_tair(ind2)= (a1*atm_tair(ind1) + a2*atm_tair(ind2))*rsuma
      atm_precip(ind2)= (a1*atm_precip(ind1) + 
     &                   a2*atm_precip(ind2))*rsuma
      atm_runoff(ind2)= atm_runoff(ind1)+ atm_runoff(ind2)
      atm_evap_ice(ind2)= (a1*atm_evap_ice(ind1) + 
     &                     a2*atm_evap_ice(ind2))*rsuma
      atm_evap_ocn(ind2)= (a1*atm_evap_ocn(ind1) +
     &                     a2*atm_evap_ocn(ind2))*rsuma
      atm_qnet_ice(ind2)= (a1*atm_qnet_ice(ind1)+ 
     &                     a2*atm_qnet_ice(ind2))*rsuma
      atm_qnet_ocn(ind2)= (a1*atm_qnet_ocn(ind1) +
     &                     a2*atm_qnet_ocn(ind2))*rsuma
      atm_dFdt_ice(ind2)= (a1*atm_dFdt_ice(ind1)+ 
     &                     a2*atm_dFdt_ice(ind2))*rsuma
      atm_dFdt_ocn(ind2)= (a1*atm_dFdt_ocn(ind1)+ 
     &                     a2*atm_dFdt_ocn(ind2))*rsuma
      atm_dLdt_ice(ind2)= (a1*atm_dLdt_ice(ind1)+ 
     &                     a2*atm_dLdt_ice(ind2))*rsuma
      atm_dLdt_ocn(ind2)= (a1*atm_dLdt_ocn(ind1)+ 
     &                     a2*atm_dLdt_ocn(ind2))*rsuma
      atm_dFdt_iceq(ind2)= (a1*atm_dFdt_iceq(ind1)+ 
     &                      a2*atm_dFdt_iceq(ind2))*rsuma
      atm_dFdt_ocnq(ind2)= (a1*atm_dFdt_ocnq(ind1)+ 
     &                      a2*atm_dFdt_ocnq(ind2))*rsuma
      atm_dLdt_iceq(ind2)= (a1*atm_dLdt_iceq(ind1)+ 
     &                      a2*atm_dLdt_iceq(ind2))*rsuma
      atm_dLdt_ocnq(ind2)= (a1*atm_dLdt_ocnq(ind1)+ 
     &                      a2*atm_dLdt_ocnq(ind2))*rsuma
      atm_solarinc(ind2)= (a1*atm_solarinc(ind1) + 
     &                     a2*atm_solarinc(ind2))*rsuma
      atm_solar_ocn(ind2)= (a1*atm_solar_ocn(ind1)+
     &                       a2*atm_solar_ocn(ind2))*rsuma
      atm_solar_ice(ind2)= (a1*atm_solar_ice(ind1)+
     &                     a2*atm_solar_ice(ind2))*rsuma
      atm_windspeed(ind2)= (a1*atm_windspeed(ind1) + 
     &                      a2*atm_windspeed(ind2))*rsuma
      atm_slp(ind2)= (a1*atm_slp(ind1) + a2*atm_slp(ind2))*rsuma
      atm_pco2(ind2)= (a1*atm_pco2(ind1)+a2*atm_pco2(ind2))*rsuma
   
      RETURN
      END





