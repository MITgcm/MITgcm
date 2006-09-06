#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE SUBTRACT_MEANS( wght0, wght1,
     &                           intime0, intime1, myThid )
C     *==========================================================*
C     | Subtract out (1D) mean values of wind variables          |
c     | (anomaly coupling)                                       |
C     *==========================================================*
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL  wght0
      _RL  wght1
      INTEGER intime0
      INTEGER intime1
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER j_atm

C     Interpolate and subtract 1D mean wind files
      
      DO j_atm=2,jm0-1
        atm_tauu(j_atm)= atm_tauu(j_atm) -
     &       (wght0*atau(j_atm,intime0) + wght1*atau(j_atm,intime1))
        atm_tauv(j_atm)= atm_tauv(j_atm) -
     &       (wght0*atav(j_atm,intime0) + wght1*atav(j_atm,intime1))
        atm_windspeed(j_atm)= atm_windspeed(j_atm) -
     &       (wght0*awind(j_atm,intime0) + wght1*awind(j_atm,intime1))
      ENDDO

      RETURN
      END
