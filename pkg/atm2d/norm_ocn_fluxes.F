#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE NORM_OCN_FLUXES( myThid )
C     *==========================================================*
C     | Converts non-seaice quantities to MITGCM ocn sign/units. |
c     | Seaice quantities put into seaice model sign/units.      |
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
      INTEGER i,j
      _RS nper    ! number of atm steps in a coupled period

      nper= dtcouplo/dtatmo

      DO j=1, sNy
        DO i=1, sNx
        
          IF (maskC(i,j,1,1,1) .EQ. 1.) THEN

            pass_runoff(i,j)= sum_runoff(i,j)/nper
            pass_precip(i,j)= sum_precip(i,j)/nper
            pass_evap(i,j)= -sum_evap(i,j)/nper   !convert sign convention here
            pass_qnet(i,j)= sum_qnet(i,j)/nper
            pass_fu(i,j)= sum_fu(i,j)/nper
            pass_fv(i,j)= sum_fv(i,j)/nper
            pass_wspeed(i,j)= sum_wspeed(i,j)/nper
            pass_solarnet(i,j)= -sum_solarnet(i,j)/nper !change sign
            pass_slp(i,j)= sum_slp(i,j)/nper*100. _d 0  !convert to Pa
            pass_pCO2(i,j)= sum_pCO2(i,j)*1. _d -6/nper

            sHeating(i,j,1,1) = sum_sHeat(i,j)/nper ! from temp-solve step
            flxCndBt(i,j,1,1)= sum_flxCnB(i,j)/nper ! from temp-solve step
            pass_PrcAtm(i,j)= sum_prcIce(i,j)/nper
            icFrwAtm(i,j,1,1)= sum_evapIce(i,j)/nper
 
C  Load the following directly into the seaice common block
            snowPrc(i,j,1,1)= sum_snowPrc(i,j)/nper

C  Seaice diagnostics - tally at end of coupled period              
            sum_iceMask(i,j)=sum_iceMask(i,j) + 
     &                       iceMask(i,j,1,1)*dtcouplo
            sum_iceHeight(i,j)= sum_iceHeight(i,j) + 
     &                          iceHeight(i,j,1,1)*dtcouplo
            IF (iceMask(i,j,1,1) .NE. 0. _d 0) 
     &           sum_iceTime(i,j)= sum_iceTime(i,j) + dtcouplo
            sum_oceMxLT(i,j)=sum_oceMxLT(i,j) + 
     &                       tOceMxL(i,j,1,1)*dtcouplo
            sum_oceMxLS(i,j)=sum_oceMxLS(i,j) + 
     &                       sOceMxL(i,j,1,1)*dtcouplo

          ENDIF

        ENDDO
      ENDDO  

      CALL INIT_SUMVARS( myThid)

      RETURN
      END
