#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_INIT_ATM_VARS

C !INTERFACE:
      SUBROUTINE CPL_INIT_ATM_VARS( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_INIT_ATM_VARS
C     | o Initialise to zero all ATMVARS.h arrays
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
c#include "ATMIDS.h"

C !INPUT PARAMETERS:
C     msgUnit          :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
C     i,j     :: loop index
      INTEGER i, j
CEOP

      WRITE(msgUnit,'(2A)') 'CPL_INIT_ATM_VARS: ',
     &                      'initialise all ATMVARS.h arrays'

      DO j=1,Ny_atm
        DO i=1,Nx_atm
          landMask_atm   (i,j) = 0. _d 0
          OcMxlD_atm     (i,j) = 0. _d 0
          SST_atm        (i,j) = 0. _d 0
          SSS_atm        (i,j) = 0. _d 0
          vSq_atm        (i,j) = 0. _d 0
          fluxCO2_atm    (i,j) = 0. _d 0
          atmSLPr_atm    (i,j) = 0. _d 0
          HeatFlux_atm   (i,j) = 0. _d 0
          qShortWave_atm (i,j) = 0. _d 0
          TauX_atm       (i,j) = 0. _d 0
          TauY_atm       (i,j) = 0. _d 0
          EvMPr_atm      (i,j) = 0. _d 0
          RunOff_atm     (i,j) = 0. _d 0
          ROEnFx_atm     (i,j) = 0. _d 0
          SaltFlx_atm    (i,j) = 0. _d 0
          sIceMass_atm   (i,j) = 0. _d 0
          saltPlmFlx_atm (i,j) = 0. _d 0
          aCO2_atm       (i,j) = 0. _d 0
          wSpeed_atm     (i,j) = 0. _d 0
          sIceFrac_atm   (i,j) = 0. _d 0
          sIceThick_atm  (i,j) = 0. _d 0
          sIceSnowH_atm  (i,j) = 0. _d 0
          sIceQ1_atm     (i,j) = 0. _d 0
          sIceQ2_atm     (i,j) = 0. _d 0
        ENDDO
      ENDDO

      RETURN
      END
