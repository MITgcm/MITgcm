#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_FIELDS
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_FIELDS                           |
C     | o Routine for sending surface flux to ocean component    |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Send Sea Level atmos. pressure to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSLPrName, 
     I                       Nx_ocn, Ny_ocn, atmSLPr_ocn )

C     Send (net) heatflux. to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnHeatFluxName, 
     I                       Nx_ocn, Ny_ocn, HeatFlux_ocn )

C     Send net shortwave radiation to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnQshortWaveName, 
     I                       Nx_ocn, Ny_ocn, qShortWave_ocn )

C     Send Zonal momentum flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnTauXName, 
     I                       Nx_ocn, Ny_ocn, TauX_ocn )

C     Send Meridional momentum flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnTauYName, 
     I                       Nx_ocn, Ny_ocn, TauY_ocn )

C     Send E-P-R to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnFWFluxName, 
     I                       Nx_ocn, Ny_ocn, FWFlux_ocn )

C     Send salt flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSaltFxName, 
     I                       Nx_ocn, Ny_ocn, SaltFlx_ocn )

C     Send sea-ice mass to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnSeaIceName, 
     I                       Nx_ocn, Ny_ocn, sIceMass_ocn )

      IF ( ocnCpl_exchange_DIC ) THEN

C       Send atmos CO2 to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnAirCO2Name, 
     I                       Nx_ocn, Ny_ocn, aCO2_ocn )

C       Send surface windspeed to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnWSpdName, 
     I                       Nx_ocn, Ny_ocn, wSpeed_ocn )

C       Send sea-ice fraction to ocean component
        CALL COUPSEND_R8TILES( ocnCompName, ocnFIceName, 
     I                       Nx_ocn, Ny_ocn, SIceFrac_ocn )

      ENDIF 
     
      RETURN
      END

#endif

