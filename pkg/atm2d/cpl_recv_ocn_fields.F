#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE CPL_RECV_OCN_FIELDS       
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_OCN_FIELDS                           |
C     | o Routine for receiving surface fields from ocean        |
C     |   component.                                             |
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

C     Receive Ocean mixed-layer depths from ocean component
      CALL COUPRECV_R8TILES( ocnCompName, ocnMxlDName, 
     I                       Nx_ocn, Ny_ocn, 
     O                       OcMxlD_ocn )

C     Receive sea surface temperature from ocean component
      CALL COUPRECV_R8TILES( ocnCompName, ocnSSTName, 
     I                       Nx_ocn, Ny_ocn, 
     O                       SST_ocn )

C     Receive sea surface salinity from ocean component
      CALL COUPRECV_R8TILES( ocnCompName, ocnSSSName, 
     I                       Nx_ocn, Ny_ocn, 
     O                       SSS_ocn )

C     Receive sea surface velocity square from ocean component
      CALL COUPRECV_R8TILES( ocnCompName, ocnSSVsqName, 
     I                       Nx_ocn, Ny_ocn, 
     O                       vSq_ocn )

      IF ( ocnCpl_exchange_DIC ) THEN
C       Receive ocn->atm flux of CO2 from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnFCO2Name, 
     I                       Nx_ocn, Ny_ocn, 
     O                       fluxCO2_ocn )
      ENDIF

      RETURN
      END

#endif
