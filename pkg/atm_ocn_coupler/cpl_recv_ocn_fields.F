#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_RECV_OCN_FIELDS
C     !INTERFACE:
      SUBROUTINE CPL_RECV_OCN_FIELDS( msgUnit, iter )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_RECV_OCN_FIELDS
C     | o Routine for receiving surface fields from ocean
C     |   component.
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "CPL_PARAMS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
C     iter      :: current iteration number
      INTEGER msgUnit, iter

C     !LOCAL VARIABLES:
CEOP

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

      IF ( MOD(cpl_exchange_DIC,2).EQ.1 ) THEN
C       Receive ocn->atm flux of CO2 from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnCO2FluxName,
     I                         Nx_ocn, Ny_ocn,
     O                         fluxCO2_ocn )
      ENDIF

      IF ( MOD(cpl_exchange2W_sIce,2).EQ.1 ) THEN
C-    Receive seaice fraction from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnSIceFracName,
     I                         Nx_ocn, Ny_ocn,
     O                         sIceFrac_ocn )
C-    Receive seaice thickness from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnSIceThickName,
     I                         Nx_ocn, Ny_ocn,
     O                         sIceThick_ocn )
C-    Receive snow thickness from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnSIceSnowName,
     I                         Nx_ocn, Ny_ocn,
     O                         sIceSnowH_ocn )
C-    Receive seaice enthalpy (layer 1) from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnSIceQ1Name,
     I                         Nx_ocn, Ny_ocn,
     O                         sIceQ1_ocn )
C-    Receive seaice enthalpy (layer 2) from ocean component
        CALL COUPRECV_R8TILES( ocnCompName, ocnSIceQ2Name,
     I                         Nx_ocn, Ny_ocn,
     O                         sIceQ2_ocn )
      ENDIF

      RETURN
      END
