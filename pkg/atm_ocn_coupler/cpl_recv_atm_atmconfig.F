C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_recv_atm_atmconfig.F,v 1.4 2013/12/02 23:25:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_RECV_ATM_ATMCONFIG

C !INTERFACE:
      SUBROUTINE CPL_RECV_ATM_ATMCONFIG

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_RECV_ATM_ATMCONFIG
C     | o Routine for receiving atmos config into coupling level.
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model. For now just get the models land/sea mask.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C !LOCAL VARIABLES:
      INTEGER tmpFld(1)
CEOP

C     Get config from atmos component
C     o receive number of coupler time-steps the atmos will do
      CALL COUPRECV_I4VEC( atmCompName,
     I                     'nCouplingSteps', 1,
     O                     tmpFld )
      nSteps_atm = tmpFld(1)

C     o receive Atmosphere Land/Sea mask
      CALL COUPRECV_R8TILES( atmCompName, atmLandName, Nx_atm, Ny_atm,
     &                       landMask_atm )

      RETURN
      END
