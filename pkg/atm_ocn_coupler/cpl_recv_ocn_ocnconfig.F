C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_recv_ocn_ocnconfig.F,v 1.4 2013/12/02 23:25:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_RECV_OCN_OCNCONFIG

C !INTERFACE:
      SUBROUTINE CPL_RECV_OCN_OCNCONFIG

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_RECV_OCN_OCNCONFIG
C     | o Routine for receiving ocean config into coupling level.
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model. For now just get the models depth map.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"
#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:

C !LOCAL VARIABLES:
      INTEGER tmpFld(1)
CEOP

C     Get config from ocean component
C     o receive number of coupler time-steps the ocean will do
      CALL COUPRECV_I4VEC( ocnCompName,
     I                     'nCouplingSteps', 1,
     O                     tmpFld )
      nSteps_ocn = tmpFld(1)

C     o receive Ocean Mixed-Layer Depth
      CALL COUPRECV_R8TILES( ocnCompName,
     I                       ocnMxlDName, Nx_ocn, Ny_ocn,
     O                       OcMxlD_ocn )

      RETURN
      END
