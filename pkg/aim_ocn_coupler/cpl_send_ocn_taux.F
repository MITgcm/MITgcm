C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_ocn_taux.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_TAUX
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_TAUX                             |
C     | o Routine for sending zonal momentum forcing to ocean    |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just send atmosphere depths.              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Map atmos. zonal momentum flux onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm, 
     I     TauX_atm, 
     I     Nx_ocn, Ny_ocn,
     O     TauX_ocn)

C     Send zonal momentum flux to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnTauxName, 
     I                       Nx_ocn, Ny_ocn,
     I                       TauX_ocn )

      RETURN
      END
