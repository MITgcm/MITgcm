C $Header: /u/gcmpack/MITgcm/pkg/atm2d/cpl_recv_ocn_ocnconfig.F,v 1.2 2007/10/08 23:48:28 jmc Exp $
C $Name:  $

#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE CPL_RECV_OCN_OCNCONFIG
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_OCN_OCNCONFIG                        |
C     | o Routine for receiving ocean config into coupling level.|
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just get the models depth map.            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Get config from ocean component
      CALL COUPRECV_R8TILES( ocnCompName,
     I                       ocnMxlDName, Nx_ocn, Ny_ocn,
     O                       OcMxlD_ocn )

      RETURN
      END
#endif
