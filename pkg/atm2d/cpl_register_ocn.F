#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE CPL_REGISTER_OCN
C     /==========================================================\
C     | SUBROUTINE CPL_REGISTER_OCN                              |
C     | o Routine for registration of ocean config. with coupler |
C     |   layer.                                                 |
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

C     Call MIT Coupler registration acceptance procedure.  Make sure that nobody
C     who registers for ocnCompName has a sub-domain that exceeds the bounds
C     Nx_ocn, Ny_ocn.
C     The first argument to MITCOUPLER_tile_register gives the name of the
C     component whose attributes will be collected by the particular call
C     to MITCOUPLER_tile_register.
C     The second and third arguments to MITCOUPLER_tile_register are used
C     for an internal error check. This check makes sure that the sub-domain
C     sizes reported by each of the component processes for a given component
C     fall within the global domain size the coupler is expecting.
      CALL MITCOUPLER_TILE_REGISTER( ocnCompName, Nx_ocn, Ny_ocn )

      RETURN
      END
#endif
