C $Header: /u/gcmpack/MITgcm/pkg/atm2d/Attic/receive_component_configs.F,v 1.2 2007/10/08 23:48:28 jmc Exp $
C $Name:  $

#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE RECEIVE_COMPONENT_CONFIGS
C     /==========================================================\
C     | SUBROUTINE RECEIVE_COMPONENT_CONFIGS                     |
C     | o Rotuine which controls receiving the configurations    |
C     |   of each of the component models.                       |
C     \==========================================================/
      IMPLICIT NONE

C     Data on ocean grid
#include "OCNSIZE.h"
#include "OCNVARS.h"

C     Receive ocean config (OCNCONFIG) from ocean
C     component (OCN).
      CALL CPL_RECV_OCN_OCNCONFIG

      RETURN
      END

#endif

