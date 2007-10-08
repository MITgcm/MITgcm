C $Header: /u/gcmpack/MITgcm/pkg/atm2d/Attic/sendout_component_configs.F,v 1.2 2007/10/08 23:48:28 jmc Exp $
C $Name:  $

#include "ATM2D_OPTIONS.h"
#ifdef ATM2D_MPI_ON

CStartOfInterface
      SUBROUTINE SENDOUT_COMPONENT_CONFIGS
C     /==========================================================\
C     | SUBROUTINE SENDOUT_COMPONENT_CONFIGS                     |
C     | o Rotuine which controls distribution of component model |
C     |   configuration information to other components.         |
C     \==========================================================/
      IMPLICIT NONE

C     Send atmos. config (ATMCONFIG) to ocean
C     component (OCN).
      CALL CPL_SEND_OCN_ATMCONFIG

      RETURN
      END

#endif
