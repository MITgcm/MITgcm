C $Header: /u/gcmpack/MITgcm/verification/1D_ocean_ice_column/code_ad/Attic/autodiff_inadmode_set_ad.F,v 1.1 2011/03/28 15:50:51 ifenty Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine adautodiff_inadmode_set( myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE adautodiff_inadmode_set                       |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

      inAdmode = inAdTrue

      useKPP = .FALSE.
      end









