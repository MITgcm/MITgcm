C $Header: /u/gcmpack/MITgcm/verification/global1x1_tot/code_seaice_adj/Attic/autodiff_inadmode_unset_ad.F,v 1.2 2005/06/16 15:40:07 heimbach Exp $

#include "AUTODIFF_OPTIONS.h"

      subroutine adautodiff_inadmode_unset( myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE autodiff_inadmode_unset                       |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

      inAdMode = inAdFALSE

      useGMREDI = .TRUE.
      useKPP    = .TRUE.
      useSEAICE = .TRUE.

      end









