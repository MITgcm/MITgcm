C $Header: /u/gcmpack/MITgcm/verification/lab_sea/code_ad/Attic/autodiff_inadmode_set_ad.F,v 1.2 2007/10/09 02:36:27 jmc Exp $
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

cph      useKPP = .FALSE.
cph      useGMREDI = .FALSE.

      end









