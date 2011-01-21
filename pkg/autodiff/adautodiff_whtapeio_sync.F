C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adautodiff_whtapeio_sync.F,v 1.1 2011/01/21 01:19:03 gforget Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine adautodiff_whtapeio_sync( myLev, myStep, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE adautodiff_whtapeio_sync                             |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myLev
      integer myStep

#ifdef ALLOW_AUTODIFF_WHTAPEIO
      tapeFileUnit=0
      tapeFileCounter=0
#endif

      end
