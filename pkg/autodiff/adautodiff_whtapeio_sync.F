C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adautodiff_whtapeio_sync.F,v 1.2 2011/01/21 22:02:50 gforget Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#include "MDSIO_OPTIONS.h"

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
#ifdef ALLOW_WHIO_3D
# include "MDSIO_BUFF_WH.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myLev
      integer myStep

#ifdef ALLOW_AUTODIFF_WHTAPEIO
      tapeFileUnit=0
      tapeFileCounter=0
      tapeBufferIO=.FALSE.
#  ifdef ALLOW_WHIO_3D
      writeWh=.FALSE.
      iWh=0
      jWh=0
#  endif
#endif

      end
