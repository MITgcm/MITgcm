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
