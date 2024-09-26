#include "AUTODIFF_OPTIONS.h"
#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: ADAUTODIFF_WHTAPEIO_SYNC

C !INTERFACE: ==========================================================
      SUBROUTINE ADAUTODIFF_WHTAPEIO_SYNC(
     I     myLev, myStep, myThid )
C !DESCRIPTION:
C     Handwritten AD-routine of S/R AUTODIFF_WHTAPEIO_SYNC.  Used to set
C     and reseet the common block variable tapeFileUnit,
C     tapeFileCounter, and tapeBufferIO, etc. Only requried for tapes of
C     the initialisation phase

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "AUTODIFF.h"
#ifdef ALLOW_WHIO_3D
# include "MDSIO_BUFF_WH.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     myLev     :: tape level identifier
C     myStep    :: identifies from where this routine is called
C                  (1,0 = before after ad-routines)
C     myThid    :: my Thread Id number
      INTEGER myThid
      INTEGER myLev
      INTEGER myStep
CEOP

#ifdef ALLOW_AUTODIFF_WHTAPEIO
C     reset
      tapeFileUnit=0
      tapeFileCounter=0
      tapeBufferIO=.FALSE.
# ifdef ALLOW_WHIO_3D
      writeWh=.FALSE.
      iWh=0
      jWh=0
# endif
C     This subroutine is always called first with myStep = 1 and then
C     with myStep = 0, so it is probably safe to always set the
C     tapeFileUnit here.
      IF ( myStep .EQ. 1 ) THEN
       tapeFileUnit = tapeFileUnitS(myLev)
       tapeBufferIO = .TRUE.
      ENDIF
#endif

      RETURN
      END
