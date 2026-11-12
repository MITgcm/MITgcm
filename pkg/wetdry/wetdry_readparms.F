#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_READPARMS

C !INTERFACE:
      SUBROUTINE WETDRY_READPARMS( myThid )

C     !DESCRIPTION:
C     Read pkg/wetdry (wetting and drying) parameters from data.wetdry

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "WETDRY.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /WETDRY_PARM01/
     &       wdMinDepth, wdCritDepth, wdDragDepth, wdAdvDepth,
     &       wdGMDepth,
     &       wdKPPDepth, wdKPPCapDepth,
     &       wdSmoothWidth, wdMonFreq, wdMaxSpeed,
     &       wdManningN, wdDragMax, wdMaxFroude,
     &       wdManningFile, wdManningT1, wdManningT2,
     &       wdConserveVol, wdCarryVel,
     &       wdUpwindFace, wdDryForcing

      IF ( .NOT.useWETDRY ) THEN
C-    pkg wetdry is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.wetdry is found
         CALL PACKAGES_UNUSED_MSG( 'useWETDRY', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for wetdry
      wdMinDepth    = 0.05 _d 0
      wdCritDepth   = 0.10 _d 0
C-    off by default: the relaxation strands water on receding beaches
      wdDragDepth   = 0. _d 0
C-    momentum advection is unstable in thin water on 2-D flats
      wdAdvDepth    = 0.5 _d 0
C-    GM/Redi makes large tracer extrema in thin r* layers
      wdGMDepth     = 2. _d 0
C-    optional: the KPP non-local flux fraction is capped at 1 in any
C     case (WETDRY_KPP_TAPER), which removes the T/S extrema it made in
C     thin r* columns
      wdKPPDepth    = 0. _d 0
      wdKPPCapDepth = 20. _d 0
      wdSmoothWidth = 0. _d 0
C-    depth-dependent (Manning) bottom drag: off
      wdManningN    = 0. _d 0
      wdDragMax     = 0.05 _d 0
      wdMaxFroude   = 0. _d 0
      wdManningFile = ' '
      wdManningT1   = 0. _d 0
      wdManningT2   = 0. _d 0
      wdMonFreq     = monitorFreq
C-    coastal flows never reach this (transient fronts with wdCarryVel
C     reach ~10 m/s); a thin-water instability passes it within steps
      wdMaxSpeed    = 30. _d 0
      wdConserveVol = .TRUE.
      wdCarryVel    = .FALSE.
      wdDryForcing  = .TRUE.
C-    the minimum (NLFS) or mean (r*) surface throttles the shoreline flux
      wdUpwindFace  = .TRUE.

      WRITE(msgBuf,'(A)') 'WETDRY_READPARMS: opening data.wetdry'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.wetdry', 'WETDRY_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=WETDRY_PARM01)
      WRITE(msgBuf,'(A)')
     &    'WETDRY_READPARMS: finished reading data.wetdry'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_WETDRY */

      RETURN
      END
