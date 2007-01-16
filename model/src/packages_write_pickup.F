C $Header: /u/gcmpack/MITgcm/model/src/packages_write_pickup.F,v 1.25 2007/01/16 04:40:05 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     files.

CBOP
C     !ROUTINE: PACKAGES_WRITE_PICKUP

C     !INTERFACE:
      SUBROUTINE PACKAGES_WRITE_PICKUP(
     I                    permPickup,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write pickup files for each package which needs it to restart.
C     This routine (S/R PACKAGES_WRITE_PICKUP) calls per-package
C     write-pickup (or checkpoint) routines.  It writes both
C     "rolling-pickup" files (ckptA,ckptB) and permanent pickup.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     permPickup :: Is or is not a permanent pickup.
C     myTime     :: Current time of simulation ( s )
C     myIter     :: Iteration number
C     myThid     :: Thread number for this instance of the routine.
      LOGICAL permPickup
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C     == Local variables ==
C     prec   :: file precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL lgf
CEOP

C     Going to really do some IO. Make everyone except master thread wait.
      _BARRIER

      prec = precFloat64
      lgf = globalFile

C     Create suffix to pass on to package pickup routines
      IF ( permPickup ) THEN
        WRITE(fn,'(I10.10)') myIter
      ELSE
        WRITE(fn,'(A)') checkPtSuff(nCheckLev)
      ENDIF

#ifdef ALLOW_GENERIC_ADVDIFF
C     Write restart file for 2nd-Order moment (active) Tracers
      IF ( useGAD ) THEN
        CALL GAD_WRITE_PICKUP(
     I                 fn, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        CALL CD_CODE_WRITE_PICKUP(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef  ALLOW_OBCS
C     SPK 4/9/01: Open boundary checkpointing
      IF (useOBCS) THEN
        CALL OBCS_WRITE_CHECKPOINT(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif  /* ALLOW_OBCS */

#ifdef  ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_WRITE_PICKUP(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif  /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
        CALL THSICE_WRITE_PICKUP(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef  COMPONENT_MODULE
      IF (useCoupler) THEN
        CALL CPL_WRITE_PICKUP(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif  /* COMPONENT_MODULE */

#ifdef ALLOW_FLT
C     Write restart file for floats
      IF (useFLT) THEN
        CALL FLT_RESTART(myTime, myIter, myThid)
      ENDIF
#endif

#ifdef ALLOW_LAND
C     Write pickup file for Land package:
      IF (useLand) THEN
        CALL LAND_WRITE_PICKUP(permPickup,fn,
     &       myTime,myIter,myThid)
      ENDIF
#endif

#ifdef ALLOW_FIZHI
C     Write pickup file for fizhi package
      IF (usefizhi) THEN
        CALL FIZHI_WRITE_PICKUP(fn,myTime,myIter,myThid)
        CALL FIZHI_WRITE_VEGTILES(fn,0,myTime,myIter,myThid)
        CALL FIZHI_WRITE_DATETIME(myTime,myIter,myThid)
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
C     Write pickup file for diagnostics package
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_WRITE_PICKUP(permPickup,
     &       fn,myTime,myIter,myThid)
      ENDIF
#endif

#ifdef  ALLOW_GGL90
      IF ( useGGL90 ) THEN
        CALL GGL90_WRITE_CHECKPOINT(
     &       prec, lgf, permPickup, myIter, myThid)
      ENDIF
#endif  /* ALLOW_GGL90 */

#ifdef ALLOW_PTRACERS
C     Write restart file for passive tracers
      IF (usePTRACERS) THEN
        CALL PTRACERS_WRITE_PICKUP(permPickup,
     &       fn,myIter,myTime,myThid)
      ENDIF
#endif /* ALLOW_PTRACERS */

C--   Every one else must wait until writing is done.
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
