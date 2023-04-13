#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     files.

CBOP
C     !ROUTINE: PACKAGES_WRITE_PICKUP

C     !INTERFACE:
      SUBROUTINE PACKAGES_WRITE_PICKUP(
     I                    permPickup, suffix,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write pickup files for each package which needs it to restart.
C     This routine (S/R PACKAGES_WRITE_PICKUP) calls per-package
C     write-pickup (or checkpoint) routines.  It writes both
C     "rolling-pickup" files (ckptA,ckptB) and permanent pickup.

C     !CALLING SEQUENCE:
C     PACKAGES_WRITE_PICKUP
C       |
C       |-- GAD_WRITE_PICKUP
C       |
C       |-- CD_CODE_WRITE_PICKUP
C       |
C       |-- OBCS_WRITE_PICKUP
C       |
C       |-- GGL90_WRITE_PICKUP
C       |
C       |-- GMREDI_WRITE_PICKUP
C       |
C       |-- BBL_WRITE_PICKUP
C       |
C       |-- CHEAPAML_WRITE_PICKUP
C       |
C       |-- FLT_WRITE_PICKUP
C       |
C       |-- PTRACERS_WRITE_PICKUP
C       |
C       |-- GCHEM_WRITE_PICKUP
C       |
C       |-- STREAMICE_WRITE_PICKUP
C       |
C       |-- SHELFICE_WRITE_PICKUP
C       |
C       |-- SEAICE_WRITE_PICKUP
C       |
C       |-- THSICE_WRITE_PICKUP
C       |
C       |-- LAND_WRITE_PICKUP
C       |
C       |-- ATM_PHYS_WRITE_PICKUP
C       |
C       |-- FIZHI_WRITE_PICKUP
C       |-- FIZHI_WRITE_VEGTILES
C       |-- FIZHI_WRITE_DATETIME
C       |
C       |-- DIAGNOSTICS_WRITE_PICKUP
C       |
C       |-- CPL_WRITE_PICKUP
C       |
C       |-- ECCO_WRITE_PICKUP
C       |
C       |-- MYPACKAGE_WRITE_PICKUP

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"

C     !INPUT/OUTPUT PARAMETERS:
C     permPickup :: Is or is not a permanent pickup.
C     suffix     :: pickup-name suffix
C     myTime     :: Current time of simulation ( s )
C     myIter     :: Iteration number
C     myThid     :: Thread number for this instance of the routine.
      LOGICAL permPickup
      CHARACTER*(*) suffix
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

C     Going to really do some IO. Make everyone except master thread wait.
C     this is done within IO routines => no longer needed
c     _BARRIER

#ifdef ALLOW_GENERIC_ADVDIFF
C     Write restart file for 2nd-Order moment (active) Tracers
      IF ( useGAD ) THEN
        CALL GAD_WRITE_PICKUP(
     I                 suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        CALL CD_CODE_WRITE_PICKUP( permPickup,
     I                     suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef  ALLOW_OBCS
      IF (useOBCS) THEN
        CALL OBCS_WRITE_PICKUP(
     &                  suffix, myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_OBCS */

#ifdef  ALLOW_GGL90
      IF ( useGGL90 ) THEN
        CALL GGL90_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_GGL90 */

#ifdef ALLOW_GMREDI
      IF ( useGMRedi ) THEN
        CALL GMREDI_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_BBL
      IF (useBBL) THEN
        CALL BBL_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#ifdef ALLOW_CHEAPAML
C     Write restart file for CHEAPAML pkg
      IF ( useCheapAML ) THEN
         CALL CHEAPAML_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid)
       ENDIF
#endif /* ALLOW_CHEAPAML */

#ifdef ALLOW_FLT
C     Write restart file for floats
      IF (useFLT) THEN
        CALL FLT_WRITE_PICKUP(
     &                  suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_PTRACERS
C     Write restart file for passive tracers
      IF (usePTRACERS) THEN
        CALL PTRACERS_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_GCHEM
C     Write restart file for GCHEM pkg & GCHEM sub-packages
      IF ( useGCHEM ) THEN
        CALL GCHEM_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_STREAMICE
      IF (useStreamIce) THEN
        CALL STREAMICE_WRITE_PICKUP( permPickup,
     I                    suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_SHELFICE
      IF (useShelfIce) THEN
        CALL SHELFICE_WRITE_PICKUP( permPickup,
     I                    suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef  ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_WRITE_PICKUP( permPickup,
     I                    suffix, myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
        CALL THSICE_WRITE_PICKUP( permPickup,
     I                    suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_LAND
C     Write pickup file for Land package:
      IF (useLand) THEN
        CALL LAND_WRITE_PICKUP( permPickup,
     &                  suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_ATM_PHYS
      IF ( useAtm_Phys ) THEN
        CALL ATM_PHYS_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
C     Write pickup file for fizhi package
      IF (usefizhi) THEN
        CALL FIZHI_WRITE_PICKUP(suffix,myTime,myIter,myThid)
        CALL FIZHI_WRITE_VEGTILES(suffix,0,myTime,myIter,myThid)
        CALL FIZHI_WRITE_DATETIME(myTime,myIter,myThid)
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
C     Write pickup file for diagnostics package
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_WRITE_PICKUP( permPickup,
     I                         suffix, myTime, myIter, myThid )
      ENDIF
#endif

#ifdef  COMPONENT_MODULE
      IF (useCoupler) THEN
        CALL CPL_WRITE_PICKUP(
     &                 suffix, myTime, myIter, myThid )
      ENDIF
#endif  /* COMPONENT_MODULE */

#ifdef ALLOW_ECCO
      IF (useECCO) THEN
        CALL ECCO_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_ECCO */

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) THEN
        CALL MYPACKAGE_WRITE_PICKUP( permPickup,
     I                      suffix, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

C--   Every one else must wait until writing is done.
C     this is done within IO routines => no longer needed
c     _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
