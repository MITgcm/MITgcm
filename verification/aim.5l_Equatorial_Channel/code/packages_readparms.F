#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE PACKAGES_READPARMS(myThid)
C     /==========================================================\
C     | SUBROUTINE PACKAGES_READPARMS                            |
C     | o Read runtime package configuration parameters          |
C     |==========================================================|
C     |   Packages can have internal runtime configuration       |
C     |   parameters. A package provides a routine               |
C     |   ${PKGNAME}_READ_PARMS to read these parameters. In     |
C     |   general this routine should read parameters from a file|
C     |   called data.${pkgname}.                                |
C     |   This routine (S/R PACKAGES_READPARMS) calls per-package|
C     |   parameter reading routines.                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameters
      IF (useGMRedi) CALL GMREDI_READPARMS( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP parameters
      IF (useKPP) CALL KPP_READPARMS( myThid )
#endif

#ifdef INCLUDE_ECCO_PACKAGE
c--   Initialize ECCO package parameters
      IF (useECCO) CALL ECCO_READPARMS ( myThid )
#endif

#ifdef ALLOW_AIM
c--   Read in AIM package parameters
C     Currently AIM does not have any runtime parameters
C     IF (useAIM) CALL AIM_READPARMS ( myThid )
#endif

      END
