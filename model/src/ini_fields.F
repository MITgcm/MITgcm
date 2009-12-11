C $Header: /u/gcmpack/MITgcm/model/src/ini_fields.F,v 1.24 2009/12/11 13:56:28 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: INI_FIELDS

C     !INTERFACE:
      SUBROUTINE INI_FIELDS( myThid )

C     !DESCRIPTION:
C     Initial conditions of the model state variables are set here. For
C     continuation runs a restart file will be used. For original runs
C     custom rules can be included here.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     Set model variables to initial/restart conditions
      IF ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ' ) THEN
        CALL INI_VEL( myThid )
        CALL INI_THETA( myThid )
        CALL INI_SALT( myThid )
        CALL INI_PSURF( myThid )
        CALL INI_PRESSURE( myThid )
#ifdef INCLUDE_EP_FORCING_CODE
        CALL INI_EP( myThid )
#endif
      ELSEIF ( .NOT.useOffLine ) THEN
        CALL READ_PICKUP( nIter0, myThid )

c       IF (pickup_write_immed) THEN
c         CALL WRITE_PICKUP(
c    I                       .TRUE., startTime, nIter0, myThid )
c       ENDIF

      ENDIF

#ifdef ALLOW_NONHYDROSTATIC
      IF ( nonHydrostatic ) THEN
        CALL INI_NH_FIELDS( myThid )
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
