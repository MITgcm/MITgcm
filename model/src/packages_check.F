C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.9 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_CHECK
C     !INTERFACE:
      SUBROUTINE PACKAGES_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_CHECK                                 
C     | o Check runtime activated packages have been built in.    
C     *==========================================================*
C     | All packages can be selected/deselected at build time     
C     | ( when code is compiled ) and activated/deactivated at    
C     | runtime. This routine does a quick check to trap packages 
C     | that were activated at runtime but that were not compiled 
C     | in at build time.                                         
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  Number of this instances
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CHECK( myThid )
#else
      IF (useKPP) CALL PACKAGES_CHECK_ERROR('KPP', myThid)
#endif

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CHECK( myThid )
#else
      IF (useGMRedi) CALL PACKAGES_CHECK_ERROR('GMREDI', myThid)
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_CHECK( myThid )
#else
      IF (useOBCS) CALL PACKAGES_CHECK_ERROR('OBCS', myThid)
#endif

#ifndef ALLOW_AIM
      IF (useAIM) CALL PACKAGES_CHECK_ERROR('AIM', myThid)
#endif

#ifndef ALLOW_FLT
      IF (useFLT) CALL PACKAGES_CHECK_ERROR('FLT', myThid)
#endif

      RETURN
      END
