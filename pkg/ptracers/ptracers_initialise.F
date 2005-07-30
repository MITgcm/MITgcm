C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_initialise.F,v 1.5 2005/07/30 23:53:48 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_INITIALISE

C     !INTERFACE:
      SUBROUTINE PTRACERS_INITIALISE( myThid )

C     !DESCRIPTION:
C     Initialize PTRACERS constant
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "GAD.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer  :: tracer index
      INTEGER iTracer
        
C     Loop over tracers
      DO iTracer = 1, PTRACERS_numInUse
        
        IF (
     &       multiDimAdvection .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_2ND .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_UPWIND_3RD .AND.
     &       PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_4TH ) THEN
c         PTRACERS_multiDimAdv(iTracer) = .TRUE.
          useMultiDimAdvec = .TRUE.
        ENDIF

C       end of Tracer loop
      ENDDO

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Initialize the MNC variable types for PTRACERS
        CALL PTRACERS_MNC_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL PTRACERS_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#endif /* ALLOW_PTRACERS */

      RETURN
      END
