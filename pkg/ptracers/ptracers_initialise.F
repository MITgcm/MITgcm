C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_initialise.F,v 1.1 2004/07/07 22:26:44 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_INITIALISE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INITIALISE( myThid )

C !DESCRIPTION:
C     Initialize PTRACERS constant

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS.h"
#include "GAD.h"


C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C     iTracer  :: tracer index
      INTEGER iTracer
CEOP
        
C Loop over tracers
      DO iTracer = 1, PTRACERS_numInUse

       IF (
     &     multiDimAdvection .AND.
     &     PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_2ND .AND.
     &     PTRACERS_advScheme(iTracer).NE.ENUM_UPWIND_3RD .AND.
     &     PTRACERS_advScheme(iTracer).NE.ENUM_CENTERED_4TH ) THEN
c        PTRACERS_multiDimAdv(iTracer) = .TRUE.
         useMultiDimAdvec = .TRUE.
       ENDIF

C end of Tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
