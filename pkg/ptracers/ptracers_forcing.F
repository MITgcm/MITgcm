C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_forcing.F,v 1.2 2003/06/27 01:56:17 heimbach Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FORCING(
     I                            bi,bj,k,iTracer,
     U                            gPtracer,
     I                            myIter,myTime,myThid )

C !DESCRIPTION:
C     Adds sources and sinks of passive tracers to the tendancy arrays

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level number
C  iTracer              :: passive tracer index
C  gPtracer             :: the tendancy array
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      INTEGER bi,bj,k,iTracer
      _RL gPtracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gPtracer		:: updates tendancy array

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C Example of how to add forcing for iTracer=1
      IF (iTracer.EQ.1) THEN

C Example of how to add forcing at the surface
       IF (k.EQ.1) THEN
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx

C Here, we'll copy the exact forcing used for salinity to allow us
C to test everything is working.
          gPtracer(i,j,k,bi,bj)=gPtracer(i,j,k,bi,bj)
     &   +maskC(i,j,k,bi,bj)*surfaceTendencyS(i,j,bi,bj)

cph     &      + EmPmR(i,j,bi,bj)*convertFW2Salt
cph     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
cph     &      - lambdaSaltClimRelax*
cph     &           (pTracer(i,j,k,bi,bj,iTracer)-SSS(i,j,bi,bj))

         ENDDO
        ENDDO
       ENDIF

      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
