C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_impldiff.F,v 1.1 2002/03/04 19:01:29 adcroft Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_IMPLDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_IMPLDIFF( bi,bj,KappaR,myThid )

C !DESCRIPTION:
C     Calls the implicit vertical diffusion routine for each passive
C     tracer.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj   :: tile indices
C  KappaR  :: vertical diffusion coefficient
C  myThid  :: thread number
      INTEGER bi,bj
      _RL KappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer :: tracer index
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
CEOP

      iMin=0
      iMax=sNx+1
      jMin=0
      jMax=sNy+1

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaR, recip_HFacC,
     U         gPtr(1-Olx,1-Oly,1,bi,bj,iTracer),
     I         myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
