C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_forcing.F,v 1.13 2007/10/24 19:55:20 dfer Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FORCING(
     I                            bi,bj,iMin,iMax,jMin,jMax,k,iTracer,
     U                            gPtracer,surfPtracer,
     I                            myIter,myTime,myThid )

C !DESCRIPTION:
C     Adds sources and sinks of passive tracers to the tendency arrays

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "FFIELDS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_RBCS
#include "PTRACERS_SIZE.h"
#include "RBCS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  iMin iMax jMin jMax  :: working range of tile for applying forcing
C  k                    :: vertical level number
C  iTracer              :: tracer number
C  gPtracer             :: the tendency array
C  surfPtracer          :: surface forcing term
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k,iTracer
      _RL gPtracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL surfPtracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gPtracer		:: updates tendency array

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
C     number of surface interface layer
      INTEGER kSurface
CEOP

      IF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Surface forcing term surfPtracer is previously computed by 
C     PTRACERS_FORCING_SURF (and stored as surfaceForcingPtr)
C     because it is needed by KPP_TRANSPORT_PTR.

#ifdef ALLOW_GCHEM
      IF ( useGCHEM )
     &     CALL GCHEM_ADD_TENDENCY(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        iTracer,
     I                        myTime,myIter, myThid)
#endif /* ALLOW_GCHEM */

      IF ( k .EQ. kSurface ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gPtracer(i,j,k,bi,bj) = gPtracer(i,j,k,bi,bj)
     &          + surfPtracer(i,j,bi,bj)
     &           *recip_drF(k)*recip_hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_RBCS
       if (useRBCptracers) then
         CALL RBCS_ADD_TENDENCY(bi,bj,k,iTracer+2,
     I                        myTime, myThid)
       endif
#endif


#endif /* ALLOW_PTRACERS */

      RETURN
      END
