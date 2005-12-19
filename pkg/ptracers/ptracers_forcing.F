C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_forcing.F,v 1.10 2005/12/19 19:11:28 stephd Exp $
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
C     Adds sources and sinks of passive tracers to the tendancy arrays

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
C  gPtracer             :: the tendancy array
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
C  gPtracer		:: updates tendancy array

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
C     number of surface interface layer
      INTEGER kSurface
CEOP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface = Nr
      else
       kSurface = 1
      endif

C--   Surface forcing term is surfaceForcingPtr, previously computed
C     by PTRACERS_FORCING because it is needed by KPP_TRANSPORT_PTR.

#ifdef ALLOW_GCHEM 
      IF ( useGCHEM ) 
     &     CALL GCHEM_ADD_TENDENCY(
     I                        bi,bj,iMin,iMax,jMin,jMax,k,
     I                        iTracer,
     I                        myTime,myIter, myThid)
#else /* ALLOW_GCHEM */
       IF ( .NOT. useGCHEM .AND. k .EQ. kSurface ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gPtracer(i,j,k,bi,bj) = gPtracer(i,j,k,bi,bj)
     &          + surfPtracer(i,j,bi,bj)
     &           *recip_drF(k)*recip_hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GCHEM */

#ifdef ALLOW_RBCS
       if (useRBCptracers) then
         CALL RBCS_ADD_TENDENCY(bi,bj,k,iTracer+2,
     I                        myTime, myThid)
       endif
#endif


#endif /* ALLOW_PTRACERS */

      RETURN
      END
