
#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

CBOP
C !ROUTINE: RBCS_ADD_PTENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_ADD_PTENDENCY(bi,bj,iMin,iMax,jMin,jMax,k,
     &                           iTracer, myTime,myIter, myThid )

C !DESCRIPTION:
C     Will update tracer tendencies gPtr with terms to relax
C     at boundaries

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  iTracer              :: ptracer number
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER myThid, myIter
      _RL myTime
      INTEGER iTracer
      INTEGER bi,bj,imin,imax,jmin,jmax,k

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      INTEGER niter
CEOP

#ifdef ALLOW_RBCS
      if (useRBCptracers) then
       if (useRBCptrnum(iTracer)) then
        DO j=jMin,jMax
        DO i=iMin,iMax
         gPtr(I,J,K,bi,bj,iTracer) = gPtr(I,J,K,bi,bj,iTracer)
     &       - maskC(I,J,K,bi,bj)*
     &      RBC_mask(I,J,K,bi,bj)/tauRelax*
     &      (pTracer(I,J,K,bi,bj,iTracer)-
     &             RBC_ptracers(I,J,K,bi,bj,iTracer))
        ENDDO
        ENDDO
       endif
      endif
#endif /* ALLOW_RBCS */

      RETURN
      END
