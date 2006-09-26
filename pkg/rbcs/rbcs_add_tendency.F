
#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

CBOP
C !ROUTINE: RBCS_ADD_TSTENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_ADD_TENDENCY(bi,bj,k, tracernum,
     &                            myTime, myThid )

C !DESCRIPTION:
C     Will update tendencies with terms to relax to 3-D field

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#endif
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
      INTEGER bi,bj,k
      INTEGER tracernum

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      INTEGER iTracer
      INTEGER irbc
CEOP

#ifdef ALLOW_RBCS

      if (tracernum.eq.1) then
       if (useRBCtemp) then
        DO j=1,sNy
        DO i=1,sNx
         gT(I,J,K,bi,bj) = gT(I,J,K,bi,bj)
     &       - maskC(I,J,K,bi,bj)*
     &      RBC_mask(I,J,K,bi,bj,1)/tauRelaxT*
     &      (theta(I,J,K,bi,bj)-
     &             RBCtemp(I,J,K,bi,bj))
        ENDDO
        ENDDO
       endif
      endif

      if (tracernum.eq.2) then
       if (useRBCsalt) then
        DO j=1,sNy
        DO i=1,sNx
         gS(I,J,K,bi,bj) = gS(I,J,K,bi,bj)
     &       - maskC(I,J,K,bi,bj)*
     &      RBC_mask(I,J,K,bi,bj,2)/tauRelaxS*
     &      (salt(I,J,K,bi,bj)-
     &             RBCsalt(I,J,K,bi,bj))
        ENDDO
        ENDDO
       endif
      endif

#ifdef ALLOW_PTRACERS
      if (tracernum.gt.2) then
       iTracer=tracernum-2
       irbc=min(maskLEN,tracernum)
       if (useRBCptracers) then
        if (useRBCptrnum(iTracer)) then
         DO j=1,sNy
         DO i=1,sNx
           gPtr(I,J,K,bi,bj,iTracer) = gPtr(I,J,K,bi,bj,iTracer)
     &       - maskC(I,J,K,bi,bj)*
     &      RBC_mask(I,J,K,bi,bj,irbc)/tauRelaxPTR(iTracer)*
     &      (pTracer(I,J,K,bi,bj,iTracer)-
     &             RBC_ptracers(I,J,K,bi,bj,iTracer))
         ENDDO
         ENDDO
        endif
       endif
      endif
#endif

#endif /* ALLOW_RBCS */

      RETURN
      END
