C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.4 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE EXTERNAL_FORCING_SURF                         |
C     | o Determines forcing terms based on external fields      |
C     |   relaxation terms etc.                                  |
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
C     === Functions ===

C     === Local arrays ===

C     === Local variables ===
      INTEGER i,j,bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

      DO j = jMin, jMax
         DO i = iMin, iMax

c     Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = fu(i,j,bi,bj)
     &         *horiVertRatio*recip_rhoNil*recip_dRf(1)
c     Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = fV(i,j,bi,bj)
     &         *horiVertRatio*recip_rhoNil*recip_dRf(1)
c     Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = -Qnet(i,j,bi,bj)
     &        *recip_Cp*recip_rhoNil*recip_dRf(1)
     &         - lambdaThetaClimRelax*
     &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))

#ifdef USE_NATURAL_BCS
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         *recip_dRf(1)*salt(i,j,1,bi,bj)
     &         - lambdaSaltClimRelax*
     &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
#else 
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         *recip_dRf(1)*35.
     &         - lambdaSaltClimRelax*
     &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
#endif          

         ENDDO
      ENDDO

      RETURN
      END
