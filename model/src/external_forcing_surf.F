C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.5 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
CBOP
C     !ROUTINE: EXTERNAL_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXTERNAL_FORCING_SURF                          
C     | o Determines forcing terms based on external fields       
C     |   relaxation terms etc.                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER myThid
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
CEOP

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
