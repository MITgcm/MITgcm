#include "CPP_OPTIONS.h"

C     !ROUTINE: ICE_FREEZE
C     !INTERFACE:
      SUBROUTINE ICE_FREEZE( bi, bj, iMin, iMax, jMin, jMax,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREEZE     
C     | o Check water temperature and limit range of temperature
C     | appropriately.                            
C     *==========================================================*
C     \ev 

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "ICE.h"
#include "BULKF_ICE_CONSTANTS.h"
#include "BULKF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER i,j
      _RL Tf
      _RL frzmlt, ffresh,fsalt, compact
      _RL cphm

C     Check for water that should have frozen
       DO j=jMin,jMax
        DO i=iMin,iMax
         if (hFacC(i,j,1,bi,bj).gt.0.d0) then
          Tf=-mu_Tf*salt(i,j,1,bi,bj)
          IF (iceMask(i,j,bi,bj).eq.0.and.
     &                gT(i,j,1,bi,bj) .LT. Tf) THEN
           fsalt=0.D0
           ffresh=0.D0
           compact=0.d0
           cphm = cpwater*rhosw*drF(1)*hFacC(i,j,1,bi,bj)
           frzmlt = (Tf-gT(i,j,1,bi,bj))*cphm/deltaTtracer
           call ice_start(i,j,bi, bj, myThid,
     &            frzmlt,ffresh,fsalt,Tf, compact)
           gT(i,j,1,bi,bj)=Tf
cQQ problem here
c if after calc_gs
c          gS(i,j,1,bi,bj)=gS(i,j,1,bi,bj)+
c    &                     (ffresh-fsalt)/RHONIL
c    &                      *recip_dRf(1)*35.*deltaTtracer
c if before calc_gs
c Freshwater flux:
          ffresh=-(ffresh-fsalt)/RHONIL
          surfaceTendencyS(i,j,bi,bj) =
     &      surfaceTendencyS(i,j,bi,bj) +
     &      (ffresh)*recip_dRf(1)*35.
          ENDIF
         ENDIF
        ENDDO
       ENDDO

      RETURN
      END

