#include "CPP_OPTIONS.h"

C     !ROUTINE: BULKF_AVE
C     !INTERFACE:
      SUBROUTINE BULKF_AVE(bi,bj,mythid, fswnet,
     &                     flh, fsh, flwup, evap, savssq, flwupnet)
C     *==========================================================*
C     | S/R  BULKF_AVE
C     | o save values for timeaveraging
C     |   for sea/ice surface and atmosphere
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_BULK_FORCE
#include "FFIELDS.h"
#include "BULKF.h"
#include "BULKF_DIAG.h" 
#endif
c     == routine arguments ==

      integer mythid
      integer bi,bj
      integer i,j,k

      _RL     evap(1-olx:snx+olx,1-oly:sny+oly)
      _RL     savssq(1-olx:snx+olx,1-oly:sny+oly)
      _RL     fsh(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flh(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flwup(1-olx:snx+olx,1-oly:sny+oly)
      _RL     fswnet(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flwupnet(1-olx:snx+olx,1-oly:sny+oly)

#ifdef ALLOW_BULK_FORCE
#ifdef ALLOW_TIMEAVE


C--     Time-average
          do j = 1,sny
            do i = 1,snx
             BULK_Qnet_AVE(i,j,bi,bj)=BULK_Qnet_AVE(i,j,bi,bj)
     &                       +Qnet(i,j,bi,bj)*deltaTclock
c    &          +lambdaThetaClimRelax*
c    &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))/
c    &           (recip_Cp*recip_rhoNil*recip_dRf(1))*deltaTclock
             BULK_EmPmR_AVE(i,j,bi,bj)=BULK_EmPmR_AVE(i,j,bi,bj)
     &                       +EmPmR(i,j,bi,bj)*deltaTclock
             BULK_fu_AVE(i,j,bi,bj)=BULK_fu_AVE(i,j,bi,bj)
     &                       +fu(i,j,bi,bj)*deltaTclock
             BULK_fv_AVE(i,j,bi,bj)=BULK_fv_AVE(i,j,bi,bj)
     &                       +fv(i,j,bi,bj)*deltaTclock
             BULK_latent_AVE(i,j,bi,bj)=BULK_latent_AVE(i,j,bi,bj)
     &                       +flh(i,j)*deltaTclock
             BULK_sensible_AVE(i,j,bi,bj)=BULK_sensible_AVE(i,j,bi,bj)
     &                       +fsh(i,j)*deltaTclock
             BULK_evap_AVE(i,j,bi,bj)=BULK_evap_AVE(i,j,bi,bj)
     &                       +evap(i,j)*deltaTclock
             BULK_flwup_AVE(i,j,bi,bj)=BULK_flwup_AVE(i,j,bi,bj)
     &                       +flwup(i,j)*deltaTclock
             BULK_flwupnet_AVE(i,j,bi,bj)=BULK_flwupnet_AVE(i,j,bi,bj)
     &                       +flwupnet(i,j)*deltaTclock
             BULK_solar_AVE(i,j,bi,bj)=BULK_solar_AVE(i,j,bi,bj)
     &                       +fswnet(i,j)*deltaTclock
             BULK_ssq_AVE(i,j,bi,bj)=BULK_ssq_AVE(i,j,bi,bj)
     &                       +savssq(i,j)*deltaTclock
            enddo
           enddo
           BULKF_timeave(1,bi,bj)=BULKF_timeave(1,bi,bj)+deltaTclock


#endif  /*ALLOW_TIMEAVE*/
#endif  /*ALLOW_BULK_FORCE*/
       return
      end

