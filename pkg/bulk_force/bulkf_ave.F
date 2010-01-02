C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_ave.F,v 1.9 2010/01/02 22:46:29 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_AVE
C     !INTERFACE:
      SUBROUTINE BULKF_AVE(bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  BULKF_AVE
C     | o save values for timeaveraging
C     |   for sea/ice surface and atmosphere
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "BULKF.h"
#include "BULKF_INT.h"
#include "BULKF_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_BULK_FORCE
#ifdef ALLOW_TIMEAVE

      INTEGER i,j

C--     Time-average
      DO j = 1,sNy
        DO i = 1,sNx
          BULK_Qnet_Ave(i,j,bi,bj)     = BULK_Qnet_Ave(i,j,bi,bj)
     &                   +Qnet(i,j,bi,bj)*deltaTclock
c    &          +lambdaThetaClimRelax*
c    &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))/
c    &           (recip_Cp*recip_rhoNil*recip_dRf(1))*deltaTclock
          BULK_EmPmR_Ave(i,j,bi,bj)    = BULK_EmPmR_Ave(i,j,bi,bj)
     &                   +EmPmR(i,j,bi,bj)*deltaTclock
          BULK_fu_Ave(i,j,bi,bj)       = BULK_fu_Ave(i,j,bi,bj)
     &                   +fu(i,j,bi,bj)*deltaTclock
          BULK_fv_Ave(i,j,bi,bj)       = BULK_fv_Ave(i,j,bi,bj)
     &                   +fv(i,j,bi,bj)*deltaTclock
          BULK_latent_Ave(i,j,bi,bj)   = BULK_latent_Ave(i,j,bi,bj)
     &                   +flh(i,j,bi,bj)*deltaTclock
          BULK_sensible_Ave(i,j,bi,bj) = BULK_sensible_Ave(i,j,bi,bj)
     &                   +fsh(i,j,bi,bj)*deltaTclock
          BULK_evap_Ave(i,j,bi,bj)     = BULK_evap_Ave(i,j,bi,bj)
     &                   +evap(i,j,bi,bj)*deltaTclock
          BULK_flwup_Ave(i,j,bi,bj)    = BULK_flwup_Ave(i,j,bi,bj)
     &                   +flwup(i,j,bi,bj)*deltaTclock
          BULK_flwupnet_Ave(i,j,bi,bj) = BULK_flwupnet_Ave(i,j,bi,bj)
     &                   +flwupnet(i,j,bi,bj)*deltaTclock
          BULK_solar_Ave(i,j,bi,bj)    = BULK_solar_Ave(i,j,bi,bj)
     &                   +fswnet(i,j,bi,bj)*deltaTclock
          BULK_ssq_Ave(i,j,bi,bj)      = BULK_ssq_Ave(i,j,bi,bj)
     &                   +savssq(i,j,bi,bj)*deltaTclock
        ENDDO
      ENDDO
      BULK_timeAve(bi,bj) = BULK_timeAve(bi,bj)+deltaTclock

#endif  /*ALLOW_TIMEAVE*/
#endif  /*ALLOW_BULK_FORCE*/

      RETURN
      END

