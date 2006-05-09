C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_init_varia.F,v 1.1 2006/05/09 19:10:08 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE BULKF_INIT_VARIA( myThid )

C     *==========================================================*
C     | SUBROUTINE BULKF_INIT_VARIA
C     | o Initialize bulk formula arrays
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#include "BULKF_TAVE.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  Number of this instance of BULKF_INIT_VARIA
      INTEGER myThid
CEOP

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J

      _BARRIER

C     set up bulk formula arrays to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
             Tair  (i,j,bi,bj) = 0.
             Qair  (i,j,bi,bj) = 0.
             Solar (i,j,bi,bj) = 0.
             flwdwn(i,j,bi,bj) = 0.
             cloud (i,j,bi,bj) = 0.
             wspeed(i,j,bi,bj) = 0.
             uwind (i,j,bi,bj) = 0.
             vwind (i,j,bi,bj) = 0.
             runoff(i,j,bi,bj) = 0.
#ifdef ALLOW_FORMULA_AIM
             thAir (i,j,bi,bj) = 0.
#endif
             qnetch(i,j,bi,bj) = 0.
             empch (i,j,bi,bj) = 0.
           ENDDO
         ENDDO
#ifdef ALLOW_TIMEAVE
         IF (blk_taveFreq.GT.0.) THEN
          BULKF_timeave(1,bi,bj)=0. _d 0
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
              BULK_Qnet_AVE(i,j,bi,bj)=0. _d 0
              BULK_EmPmR_AVE(i,j,bi,bj)=0. _d 0
              BULK_fu_AVE(i,j,bi,bj)=0. _d 0
              BULK_fv_AVE(i,j,bi,bj)=0. _d 0
              BULK_latent_AVE(i,j,bi,bj)=0. _d 0
              BULK_sensible_AVE(i,j,bi,bj)=0. _d 0
              BULK_evap_AVE(i,j,bi,bj)=0. _d 0
              BULK_flwup_AVE(i,j,bi,bj)=0. _d 0
              BULK_flwupnet_AVE(i,j,bi,bj)=0. _d 0
              BULK_solar_AVE(i,j,bi,bj)=0. _d 0
              BULK_ssq_AVE(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDDO
         ENDIF
#endif /* ALLOW_TIMEAVE */
#ifdef CONSERV_BULKF
         DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
              CONS_Qnet(i,j,bi,bj)=0. _d 0
              CONS_EmPmR(i,j,bi,bj)=0. _d 0
           ENDDO
         ENDDO
#endif
       ENDDO
      ENDDO

#ifdef CONSERV_BULKF
        constim=0.
#endif

#endif /* ALLOW_BULK_FORCE */

      RETURN
      END
