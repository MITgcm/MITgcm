C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/bulkf_init.F,v 1.6 2004/05/04 16:32:35 adcroft Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE BULKF_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE BULKF_INIT                                      |
C     | o Set bulk formula parameters                          |
C     |==========================================================|
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"
c #include "DYNVARS.h"
#include "FFIELDS.h"
#include "BULKF.h"
#include "BULKF_DIAG.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif

C     == Routine arguments ==
C     myThid -  Number of this instance of BULKF_INIT
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
c     INTEGER prec
c     CHARACTER*(MAX_LEN_FNAM) fn

      _BARRIER

c     set up bulk formula arrays to zero
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_TIMEAVE
          IF (taveFreq.GT.0.) THEN
          BULKF_timeave(1,bi,bj)=0. _d 0
          DO J=1,sNy
           DO I=1,sNx
c             evapora(i,j,bi,bj)=0. _d 0
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
          DO J=1,sNy
           DO I=1,sNx
#ifdef CONSERV_BULKF
              CONS_Qnet(i,j,bi,bj)=0. _d 0
              CONS_EmPmR(i,j,bi,bj)=0. _d 0
#endif 
              if (.NOT.readsurface) then
c if not reading in surface fields, make sure we do not relax
               SSS(i,j,bi,bj)=0. _d 0
               SST(i,j,bi,bj)=0. _d 0
               tauThetaClimRelax=0. _d 0
               tauSaltClimRelax=0. _d 0
              endif
           ENDDO
          ENDDO
        ENDDO
        ENDDO

#ifdef CONSERV_BULKF
        constim=0
#endif

c
#endif /* ALLOW_BULK_FORCE */

      RETURN
      END
