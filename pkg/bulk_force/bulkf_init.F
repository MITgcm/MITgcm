
#include "CPP_OPTIONS.h"

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
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
cswdblk -- add ---
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#include "BULKF_DIAG.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif
#endif
cswdblk -- end add --

C     == Routine arguments ==
C     myThid -  Number of this instance of BULKF_INIT
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn

      _BARRIER

cswdblk -- add ----
#ifdef ALLOW_BULK_FORCE
c     set up bulk formula arrays to zero
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1,sNy
           DO I=1,sNx
              evapora(i,j,bi,bj)=0.d0
              BULK_Qnet_AVE(i,j,bi,bj)=0.e0
              BULK_EmPmR_AVE(i,j,bi,bj)=0.e0
              BULK_fu_AVE(i,j,bi,bj)=0.e0
              BULK_fv_AVE(i,j,bi,bj)=0.e0
              BULK_latent_AVE(i,j,bi,bj)=0.e0
              BULK_sensible_AVE(i,j,bi,bj)=0.e0
              BULK_evap_AVE(i,j,bi,bj)=0.e0
              BULK_flw_AVE(i,j,bi,bj)=0.e0
              BULK_solar_AVE(i,j,bi,bj)=0.e0
              BULK_ssq_AVE(i,j,bi,bj)=0.e0
              BULKF_timeave(1,bi,bj)=0.e0
#ifdef CONSERV_BULKF
              CONS_Qnet(i,j,bi,bj)=0.e0
              CONS_EmPmR(i,j,bi,bj)=0.e0
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
#endif
cswdblk -------

      RETURN
      END
