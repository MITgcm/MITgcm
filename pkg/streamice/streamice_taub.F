#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_TAUB ( myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_HYBRID_STRESS

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l
      _RL umid, vmid

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
cph(
#ifdef ALLOW_STREAMICE_FP_ADJ
          taubx_new_si (i,j,bi,bj) = 0.
          tauby_new_si (i,j,bi,bj) = 0.
#else
          streamice_taubx (i,j,bi,bj) = 0.
          streamice_tauby (i,j,bi,bj) = 0.
#endif
cph)
          IF (STREAMICE_hmask(i,j,bi,bj).eq.1) THEN

c            streamice_taubx (i,j,bi,bj) =
c      &      tau_beta_eff_streamice(i,j,bi,bj) * U_streamice(i,j,bi,bj)
c            streamice_tauby (i,j,bi,bj) =
c      &      tau_beta_eff_streamice(i,j,bi,bj) * V_streamice(i,j,bi,bj)

           umid = 0.0
           vmid = 0.0

           DO k=0,1
            DO l=0,1
             umid = umid + 0.25 *
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) *
     &        recip_rA(i,j,bi,bj) *
#ifdef ALLOW_STREAMICE_FP_ADJ
     &        U_new_si(i+k,j+l,bi,bj)
#else
     &        U_streamice(i+k,j+l,bi,bj)
#endif
             vmid = vmid + 0.25 *
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) *
     &        recip_rA(i,j,bi,bj) *
#ifdef ALLOW_STREAMICE_FP_ADJ
     &        V_new_si(i+k,j+l,bi,bj)
#else
     &        V_streamice(i+k,j+l,bi,bj)
#endif
            ENDDO
           ENDDO

#ifdef ALLOW_STREAMICE_FP_ADJ
           taubx_new_si (i,j,bi,bj) =
     &      tau_beta_eff_streamice(i,j,bi,bj) * umid
           tauby_new_si (i,j,bi,bj) =
     &      tau_beta_eff_streamice(i,j,bi,bj) * vmid
#else
           streamice_taubx (i,j,bi,bj) =
     &      tau_beta_eff_streamice(i,j,bi,bj) * umid
           streamice_tauby (i,j,bi,bj) =
     &      tau_beta_eff_streamice(i,j,bi,bj) * vmid
#endif
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif
#endif
      RETURN
      END
