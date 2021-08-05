#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VEL_PHI (
     I           myThid,
     I           maxNLIter,
     I           maxCGiter,
     I           cgtol,
     O           cg_iters,
     O           max_err)
C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
!#ifdef ALLOW_PETSC
!#include "finclude/petsc.h"
!#endif

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
      INTEGER maxNLIter
      INTEGER maxCGIter
      INTEGER i,j,bi,bj
      _RL cgtol
      INTEGER cg_iters
      _RL max_err

#ifdef ALLOW_STREAMICE_OAD_FP

C     LOCAL VARIABLES

      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _EXCH_XY_RL( U_streamice , myThid )
      _EXCH_XY_RL( V_streamice , myThid )

#ifdef STREAMICE_HYBRID_STRESS
       CALL STREAMICE_VISC_BETA_HYBRID ( myThid )
#else
       CALL STREAMICE_VISC_BETA ( myThid )
#endif

      _EXCH_XY_RL( tau_beta_eff_streamice , myThid )
      _EXCH_XY_RL( visc_streamice , myThid )

       CALL STREAMICE_GET_VEL_RESID_ERR_OAD (max_err,myThid)

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           u_new_SI(i,j,bi,bj) = U_streamice(i,j,bi,bj)
           v_new_SI(i,j,bi,bj) = V_streamice(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL STREAMICE_CG_WRAPPER(
     &       U_new_si,
     &       V_new_si,
     &       taudx_SI,
     &       taudy_SI,
     &       cgtol,
     &       cg_iters,
     &       maxCGIter,
     &       myThid )

#ifdef STREAMICE_HYBRID_STRESS
        CALL STREAMICE_TAUB (myThid)
#endif

#endif /* ALLOW_STREAMICE_OAD_FP */
      RETURN
      END
