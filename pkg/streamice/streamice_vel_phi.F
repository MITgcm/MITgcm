C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_vel_phi.F,v 1.1 2015/02/21 19:09:53 dgoldberg Exp $
C $Name:  $

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
     O           cg_iters)
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

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
      INTEGER maxNLIter
      INTEGER maxCGIter
      INTEGER i,j,bi,bj
      _RL cgtol
      INTEGER cg_iters

#ifdef ALLOW_STREAMICE
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))

C     LOCAL VARIABLES

      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef STREAMICE_HYBRID_STRESS
       CALL STREAMICE_VISC_BETA_HYBRID ( myThid )
#else
       CALL STREAMICE_VISC_BETA ( myThid )
#endif

      _EXCH_XY_RL( tau_beta_eff_streamice , myThid )
      _EXCH_XY_RL( visc_streamice , myThid )

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

#endif
#endif
      RETURN
      END
