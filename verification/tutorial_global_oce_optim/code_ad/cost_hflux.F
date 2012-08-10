C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/cost_hflux.F,v 1.6 2012/08/10 19:31:56 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      SUBROUTINE COST_HFLUX( myThid )
C     *==========================================================*
C     | SUBROUTINE COST_HFLUX
C     | o the subroutine computes the cost function relative to
C     |   surface hflux optimization
C     *==========================================================*

       IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "cost.h"
#include "ctrl_weights.h"
#ifdef ALLOW_HFLUXM_CONTROL
#include "FFIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#if (defined (ALLOW_COST_HFLUXM) && defined (ALLOW_HFLUXM_CONTROL))
C     ========= Local variables =========================
      integer i, j
      integer bi, bj
      _RL     locfc,tmpC

      tmpC = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         do j=1,sNy
           do i=1,sNx
             tmpC = tmpC + maskC(i,j,1,bi,bj)
           ENDDO
         ENDDO
       ENDDO
      ENDDO
      _GLOBAL_SUM_RL( tmpC , myThid )
      IF ( tmpC.GT.0. ) tmpC = 1. _d 0 / tmpC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
         act1 = bi - myBxLo(myThid)
         max1 = myBxHi(myThid) - myBxLo(myThid) + 1
         act2 = bj - myByLo(myThid)
         max2 = myByHi(myThid) - myByLo(myThid) + 1
         act3 = myThid - 1
         ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
#endif /* ALLOW_AUTODIFF_TAMC */

         locfc = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
            locfc = locfc + tmpC*maskC(i,j,1,bi,bj)*
     &         whfluxm(i,j,bi,bj)*
     &        (
     &         Qnetm(i,j,bi,bj)
     &        )**2
          ENDDO
         ENDDO

         objf_hflux_tut(bi,bj) = locfc
c        print*,'objf_hflux_tut =',locfc

       ENDDO
      ENDDO

#endif
      RETURN
      END
