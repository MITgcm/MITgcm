#include "COST_CPPOPTIONS.h"

      subroutine cost_tau_eddy( myThid )
C     o==========================================================o
C     | subroutine cost_tau_eddy                                 |
C     | o the subroutine computes the sum of the squared errors  |
C     |   relatively to max.                                     |
C     o==========================================================o
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "cost.h"
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#if (defined (ALLOW_TAU_EDDY) && defined (ALLOW_COST_TAU_EDDY))

C     ========= Local variables =========================
      integer i, j, k, jg
      integer bi, bj
      _RL     locfc
      _RL     tau2_max, tau2_temp

C------------------------------------------------------
C     Cost function as a distance to max. value
C------------------------------------------------------
C
C     maximum autorized value of the Eddy stress (squared)
C     from D. Ferreira
C     values beyond will be penalized; 
C     values below are not penalized
      tau2_max = 0.4**2
      locfc = 0.0
c
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        ikey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
#endif /* ALLOW_AUTODIFF_TAMC */
c
        do j=1,sNy
         do i=1,sNx
          do k=2,Nr
           tau2_temp = (rhonil*Eddytaux(i,j,k,bi,bj))**2
           if ( tau2_temp .gt. tau2_max) then
             locfc = locfc + maskW(i,j,k,bi,bj)*
     &               ( tau2_temp - tau2_max )
           endif
           tau2_temp = (rhonil*Eddytauy(i,j,k,bi,bj))**2
           if ( tau2_temp .gt. tau2_max) then
             locfc = locfc + maskS(i,j,k,bi,bj)*
     &               ( tau2_temp - tau2_max )
           endif
          enddo
         enddo
        enddo
c
        objf_eddytau(bi,bj) = locfc
        print*,'objf_eddytau =',locfc
c
       ENDDO
      ENDDO

#endif
 
      end
