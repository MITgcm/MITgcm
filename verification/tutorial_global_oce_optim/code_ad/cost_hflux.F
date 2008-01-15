C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/cost_hflux.F,v 1.1 2008/01/15 21:16:42 dfer Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "COST_CPPOPTIONS.h"

      subroutine cost_hflux( myThid )
C     /==========================================================\
C     | subroutine cost_hflux                                     |
C     | o the subroutine computes the cost function relative to  |
C     |   surface hflux optimization                              |
C     \==========================================================/
       implicit none

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

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#if (defined (ALLOW_COST_HFLUXM) && defined (ALLOW_HFLUXM_CONTROL))
C     ========= Local variables =========================
      integer i, j
      integer bi, bj
      _RL     locfc,tmpC
C
      tmpC = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            do j=1,sNy
               do i=1,sNx
                  tmpC = tmpC + maskC(i,j,1,bi,bj)
               enddo
            enddo
         ENDDO
      ENDDO
      _GLOBAL_SUM_R8( tmpC , myThid )
      print *,'tmpC',tmpC
      locfc = 0. _d 0
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
     &                      + act3*max1*max2
#endif /* ALLOW_AUTODIFF_TAMC */
c
      do j=1,sNy
         do i=1,sNx
         locfc = locfc + 1/tmpC*maskC(i,j,1,bi,bj)*
     &         whfluxm(i,j,bi,bj)*
     &        (
     &         Qnetm(i,j,bi,bj)
     &        )**2
         enddo
      enddo
c
      objf_hfluxm(bi,bj) = locfc
      print*,'objf_hfluxm =',locfc
c
         ENDDO
        ENDDO

#endif
 
      end
