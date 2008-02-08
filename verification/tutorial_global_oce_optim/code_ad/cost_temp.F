C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/cost_temp.F,v 1.3 2008/02/08 17:06:45 dfer Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_temp( myThid )
C     /==========================================================\
C     | subroutine cost_temp                                  |
C     | o the subroutine computes the sum of the squared errors  |
C     |   relatively to the Levitus climatology                  |
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_TEMP
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj
      integer Nk 
      _RL locfc,tmp
      _RL thetalev(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C
      Nk = 2
C Read annual mean Levitus temperature
C
      CALL READ_FLD_XYZ_RL('lev_t_an.bin',' ',thetalev,0,myThid)
C
C  Total number of wet temperature point
      tmp  = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            do k=1, Nk
               do j=1,sNy
                  do i=1,sNx
                     tmp = tmp + maskC(i,j,k,bi,bj)
                  enddo
               enddo
            enddo
         ENDDO
      ENDDO
      _GLOBAL_SUM_R8( tmp , myThid )
c
      locfc = 0. _d 0
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
     &                     + act3*max1*max2
#endif /* ALLOW_AUTODIFF_TAMC */
c
         do j=1,sNy
            do i=1,sNx
               do k=1,Nk
           locfc = locfc + 1/tmp*maskC(i,j,k,bi,bj)*
     &         wtheta(k,bj,bi)*
     &         ( cMeanTheta(i,j,k,bi,bj) - thetalev(i,j,k,bi,bj) )**2
               enddo
            enddo
         enddo
c
          objf_temp_tut(bi,bj) = locfc
c         print*,'objf_temp_tut =',locfc,startTime,endTime,tmp
c
         ENDDO
      ENDDO
#endif
      end
