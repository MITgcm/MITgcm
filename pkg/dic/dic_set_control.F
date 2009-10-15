C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_set_control.F,v 1.4 2009/10/15 23:28:43 heimbach Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

cphc$taf common DIC_XX        adname = addic_xx
cphc$taf common DIC_COST_CTRL adname = ADDIC_COST_CTRL


C !INTERFACE: ==========================================================
      subroutine dic_set_control( myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      implicit none

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef DIC_BIOTIC
# include "DIC_VARS.h"
# include "DIC_DIAGS.h"
# include "DIC_CTRL.h"
#endif
#ifdef ALLOW_CTRL
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#endif
C     == Routine arguments ==
      _RL fac
      INTEGER myThid

cph#ifdef DIC_BIOTIC
C     == Local arguments ==
      INTEGER bi, bj
      INTEGER i, j
      integer il
      logical doglobalread
      logical ladinit
      logical equal
      character*( 80)   fnamegen2d
c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP
#ifdef ALLOW_CTRL

      doglobalread = .false.
      ladinit      = .false.

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif
   
      print*,'QQ alpha before', alpha(20,10,1,1)

#ifdef ALLOW_GEN2D_CONTROL
      il=ilnblnk( xx_gen2d_file )
      write(fnamegen2d(1:80),'(2a,i10.10)')
     &     xx_gen2d_file(1:il),'.',optimcycle
      call active_read_xy( fnamegen2d, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_gen2d_dummy )

      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           do i = 1, sNx
             do j = 1, sNy
                alpha (i,j,bi,bj) = alpha(i,j,bi,bj) +
     &               fac*tmpfld2d(i,j,bi,bj)
             end do
          end do
        end do
      end do
cswd -- QQ limits!
cph                if (alpha(i,j,bi,bj).gt.alphamax) then
cph                   alpha(i,j,bi,bj)=alphamax
cph                endif
cph                if (alpha(i,j,bi,bj).lt.alphamin) then
cph                   alpha(i,j,bi,bj)=alphamin
cph                endif
cswd -- QQ limits
      print*,'QQ - preturb alpha', alpha(20,10,1,1), 
     &                             tmpfld2d(20,10,1,1)
#endif

#ifdef ALLOW_DIC_CONTROL

       DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           do i = 1, sNx
             do j = 1, sNy
                feload(i,j,bi,bj) = feload(i,j,bi,bj)*(1.+xx_dic(1))
                rain_ratio(i,j,bi,bj) = 
     &               rain_ratio(i,j,bi,bj)*(1.+xx_dic(2))
             end do
          end do
        end do
      end do

      _EXCH_XY_RL( alpha, mythid )
      _EXCH_XY_RL( rain_ratio, mythid )
      _EXCH_XY_RL( feload, mythid )

      KScav       = KScav       * (1.+1.e+6*xx_dic(3))
      ligand_stab = ligand_stab * (1.+1.e+6*xx_dic(4))
      ligand_tot  = ligand_tot  * (1.+1.e+6*xx_dic(5))

      print *,'COST KScav = ', KScav
      print *,'COST ligand_stab = ', ligand_stab
      print *,'COST ligand_tot = ', ligand_tot

#endif

#endif

cph#endif /* DIC_BIOTIC */

      end
