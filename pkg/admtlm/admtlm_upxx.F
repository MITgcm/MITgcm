C $Header: /u/gcmpack/MITgcm/pkg/admtlm/Attic/admtlm_upxx.F,v 1.1 2004/11/17 03:06:12 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"

CBOP
C     !ROUTINE: admtlm_upxx
C     !INTERFACE:
      subroutine admtlm_upxx( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE admtlm_upxx
c     | ALLOW_ADMTLM needs to shift xx_... from optimcycle to
c     | optimcycle + 1 without change (iteration w.r.t. same state)
c     | Least interfering way to do this is to copy this file
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid

#ifdef ALLOW_ADMTLM
C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il

      logical equal
      logical doglobalread
      logical ladinit

      character*( 80)   fnametheta
      character*( 80)   fnamesalt
      character*( 80)   fnametr1
      character*( 80)   fnamediffkr
      character*( 80)   fnamekapgm
      character*( 80)   fnameefluxy
      character*( 80)   fnameefluxp
      character*( 80)   fnamebottomdrag
      character*( 80)   fnamesss
      character*( 80)   fnamesst

      _RL     fac
      _RL tmptest

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif

#ifdef ALLOW_SST0_CONTROL
c--   sst0.
      il=ilnblnk( xx_sst_file )
      write(fnamesst(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle-1
      call active_read_xy_loc ( fnamesst, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_sst_dummy )
      il=ilnblnk( xx_sst_file )
      write(fnamesst(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamesst, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_sst_dummy )
#endif

#ifdef ALLOW_SSS0_CONTROL
c--   sss0.
      il=ilnblnk( xx_sss_file )
      write(fnamesss(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle-1
      call active_read_xy_loc ( fnamesss, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_sss_dummy )
      il=ilnblnk( xx_sss_file )
      write(fnamesss(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamesss, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_sss_dummy )

#endif

#endif /* ALLOW_ADMTLM */

      return
      end

