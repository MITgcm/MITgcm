C $Header: /u/gcmpack/MITgcm/pkg/admtlm/Attic/admtlm_upxx.F,v 1.2 2005/04/29 10:36:45 heimbach Exp $

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
#ifdef ALLOW_ADMTLM
# include "adcost.h"
# include "g_cost.h"
# include "adcommon.h"
# include "g_common.h"
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

      character*( 80)   fnamegen

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

cph A quick hack to make the full state I/O work quickly
#ifdef ALLOW_ADMTLM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          DO k=1,Nr
            g_theta(i,j,k,bi,bj) = g_objf_state_final(i,j,bi,bj,k)
            g_salt(i,j,k,bi,bj) = g_objf_state_final(i,j,bi,bj,k+Nr)
            g_uvel(i,j,k,bi,bj) = g_objf_state_final(i,j,bi,bj,k+2*Nr)
            g_vvel(i,j,k,bi,bj) = g_objf_state_final(i,j,bi,bj,k+3*Nr)
          END DO
           g_etan(i,j,bi,bj) = g_objf_state_final(i,j,bi,bj,4*Nr+1)
         END DO
        END DO
       END DO
      END DO

      il=ilnblnk( xx_theta_file )
      write(fnamegen(1:80),'(3a,i10.10)')
     &     'g_',xx_theta_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, g_theta, 1,
     &                      optimcycle,
     &                      mythid, xx_theta_dummy )

      il=ilnblnk( xx_salt_file )
      write(fnamegen(1:80),'(3a,i10.10)')
     &     'g_',xx_salt_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, g_salt, 1,
     &                      optimcycle,
     &                      mythid, xx_salt_dummy )

      il=ilnblnk( xx_uvel_file )
      write(fnamegen(1:80),'(3a,i10.10)')
     &     'g_',xx_uvel_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, g_uvel, 1,
     &                      optimcycle,
     &                      mythid, xx_uvel_dummy )

      il=ilnblnk( xx_vvel_file )
      write(fnamegen(1:80),'(3a,i10.10)')
     &     'g_',xx_vvel_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, g_vvel, 1,
     &                      optimcycle,
     &                      mythid, xx_vvel_dummy )

      il=ilnblnk( xx_etan_file )
      write(fnamegen(1:80),'(3a,i10.10)')
     &     'g_',xx_etan_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamegen, g_etan, 1,
     &                      optimcycle,
     &                      mythid, xx_etan_dummy )

#endif

#ifdef ALLOW_SST0_CONTROL
c--   sst0.
      il=ilnblnk( xx_sst_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle-1
      call active_read_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_sst_dummy )
      il=ilnblnk( xx_sst_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_sst_dummy )
#endif

#ifdef ALLOW_SSS0_CONTROL
c--   sss0.
      il=ilnblnk( xx_sss_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle-1
      call active_read_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_sss_dummy )
      il=ilnblnk( xx_sss_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_sss_dummy )

#endif

#ifdef ALLOW_THETA0_CONTROL
      il=ilnblnk( xx_theta_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle-1
      call active_read_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_theta_dummy )
      il=ilnblnk( xx_theta_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_theta_dummy )
#endif

#ifdef ALLOW_SALT0_CONTROL
      il=ilnblnk( xx_salt_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_salt_file(1:il),'.',optimcycle-1
      call active_read_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_salt_dummy )
      il=ilnblnk( xx_salt_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_salt_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_salt_dummy )
#endif

#ifdef ALLOW_UVEL0_CONTROL
      il=ilnblnk( xx_uvel_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_uvel_file(1:il),'.',optimcycle-1
      call active_read_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_uvel_dummy )
      il=ilnblnk( xx_uvel_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_uvel_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_uvel_dummy )
#endif

#ifdef ALLOW_VVEL0_CONTROL
      il=ilnblnk( xx_vvel_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_vvel_file(1:il),'.',optimcycle-1
      call active_read_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_vvel_dummy )
      il=ilnblnk( xx_vvel_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_vvel_file(1:il),'.',optimcycle
      call active_write_xyz_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_vvel_dummy )
#endif

#ifdef ALLOW_ETAN0_CONTROL
c--   etan0.
      il=ilnblnk( xx_etan_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_etan_file(1:il),'.',optimcycle-1
      call active_read_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle-1,
     &                      mythid, xx_etan_dummy )
      il=ilnblnk( xx_etan_file )
      write(fnamegen(1:80),'(2a,i10.10)')
     &     xx_etan_file(1:il),'.',optimcycle
      call active_write_xy_loc ( fnamegen, tmpfld2d, 1,
     &                      optimcycle,
     &                      mythid, xx_etan_dummy )

#endif

#endif /* ALLOW_ADMTLM */

      return
      end

