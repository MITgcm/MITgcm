C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini.F,v 1.5 2001/09/28 15:15:55 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"

CBOP
C     !ROUTINE: ctrl_map_ini
C     !INTERFACE:
      subroutine ctrl_map_ini( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_map_ini
c     | Add the temperature, salinity, and diffusivity parts of the 
c     | control vector to the model state and update the tile halos. 
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "TR1.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid

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

      _RL     fac

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      doglobalread = .false.
      ladinit      = .false.

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif

#ifdef ALLOW_THETA0_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_theta_file )
      write(fnametheta(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle
      call active_read_xyz( fnametheta, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_theta_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
cph                gtNm1(i,j,k,bi,bj) = gtNm1(i,j,k,bi,bj) +
cph     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_SALT0_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_salt_file )
      write(fnamesalt(1:80),'(2a,i10.10)')
     &     xx_salt_file(1:il),'.',optimcycle
      call active_read_xyz( fnamesalt, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_salt_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
cph                gsNm1(i,j,k,bi,bj) = gsNm1(i,j,k,bi,bj) +
cph     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_TR10_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_tr1_file )
      write(fnametr1(1:80),'(2a,i10.10)')
     &     xx_tr1_file(1:il),'.',optimcycle
      call active_read_xyz( fnametr1, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_tr1_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                tr1(i,j,k,bi,bj) = tr1(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
cph                gtr1Nm1(i,j,k,bi,bj) = gtr1Nm1(i,j,k,bi,bj) +
cph     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_DIFFKR_CONTROL
c--   diffkr.
      il=ilnblnk( xx_diffkr_file )
      write(fnamediffkr(1:80),'(2a,i10.10)')
     &     xx_diffkr_file(1:il),'.',optimcycle
      call active_read_xyz( fnamediffkr, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_diffkr_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                diffkr(i,j,k,bi,bj) = diffkr(i,j,k,bi,bj) +
     &                                tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_KAPGM_CONTROL
c--   kapgm.
      il=ilnblnk( xx_kapgm_file )
      write(fnamekapgm(1:80),'(2a,i10.10)')
     &     xx_kapgm_file(1:il),'.',optimcycle
      call active_read_xyz( fnamekapgm, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_kapgm_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                kapgm(i,j,k,bi,bj) = kapgm(i,j,k,bi,bj) +
     &                               tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif


c--   Update the tile edges.

#ifdef ALLOW_THETA0_CONTROL
      _EXCH_XYZ_R8( theta, mythid )
      _EXCH_XYZ_R8( gtNm1, mythid )
#endif
#ifdef ALLOW_SALT0_CONTROL
      _EXCH_XYZ_R8(  salt, mythid )
      _EXCH_XYZ_R8( gsNm1, mythid )
#endif
#ifdef ALLOW_TR10_CONTROL
      _EXCH_XYZ_R8(     tr1, mythid )
      _EXCH_XYZ_R8( gTr1Nm1, mythid )
#endif
#ifdef ALLOW_DIFFKR_CONTROL
      _EXCH_XYZ_R8( diffkr, mythid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      _EXCH_XYZ_R8( kapgm, mythid)
#endif


      return
      end

