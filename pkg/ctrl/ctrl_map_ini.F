C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini.F,v 1.13 2004/09/17 23:02:01 heimbach Exp $

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

#ifdef ALLOW_THETA0_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_theta_file )
      write(fnametheta(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnametheta, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_theta_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_ECCO
               IF (abs(tmpfld3d(i,j,k,bi,bj)).gt.
     $          2.0/sqrt(wtheta(k,bi,bj)))
     $          tmpfld3d(i,j,k,bi,bj)=
     $          sign(2.0/sqrt(wtheta(k,bi,bj)),tmpfld3d(i,j,k,bi,bj))
#endif
                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
                if(theta(i,j,k,bi,bj).lt.-2.0) 
     &               theta(i,j,k,bi,bj)= -2.0 
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
      call active_read_xyz_loc( fnamesalt, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_salt_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_ECCO
               IF (abs(tmpfld3d(i,j,k,bi,bj)).gt.
     $          2.0/sqrt(wsalt(k,bi,bj)))
     $          tmpfld3d(i,j,k,bi,bj)=
     $          sign(2.0/sqrt(wsalt(k,bi,bj)),tmpfld3d(i,j,k,bi,bj))
#endif
                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)

              enddo
            enddo
          enddo
       enddo
      enddO
#endif

#ifdef ALLOW_TR10_CONTROL
#ifdef ALLOW_PTRACERS
c--   Temperature field.
      il=ilnblnk( xx_tr1_file )
      write(fnametr1(1:80),'(2a,i10.10)')
     &     xx_tr1_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnametr1, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_tr1_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                ptracer(i,j,k,bi,bj,1) = ptracer(i,j,k,bi,bj,1) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif
#endif

#ifdef ALLOW_DIFFKR_CONTROL
c--   diffkr.
      il=ilnblnk( xx_diffkr_file )
      write(fnamediffkr(1:80),'(2a,i10.10)')
     &     xx_diffkr_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnamediffkr, tmpfld3d, 1,
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
      call active_read_xyz_loc( fnamekapgm, tmpfld3d, 1,
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

#ifdef ALLOW_EFLUXY0_CONTROL
c--   y-component EP-flux field.
      il=ilnblnk( xx_efluxy_file )
      write(fnameefluxy(1:80),'(2a,i10.10)')
     &     xx_efluxy_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnameefluxy, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_efluxy_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                EfluxY(i,j,k,bi,bj) = EfluxY(i,j,k,bi,bj)
     &                                - fac*tmpfld3d(i,j,k,bi,bj)
     &                                  *maskS(i,j,k,bi,bj)
cph                EfluxY(i,j,k,bi,bj) = EfluxY(i,j,k,bi,bj)
cph     &                                - rSphere*cosFacU(J,bi,bj)
cph     &                                  *fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_EFLUXP0_CONTROL
c--   p-component EP-flux field.
      il=ilnblnk( xx_efluxp_file )
      write(fnameefluxp(1:80),'(2a,i10.10)')
     &     xx_efluxp_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnameefluxp, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_efluxp_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                EfluxP(i,j,k,bi,bj) = EfluxP(i,j,k,bi,bj)
     &                                + fCori(i,j,bi,bj)
     &                                  *fac*tmpfld3d(i,j,k,bi,bj)
     &                                  *hFacV(i,j,k,bi,bj)
cph                EfluxP(i,j,k,bi,bj) = EfluxP(i,j,k,bi,bj)
cph     &                                + fCori(i,j,bi,bj)
cph     &                                  *rSphere*cosFacU(J,bi,bj)
cph     &                                  *fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
c--   bottom drag
      il=ilnblnk( xx_bottomdrag_file )
      write(fnamebottomdrag(1:80),'(2a,i10.10)')
     &     xx_bottomdrag_file(1:il),'.',optimcycle
      call active_read_xy_loc ( fnamebottomdrag, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_bottomdrag_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              bottomdragfld(i,j,bi,bj) = bottomdragfld(i,j,bi,bj) 
     &                                   + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif


c--   Update the tile edges.

#ifdef ALLOW_THETA0_CONTROL
      _EXCH_XYZ_R8( theta, mythid )
#endif
#ifdef ALLOW_SALT0_CONTROL
      _EXCH_XYZ_R8(  salt, mythid )
#endif
#ifdef ALLOW_TR10_CONTROL
#ifdef ALLOW_PTRACERS
      _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,1),myThid)
#endif
#endif
#ifdef ALLOW_DIFFKR_CONTROL
      _EXCH_XYZ_R8( diffkr, mythid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      _EXCH_XYZ_R8( kapgm, mythid)
#endif
#ifdef ALLOW_EFLUXY0_CONTROL
      _EXCH_XYZ_R8( EfluxY, mythid )
#endif
#ifdef ALLOW_EFLUXP0_CONTROL
      _EXCH_XYZ_R8( EfluxP, mythid )
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      _EXCH_XY_R8( bottomdragfld, mythid )
#endif


      return
      end

