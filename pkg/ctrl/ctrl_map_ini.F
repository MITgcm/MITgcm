C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini.F,v 1.24 2007/06/21 04:06:21 heimbach Exp $

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
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
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

      character*( 80)   fnamegeneric

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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
#ifdef ALLOW_AUTODIFF_OPENAD
                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                               fac*xx_theta(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
#else
                theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
#endif
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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_salt_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
#ifdef ALLOW_AUTODIFF_OPENAD
                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*xx_salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
#else
                salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
#endif

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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_tr1_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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

#ifdef ALLOW_SST0_CONTROL
c--   sst0.
      il=ilnblnk( xx_sst_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_sst_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
cph              sst(i,j,bi,bj) = sst(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
              theta(i,j,1,bi,bj) = theta(i,j,1,bi,bj) 
     &                             + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SSS0_CONTROL
c--   sss0.
      il=ilnblnk( xx_sss_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_sss_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
cph              sss(i,j,bi,bj) = sss(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
              salt(i,j,1,bi,bj) = salt(i,j,1,bi,bj)
     &                             + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_DIFFKR_CONTROL
c--   diffkr.
      il=ilnblnk( xx_diffkr_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_diffkr_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_kapgm_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_efluxy_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_efluxp_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
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
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_bottomdrag_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
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

#ifdef ALLOW_EDTAUX_CONTROL
c-- zonal eddy stress : edtaux
      il=ilnblnk( xx_edtaux_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_edtaux_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_edtaux_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                eddyTauX(i,j,k,bi,bj) = eddyTauX(i,j,k,bi,bj) +
     &            fCori(i,j,bi,bj)*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_EDTAUY_CONTROL
c-- meridional eddy stress : edtauy
      il=ilnblnk( xx_edtauy_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_edtauy_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_edtauy_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                eddyTauY(i,j,k,bi,bj) = eddyTauY(i,j,k,bi,bj) +
     &            fCoriG(i,j,bi,bj)*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_UVEL0_CONTROL
c-- initial zonal velocity
      il=ilnblnk( xx_uvel_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_uvel_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_uvel_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_AUTODIFF_OPENAD
                uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) +
     &                                  fac*xx_uvel(i,j,k,bi,bj)
#else
                uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) +
     &                                  fac*tmpfld3d(i,j,k,bi,bj)
#endif
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_VVEL0_CONTROL
c-- initial merid. velocity
      il=ilnblnk( xx_vvel_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_vvel_file(1:il),'.',optimcycle
      call active_read_xyz( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_vvel_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_AUTODIFF_OPENAD
                vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj) +
     &                                  fac*xx_vvel(i,j,k,bi,bj)
#else
                vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj) +
     &                                  fac*tmpfld3d(i,j,k,bi,bj)
#endif
              enddo
            enddo
          enddo
       enddo
      enddo
#endif

#ifdef ALLOW_ETAN0_CONTROL
c--   initial Eta.
      il=ilnblnk( xx_etan_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_etan_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_etan_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
#ifdef ALLOW_AUTODIFF_OPENAD
              etaN(i,j,bi,bj) = etaN(i,j,bi,bj) +
     &                              fac*xx_etan(i,j,bi,bj)
#else
              etaN(i,j,bi,bj) = etaN(i,j,bi,bj) + 
     &                              fac*tmpfld2d(i,j,bi,bj)
#endif
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_RELAXSST_CONTROL
c--   SST relaxation coefficient.
      il=ilnblnk( xx_relaxsst_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_relaxsst_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_relaxsst_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              lambdaThetaClimRelax(i,j,bi,bj) = 
     &              lambdaThetaClimRelax(i,j,bi,bj) 
     &              + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
c--   SSS relaxation coefficient.
      il=ilnblnk( xx_relaxsss_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_relaxsss_file(1:il),'.',optimcycle
      call active_read_xy ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_relaxsss_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              lambdaSaltClimRelax(i,j,bi,bj) = 
     &              lambdaSaltClimRelax(i,j,bi,bj) 
     &              + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SEAICE
      call seaice_ctrl_map_ini( mythid )
#endif

c--   Update the tile edges.

#if (defined (ALLOW_THETA0_CONTROL) || defined (ALLOW_SST0_CONTROL))
      _EXCH_XYZ_R8( theta, mythid )
#endif
#if (defined (ALLOW_SALT0_CONTROL) || defined (ALLOW_SSS0_CONTROL))
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

#if (defined (ALLOW_EDTAUX_CONTROL) && defined (ALLOW_EDTAUY_CONTROL))
       CALL EXCH_UV_XYZ_RS(eddyTauX,eddyTauY,.TRUE.,myThid)
#elif (defined (ALLOW_EDTAUX_CONTROL) || defined (ALLOW_EDTAUY_CONTROL))
       STOP 'ctrl_map_forcing: need BOTH ALLOW_EDTAU[X,Y]_CONTROL'
#endif

#ifdef ALLOW_UVEL0_CONTROL
      _EXCH_XYZ_R8( uVel, mythid)
#endif

#ifdef ALLOW_VVEL0_CONTROL
      _EXCH_XYZ_R8( vVel, mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
      _EXCH_XY_R8( etaN, mythid )
#endif

#ifdef ALLOW_RELAXSST_CONTROL
      _EXCH_XY_R4( lambdaThetaClimRelax, mythid )
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
      _EXCH_XY_R4( lambdaThetaClimRelax, mythid )
#endif

      return
      end

