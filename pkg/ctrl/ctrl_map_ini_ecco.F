C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_ecco.F,v 1.5 2006/05/11 21:43:12 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_map_ini_ecco( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================
c
c     o Add the temperature and salinity parts of the control vector to
c       the model state and update the tile edges. The control vector is
c       defined in the header file "ctrl.h".
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_ECCO
#include "ecco_cost.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_ECCO
c     == local variables ==

      _RL     fac
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il

      logical doglobalread
      logical ladinit

      character*( 80) fnamegeneric
      character*(MAX_LEN_FNAM) suff

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = 0
          ikey = (act3 + 1) + act4*max3
#endif /* ALLOW_AUTODIFF_TAMC */

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

      fac = 1. _d 0

#ifdef ALLOW_THETA0_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_theta_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_theta_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_theta_dummy )
cph(
       print *, 'ph-ctrl theta0 a ',
     &    tmpfld3d(15,15,1,1,1)
cph)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                 IF (abs(tmpfld3d(i,j,k,bi,bj)).gt.
     &                2.0/sqrt(wtheta(k,bi,bj))) 
     &                tmpfld3d(i,j,k,bi,bj)=
     &                sign(2.0/sqrt(wtheta(k,bi,bj)),
     &                tmpfld3d(i,j,k,bi,bj))
              enddo
            enddo
          enddo
       enddo
      enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = tapelev_init, key = ikey
#endif

c                 if(abs(xc(i,j,bi,bj)-123) .gt. 2 .or. 
c     $                abs(yc(i,j,bi,bj)-84) .gt. 2 )
c                if((abs(xc(i,j,bi,bj)-125) .gt. 8 .or. 
c     $                abs(yc(i,j,bi,bj)-5.5) .gt. 9 )
c     $                .and. (k.le.10.or.
c     $                (abs(xc(i,j,bi,bj)-164) .gt. 16 .or. 
c     $                abs(yc(i,j,bi,bj)+60.5) .gt. 6)  .and.
c     $                (abs(xc(i,j,bi,bj)-359).gt.2 .or. 
c     $                abs(yc(i,j,bi,bj)-35.5).gt.2) ) .and.
c     $                (k.le.17.or.
c     $                (abs(xc(i,j,bi,bj)-203) .gt. 26 .or. 
c     $                abs(yc(i,j,bi,bj)+58.5) .gt. 6)
c     $                ))

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                 theta(i,j,k,bi,bj) = theta(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
                if(theta(i,j,k,bi,bj).lt.-2.0) 
     &               theta(i,j,k,bi,bj)= -2.0  
              enddo
            enddo
          enddo
       enddo
      enddo
cph(
       print *, 'ph-ctrl theta0 b ',
     &    tmpfld3d(15,15,1,1,1)
cph)

#endif

#ifdef ALLOW_SALT0_CONTROL
c--   Temperature field.
      il=ilnblnk( xx_salt_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_salt_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnamegeneric, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_salt_dummy )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                 IF (abs(tmpfld3d(i,j,k,bi,bj)).gt.
     &                2.0/sqrt(wsalt(k,bi,bj))) 
     &                tmpfld3d(i,j,k,bi,bj)=
     &                sign(2.0/sqrt(wsalt(k,bi,bj)),
     &                tmpfld3d(i,j,k,bi,bj))
c                 if(abs(xc(i,j,bi,bj)-123) .gt. 2 .or. 
c     $                abs(yc(i,j,bi,bj)-84) .gt. 2 )
c                if((abs(xc(i,j,bi,bj)-125) .gt. 8 .or. 
c     $                abs(yc(i,j,bi,bj)-5.5) .gt. 9)
c     $                .and. (k.le.10.or.
c     $                (abs(xc(i,j,bi,bj)-164) .gt. 16 .or. 
c     $                abs(yc(i,j,bi,bj)+60.5) .gt. 6)  )
c     $                 .and. (k.le.17.or.
c     $                (abs(xc(i,j,bi,bj)-203) .gt. 26 .or. 
c     $                abs(yc(i,j,bi,bj)+58.5) .gt. 6)
c     $                ) )

              enddo
            enddo
          enddo
       enddo
      enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE salt = tapelev_init, key = ikey
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                 salt(i,j,k,bi,bj) = salt(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
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
cph(
      WRITE(suff,'(I10.10)') 0
      CALL WRITE_FLD_XYZ_RS('GM_edTauX_pXX.',suff,eddyTauX,0,myThid)
cph)
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
cph(
      WRITE(suff,'(I10.10)') 0
      CALL WRITE_FLD_XYZ_RS('GM_edTauY_pXX.',suff,eddyTauY,0,myThid)
cph)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
c--   diffkr.
      il=ilnblnk( xx_diffkr_file )
      write(fnamegeneric(1:80),'(2a,i10.10)')
     &     xx_diffkr_file(1:il),'.',optimcycle
      call active_read_xyz_loc( fnamegeneric, tmpfld3d, 1,
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
      call active_read_xyz_loc( fnamegeneric, tmpfld3d, 1,
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
                uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) +
     &                              tmpfld3d(i,j,k,bi,bj)
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
                vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj) +
     &                              tmpfld3d(i,j,k,bi,bj)
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
      call active_read_xy_loc ( fnamegeneric, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_etan_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              etaN(i,j,bi,bj) = etaN(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
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
      call active_read_xy_loc ( fnamegeneric, tmpfld2d, 1,
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
      call active_read_xy_loc ( fnamegeneric, tmpfld2d, 1,
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

c--   Update the tile edges.

#ifdef ALLOW_THETA0_CONTROL
      _EXCH_XYZ_R8( theta, mythid )
#endif
#ifdef ALLOW_SALT0_CONTROL
      _EXCH_XYZ_R8(  salt, mythid )
#endif
#if (defined (ALLOW_EDTAUX_CONTROL) && defined (ALLOW_EDTAUY_CONTROL))
       CALL EXCH_UV_XYZ_RS(eddyTauX,eddyTauY,.TRUE.,myThid)
#elif (defined (ALLOW_EDTAUX_CONTROL) || defined (ALLOW_EDTAUY_CONTROL))
       STOP 'ctrl_map_forcing: need BOTH ALLOW_EDTAU[X,Y]_CONTROL'
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      _EXCH_XYZ_R8( diffkr, mythid)
#endif

#ifdef ALLOW_KAPGM_CONTROL
      _EXCH_XYZ_R8( kapgm, mythid)
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

#endif

      return
      end

