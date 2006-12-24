
#include "COST_CPPOPTIONS.h"


      subroutine ecco_cost_init_barfiles( mythid )

c     ==================================================================
c     SUBROUTINE ecco_cost_init_barfiles
c     ==================================================================
c
c--   Initialise adjoint of monthly mean files calculated
c--   in cost_averagesfields (and their ad...).
c
c     started: heimbach@mit.edu 20-Mar-2002
c
c     ==================================================================
c     SUBROUTINE ecco_cost_cost_init_barfiles
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "optim.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer ilps, ils, ilt, irec

      character*(128) fnamepsbar
      character*(128) fnametbar
      character*(128) fnamesbar
      character*(128) fnameubar
      character*(128) fnamevbar
      character*(128) fnamewbar
      character*(128) fnametauxbar
      character*(128) fnametauybar
      character*(128) fnamehfluxbar
      character*(128) fnamesfluxbar
      character*(128) fnamesmrareabar
c
      character*(128) adfnamepsbar
      character*(128) adfnametbar
      character*(128) adfnamesbar
      character*(128) adfnameubar
      character*(128) adfnamevbar
      character*(128) adfnamewbar
      character*(128) adfnametauxbar
      character*(128) adfnametauybar
      character*(128) adfnamehfluxbar
      character*(128) adfnamesfluxbar
      character*(128) adfnamesmrareabar

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i =  imin,imax
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                tmpfld3d(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

cgg(  Also initialize the tmp fields used in ctrl_getobcs and cost_obcs.
#ifdef OBCS_CONTROL
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              tmpfldyz (j,k,bi,bj) = 0. _d 0
              tmpfldyz2(j,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do i =  imin,imax
              tmpfldxz (i,k,bi,bj) = 0. _d 0
              tmpfldxz2(i,k,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo
#endif
cgg)

#ifdef ALLOW_SSH_COST_CONTRIBUTION
c--   Save psbar on file.
      ilps=ilnblnk( psbarfile )
      write(fnamepsbar,'(2a,i10.10)') 
     &     psbarfile(1:ilps), '.', optimcycle
      write(adfnamepsbar,'(3a,i10.10)')
     &     yadprefix, psbarfile(1:ilps), '.', optimcycle

      do irec = 1, ndaysrec
c        call active_write_xy( fnamepsbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_psbar_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnamepsbar, tmpfld2d, irec,
     &                         optimcycle,mythid, xx_psbar_mean_dummy )
#endif
      enddo
#endif /* ALLOW_SSH_COST_CONTRIBUTION */

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDTCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--   Save tbar on file.
      ilt=ilnblnk( tbarfile )
      write(fnametbar,'(2a,i10.10)') 
     &     tbarfile(1:ilt), '.', optimcycle
      write(adfnametbar,'(3a,i10.10)') 
     &     yadprefix, tbarfile(1:ilt), '.', optimcycle

      do irec = 1,nmonsrec
c        call active_write_xyz( fnametbar, tmpfld3d, irec,
c     &                         optimcycle, mythid, xx_tbar_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xyz( adfnametbar, tmpfld3d, irec, 
     &                         optimcycle, mythid, xx_tbar_mean_dummy )
#endif
      enddo
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
c--   Save tbar on file.
      ilt=ilnblnk( tbarfile )
      write(fnametbar,'(2a,i10.10)') 
     &     tbarfile(1:ilt), '.', optimcycle
      write(adfnametbar,'(3a,i10.10)') 
     &     yadprefix, tbarfile(1:ilt), '.', optimcycle
      do irec = 1,nmonsrec
c        call active_write_xy( fnametbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_tbar_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnametbar, tmpfld2d, irec, 
     &                        optimcycle, mythid, xx_tbar_mean_dummy )
#endif
      enddo
#endif
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDSCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--   Save sbar.
      ils=ilnblnk( sbarfile )
      write(fnamesbar,'(2a,i10.10)') 
     &     sbarfile(1:ils), '.', optimcycle
      write(adfnamesbar,'(3a,i10.10)') 
     &     yadprefix, sbarfile(1:ils), '.', optimcycle

      do irec = 1,nmonsrec
c        call active_write_xyz( fnamesbar, tmpfld3d, irec,
c     &                         optimcycle, mythid, xx_sbar_mean_dummy)
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xyz( adfnamesbar, tmpfld3d, irec,
     &                         optimcycle, mythid, xx_sbar_mean_dummy)
#endif
      enddo
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
c--   Save sbar.
      ils=ilnblnk( sbarfile )
      write(fnamesbar,'(2a,i10.10)') 
     &     sbarfile(1:ils), '.', optimcycle
      write(adfnamesbar,'(3a,i10.10)') 
     &     yadprefix, sbarfile(1:ils), '.', optimcycle

      do irec = 1,nmonsrec
c        call active_write_xy( fnamesbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_sbar_mean_dummy)
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnamesbar, tmpfld2d, irec,
     &                        optimcycle, mythid, xx_sbar_mean_dummy)
#endif
      enddo
#endif
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
cph   There's a mismatch between the cost_drifer and the 
cph   cost_obcs usage of ubar, vbar.
cph   cost_obcs refers to monthly means, cost_drifer to total mean.
cph   Needs to be updated for cost_drifer.

c--   Save ubar and vbar.
      ils=ilnblnk( ubarfile )
      write(fnameubar,'(2a,i10.10)') 
     &     ubarfile(1:ils), '.', optimcycle
      write(fnamevbar,'(2a,i10.10)') 
     &     vbarfile(1:ils), '.', optimcycle
      write(adfnameubar,'(3a,i10.10)') 
     &     yadprefix, ubarfile(1:ils), '.', optimcycle
      write(adfnamevbar,'(3a,i10.10)') 
     &     yadprefix, vbarfile(1:ils), '.', optimcycle

      do irec = 1,nmonsrec
c        call active_write_xyz( fnameubar, tmpfld3d, irec,
c     &                         optimcycle, mythid, xx_ubar_mean_dummy)
c        call active_write_xyz( fnamevbar, tmpfld3d, irec,
c     &                         optimcycle, mythid, xx_vbar_mean_dummy)
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xyz( adfnameubar, tmpfld3d, irec,
     &                         optimcycle, mythid, xx_ubar_mean_dummy)
        call active_write_xyz( adfnamevbar, tmpfld3d, irec,
     &                         optimcycle, mythid, xx_vbar_mean_dummy)
#endif
      enddo
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
c--   Save wbar
      ils=ilnblnk( wbarfile )
      write(fnamewbar,'(2a,i10.10)') 
     &     wbarfile(1:ils), '.', optimcycle
      write(adfnamewbar,'(3a,i10.10)') 
     &     yadprefix, wbarfile(1:ils), '.', optimcycle

      do irec = 1,nmonsrec
c        call active_write_xyz( fnamewbar, tmpfld3d, irec,
c     &                         optimcycle, mythid, xx_wbar_mean_dummy)
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xyz( adfnamewbar, tmpfld3d, irec,
     &                         optimcycle, mythid, xx_wbar_mean_dummy)
#endif
      enddo
#endif

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c--   Save tauxbar, tauybar on file.
      ilps=ilnblnk( tauxbarfile )
      write(fnametauxbar,'(2a,i10.10)') 
     &     tauxbarfile(1:ilps), '.', optimcycle
      write(adfnametauxbar,'(3a,i10.10)')
     &     yadprefix, tauxbarfile(1:ilps), '.', optimcycle
      ilps=ilnblnk( tauybarfile )
      write(fnametauybar,'(2a,i10.10)') 
     &     tauybarfile(1:ilps), '.', optimcycle
      write(adfnametauybar,'(3a,i10.10)')
     &     yadprefix, tauybarfile(1:ilps), '.', optimcycle

      do irec = 1, nmonsrec
c        call active_write_xy( fnametauxbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_taux_mean_dummy )
c        call active_write_xy( fnametauybar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_tauy_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnametauxbar, tmpfld2d, irec,
     &                         optimcycle,mythid, xx_taux_mean_dummy )
        call active_write_xy( adfnametauybar, tmpfld2d, irec,
     &                         optimcycle,mythid, xx_tauy_mean_dummy )
#endif
      enddo
#endif

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
c--   Save hfluxbar on file.
      ilps=ilnblnk( hfluxbarfile )
      write(fnamehfluxbar,'(2a,i10.10)') 
     &     hfluxbarfile(1:ilps), '.', optimcycle
      write(adfnamehfluxbar,'(3a,i10.10)')
     &     yadprefix, hfluxbarfile(1:ilps), '.', optimcycle

      do irec = 1, 1
c        call active_write_xy( fnamehfluxbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_hflux_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnamehfluxbar, tmpfld2d, irec,
     &                         optimcycle,mythid, xx_hflux_mean_dummy )
#endif
      enddo
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
c--   Save sfluxbar on file.
      ilps=ilnblnk( sfluxbarfile )
      write(fnamesfluxbar,'(2a,i10.10)') 
     &     sfluxbarfile(1:ilps), '.', optimcycle
      write(adfnamesfluxbar,'(3a,i10.10)')
     &     yadprefix, sfluxbarfile(1:ilps), '.', optimcycle

      do irec = 1, nyearsrec
c        call active_write_xy( fnamesfluxbar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_sflux_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnamesfluxbar, tmpfld2d, irec,
     &                         optimcycle,mythid, xx_sflux_mean_dummy )
#endif
      enddo
#endif

#ifdef ALLOW_SEAICE

#ifdef ALLOW_SEAICE_COST_SMR_AREA
      ilps=ilnblnk( smrareabarfile )
      write(fnamesmrareabar,'(2a,i10.10)') 
     &     smrareabarfile(1:ilps), '.', optimcycle
      write(adfnamesmrareabar,'(3a,i10.10)')
     &     yadprefix, smrareabarfile(1:ilps), '.', optimcycle

      do irec = 1, ndaysrec
c        call active_write_xy( fnamesmrareabar, tmpfld2d, irec,
c     &                        optimcycle, mythid, xx_smrareabar_mean_dummy )
#ifndef ALLOW_TANGENTLINEAR_RUN
        call active_write_xy( adfnamesmrareabar, tmpfld2d, irec,
     &        optimcycle,mythid, xx_smrareabar_mean_dummy )
#endif
      enddo
#endif /* ALLOW_SEAICE_COST_SMR_AREA */

#endif /* ALLOW_SEAICE */

#endif /* ALLOW_COST */

      return
      end

