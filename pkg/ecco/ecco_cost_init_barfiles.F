#include "ECCO_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

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
#include "PARAMS.h"

#ifdef ALLOW_ECCO
# ifdef ECCO_CTRL_DEPRECATED
#  include "ecco_cost.h"
# else
#  include "ECCO_SIZE.h"
#  include "ECCO.h"
# endif
#endif
#ifdef ALLOW_CTRL
# include "optim.h"
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer ilt, irec

#ifdef ALLOW_GENCOST_CONTRIBUTION
      character*(128) fname_gencostbar
      character*(128) adfname_gencostbar
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#if (defined ALLOW_SEAICE) && (defined ALLOW_SEAICE_COST_SMR_AREA)
      integer ilps
      character*(128) fnamesmrareabar
      character*(128) fnamesmrsstbar
      character*(128) fnamesmrsssbar
      character*(128) adfnamesmrareabar
      character*(128) adfnamesmrsstbar
      character*(128) adfnamesmrsssbar
#elif ( defined ECCO_CTRL_DEPRECATED )
      integer ilps
#endif

      _RL tmpfld2d (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tmpfld3d (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

#ifdef ECCO_CTRL_DEPRECATED
      integer ils
      character*(128) fnamepsbar
      character*(128) fnametbar
      character*(128) fnamesbar
      character*(128) fnamesstbar
      character*(128) fnameubar
      character*(128) fnamevbar
      character*(128) fnamewbar
      character*(128) fnametauxbar
      character*(128) fnametauybar
      character*(128) fnamehfluxmeanbar
      character*(128) fnamesfluxmeanbar
      character*(128) fnamebpbar
      character*(128) fnameiestaubar
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(128) fnamesigmaRbar
#endif
      character*(128) adfnamepsbar
      character*(128) adfnametbar
      character*(128) adfnamesbar
      character*(128) adfnamesstbar
      character*(128) adfnameubar
      character*(128) adfnamevbar
      character*(128) adfnamewbar
      character*(128) adfnametauxbar
      character*(128) adfnametauybar
      character*(128) adfnamehfluxmeanbar
      character*(128) adfnamesfluxmeanbar
      character*(128) adfnamebpbar
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(128) adfnamesigmaRbar
#endif
      character*(128) adfnameiestaubar
#endif /* ECCO_CTRL_DEPRECATED */
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

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_SSH_COST_CONTRIBUTION
      IF (using_cost_altim) THEN
c--   Save psbar on file.
      ilps=ilnblnk( psbarfile )
      write(fnamepsbar,'(2a,i10.10)')
     &     psbarfile(1:ilps), '.', eccoiter
      write(adfnamepsbar,'(3a,i10.10)')
     &     yadprefix, psbarfile(1:ilps), '.', eccoiter

      do irec = 1, ndaysrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamepsbar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_psbar_mean_dummy )
#endif
      enddo
      ENDIF
#endif /* ALLOW_SSH_COST_CONTRIBUTION */

#ifdef ALLOW_BP_COST_CONTRIBUTION
      IF (using_cost_bp) THEN
c--   Save bpbar on file.
      ilps=ilnblnk( bpbarfile )
      write(fnamebpbar,'(2a,i10.10)')
     &     bpbarfile(1:ilps), '.', eccoiter
      write(adfnamebpbar,'(3a,i10.10)')
     &     yadprefix, bpbarfile(1:ilps), '.', eccoiter

      do irec = 1, nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamebpbar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_bpbar_mean_dummy )
#endif
      enddo
      ENDIF !      IF (using_cost_bp) THEN
#endif

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
c--   Save sigmaRbar on file.
      ilt=ilnblnk( sigmaRbarfile )
      write(fnamesigmaRbar,'(2a,i10.10)')
     &     sigmaRbarfile(1:ilt), '.', eccoiter
      write(adfnamesigmaRbar,'(3a,i10.10)')
     &     yadprefix, sigmaRbarfile(1:ilt), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xyz( adfnamesigmaRbar, tmpfld3d, irec,
     &              eccoiter, mythid, xx_sigmaRbar_mean_dummy )
#endif
      enddo
#endif

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDTCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--   Save tbar on file.
      ilt=ilnblnk( tbarfile )
      write(fnametbar,'(2a,i10.10)')
     &     tbarfile(1:ilt), '.', eccoiter
      write(adfnametbar,'(3a,i10.10)')
     &     yadprefix, tbarfile(1:ilt), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xyz( adfnametbar, tmpfld3d, irec,
     &                         eccoiter, mythid, xx_tbar_mean_dummy )
#endif
      enddo
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
c--   Save tbar on file.
      ilt=ilnblnk( tbarfile )
      write(fnametbar,'(2a,i10.10)')
     &     tbarfile(1:ilt), '.', eccoiter
      write(adfnametbar,'(3a,i10.10)')
     &     yadprefix, tbarfile(1:ilt), '.', eccoiter
      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnametbar, tmpfld2d, irec,
     &                        eccoiter, mythid, xx_tbar_mean_dummy )
#endif
      enddo
#endif
#endif

#ifdef ALLOW_DAILYSST_COST_CONTRIBUTION
      IF (using_cost_sst) THEN
cph#ifdef ALLOW_SEAICE_COST_AREASST
c--   Save tbar on file.
      ilt=ilnblnk( sstbarfile )
      write(fnamesstbar,'(2a,i10.10)')
     &     sstbarfile(1:ilt), '.', eccoiter
      write(adfnamesstbar,'(3a,i10.10)')
     &     yadprefix, sstbarfile(1:ilt), '.', eccoiter
      do irec = 1,ndaysrec
#ifdef ALLOW_ADJOINT_RUN
         call active_write_xy( adfnamesstbar, tmpfld2d, irec,
     &                        eccoiter, mythid, xx_sstbar_mean_dummy )
#endif
      enddo
      ENDIF !      IF (using_cost_sst) THEN
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDSCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--   Save sbar.
      ils=ilnblnk( sbarfile )
      write(fnamesbar,'(2a,i10.10)')
     &     sbarfile(1:ils), '.', eccoiter
      write(adfnamesbar,'(3a,i10.10)')
     &     yadprefix, sbarfile(1:ils), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xyz( adfnamesbar, tmpfld3d, irec,
     &                         eccoiter, mythid, xx_sbar_mean_dummy)
#endif
      enddo
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
c--   Save sbar.
      ils=ilnblnk( sbarfile )
      write(fnamesbar,'(2a,i10.10)')
     &     sbarfile(1:ils), '.', eccoiter
      write(adfnamesbar,'(3a,i10.10)')
     &     yadprefix, sbarfile(1:ils), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamesbar, tmpfld2d, irec,
     &                        eccoiter, mythid, xx_sbar_mean_dummy)
#endif
      enddo
#endif
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
cph   There is a mismatch between the cost_drifer and the
cph   cost_obcs usage of ubar, vbar.
cph   cost_obcs refers to monthly means, cost_drifer to total mean.
cph   Needs to be updated for cost_drifer.

c--   Save ubar and vbar.
      ils=ilnblnk( ubarfile )
      write(fnameubar,'(2a,i10.10)')
     &     ubarfile(1:ils), '.', eccoiter
      write(fnamevbar,'(2a,i10.10)')
     &     vbarfile(1:ils), '.', eccoiter
      write(adfnameubar,'(3a,i10.10)')
     &     yadprefix, ubarfile(1:ils), '.', eccoiter
      write(adfnamevbar,'(3a,i10.10)')
     &     yadprefix, vbarfile(1:ils), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xyz( adfnameubar, tmpfld3d, irec,
     &                         eccoiter, mythid, xx_ubar_mean_dummy)
        call active_write_xyz( adfnamevbar, tmpfld3d, irec,
     &                         eccoiter, mythid, xx_vbar_mean_dummy)
#endif
      enddo
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
c--   Save wbar
      ils=ilnblnk( wbarfile )
      write(fnamewbar,'(2a,i10.10)')
     &     wbarfile(1:ils), '.', eccoiter
      write(adfnamewbar,'(3a,i10.10)')
     &     yadprefix, wbarfile(1:ils), '.', eccoiter

      do irec = 1,nmonsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xyz( adfnamewbar, tmpfld3d, irec,
     &                         eccoiter, mythid, xx_wbar_mean_dummy)
#endif
      enddo
#endif

      IF (using_cost_scat) THEN
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
c--   Save tauxbar, tauybar on file.
      ilps=ilnblnk( tauxbarfile )
      write(fnametauxbar,'(2a,i10.10)')
     &     tauxbarfile(1:ilps), '.', eccoiter
      write(adfnametauxbar,'(3a,i10.10)')
     &     yadprefix, tauxbarfile(1:ilps), '.', eccoiter
      ilps=ilnblnk( tauybarfile )
      write(fnametauybar,'(2a,i10.10)')
     &     tauybarfile(1:ilps), '.', eccoiter
      write(adfnametauybar,'(3a,i10.10)')
     &     yadprefix, tauybarfile(1:ilps), '.', eccoiter

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
      do irec = 1, nmonsrec
#else
      do irec = 1, ndaysrec
#endif
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnametauxbar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_taux_mean_dummy )
        call active_write_xy( adfnametauybar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_tauy_mean_dummy )
#endif
      enddo
#endif
      ENDIF !      IF (using_cost_scat) THEN

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
c--   Save hfluxmeanbar on file.
      ilps=ilnblnk( hfluxmeanbarfile )
      write(fnamehfluxmeanbar,'(2a,i10.10)')
     &     hfluxmeanbarfile(1:ilps), '.', eccoiter
      write(adfnamehfluxmeanbar,'(3a,i10.10)')
     &     yadprefix, hfluxmeanbarfile(1:ilps), '.', eccoiter

      do irec = 1, nyearsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamehfluxmeanbar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_hflux_mean_dummy )
#endif
      enddo
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
c--   Save sfluxmeanbar on file.
      ilps=ilnblnk( sfluxmeanbarfile )
      write(fnamesfluxmeanbar,'(2a,i10.10)')
     &     sfluxmeanbarfile(1:ilps), '.', eccoiter
      write(adfnamesfluxmeanbar,'(3a,i10.10)')
     &     yadprefix, sfluxmeanbarfile(1:ilps), '.', eccoiter

      do irec = 1, nyearsrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamesfluxmeanbar, tmpfld2d, irec,
     &                         eccoiter,mythid, xx_sflux_mean_dummy )
#endif
      enddo
#endif

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
      ilps=ilnblnk( iestaubarfile )
      write(fnameiestaubar,'(2a,i10.10)')
     &     iestaubarfile(1:ilps), '.', eccoiter
      write(adfnameiestaubar,'(3a,i10.10)')
     &     yadprefix, iestaubarfile(1:ilps), '.', eccoiter
      do irec = 1, ndaysrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnameiestaubar, tmpfld2d, irec,
     &        eccoiter,mythid, xx_iestaubar_mean_dummy )
#endif
      enddo

#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_SEAICE
      if (useSEAICE) then

#ifdef ALLOW_SEAICE_COST_SMR_AREA
c initialize smr area bar
      ilps=ilnblnk( smrareabarfile )
      write(fnamesmrareabar,'(2a,i10.10)')
     &     smrareabarfile(1:ilps), '.', eccoiter
      write(adfnamesmrareabar,'(3a,i10.10)')
     &     yadprefix, smrareabarfile(1:ilps), '.', eccoiter
c initialize smr sst bar
      ilps=ilnblnk( smrsstbarfile )
      write(fnamesmrsstbar,'(2a,i10.10)')
     &     smrsstbarfile(1:ilps), '.', eccoiter
      write(adfnamesmrsstbar,'(3a,i10.10)')
     &     yadprefix, smrsstbarfile(1:ilps), '.', eccoiter
c initialize smr sss bar
      ilps=ilnblnk( smrsssbarfile )
      write(fnamesmrsssbar,'(2a,i10.10)')
     &     smrsssbarfile(1:ilps), '.', eccoiter
      write(adfnamesmrsssbar,'(3a,i10.10)')
     &     yadprefix, smrsssbarfile(1:ilps), '.', eccoiter

      do irec = 1, ndaysrec
#ifdef ALLOW_ADJOINT_RUN
        call active_write_xy( adfnamesmrareabar, tmpfld2d, irec,
     &        eccoiter,mythid, xx_smrareabar_mean_dummy )
        call active_write_xy( adfnamesmrsstbar, tmpfld3d, irec,
     &        eccoiter,mythid, xx_smrsstbar_mean_dummy )
        call active_write_xy( adfnamesmrsssbar, tmpfld3d, irec,
     &        eccoiter,mythid, xx_smrsssbar_mean_dummy )
#endif
      enddo
#endif /* ALLOW_SEAICE_COST_SMR_AREA */

      endif
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_GENCOST_CONTRIBUTION
c--   Save gencost_barfile on file.
      do k=1,NGENCOST
      if ( .NOT.gencost_barskip(k) ) then
      ilt=ilnblnk( gencost_barfile(k) )
      write(fname_gencostbar,'(2a,i10.10)')
     &     gencost_barfile(k)(1:ilt), '.', eccoiter

#ifdef ALLOW_AUTODIFF
      if ( useAUTODIFF ) then
      write(adfname_gencostbar,'(3a,i10.10)')
     &     yadprefix, gencost_barfile(k)(1:ilt), '.', eccoiter
      do irec = 1,gencost_nrec(k)
#ifdef ALLOW_ADJOINT_RUN
        if (.NOT.gencost_is3d(k))
     &  call active_write_xy( adfname_gencostbar, tmpfld2d, irec,
     &       eccoiter, mythid, gencost_dummy(k) )
        if (gencost_is3d(k))
     &  call active_write_xyz( adfname_gencostbar, tmpfld3d, irec,
     &       eccoiter, mythid, gencost_dummy(k) )
#endif
      enddo
      endif
#endif

      endif
      enddo
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      return
      end
