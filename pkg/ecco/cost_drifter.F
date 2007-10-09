C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_drifter.F,v 1.4 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_drifter(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_drifter
c     ==================================================================
c
c     o Evaluate cost function contribution of temperature.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c
c              - set ladinit to .true. to initialise adubar file
c
c     ==================================================================
c     SUBROUTINE cost_drifter
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer i6min,i6max
      integer iglomin
      integer irec
      integer ilu

      _RL fctile_drift
      _RL fcthread_drift
      _RL www    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wud    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wvd    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL uddat  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL u6bar  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vddat  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL v6bar  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL udmod  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vdmod  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mask13c(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mask6c (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL masktmp(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      character*(80) fnameud
      character*(80) fnamevd

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf

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

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_DRIFTER_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilu = ilnblnk( ubarfile )
        write(fnameud(1:80),'(2a,i10.10)')
     &    ubarfile(1:ilu),'.',optimcycle
        ilu = ilnblnk( vbarfile )
        write(fnamevd(1:80),'(2a,i10.10)')
     &    vbarfile(1:ilu),'.',optimcycle
      endif

      fcthread_drift = 0. _d 0

      do bj = jtlo,jthi
       do bi = itlo,ithi

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = 0
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

         k = 2
         do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
          call active_read_xyz( fnameud, ubar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_ubar_mean_dummy )

          call active_read_xyz( fnamevd, vbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_vbar_mean_dummy )

          do j = jmin,jmax
           do i = imin,imax
            if(irec.eq.1)then
               udmod(i,j,bi,bj)=ubar(i,j,k,bi,bj)
               vdmod(i,j,bi,bj)=vbar(i,j,k,bi,bj)
            elseif(irec.eq.nmonsrec)then
               udmod(i,j,bi,bj)=udmod(i,j,bi,bj)/float(nmonsrec)
               vdmod(i,j,bi,bj)=vdmod(i,j,bi,bj)/float(nmonsrec)
            else
               udmod(i,j,bi,bj)=udmod(i,j,bi,bj)+ubar(i,j,k,bi,bj)
               vdmod(i,j,bi,bj)=vdmod(i,j,bi,bj)+vbar(i,j,k,bi,bj)
            endif
           enddo
          enddo
         enddo

c--     Read drifter data
         call mdsreadfield( udriftfile, 32, 'RL', 1, udriftdat, 1,
     &                    mythid)
         call mdsreadfield( vdriftfile, 32, 'RL', 1, vdriftdat, 1,
     &                    mythid)
c--     Read error data
         call mdsreadfield( udrifterrfile, 32, 'RL', 1, wudrift, 1,
     &                    mythid)
         call mdsreadfield( vdrifterrfile, 32, 'RL', 1, wvdrift, 1,
     &                    mythid)

         fctile_drift = 0. _d 0

c--           Calculate mask for tracer cells  (0 => land, 1 => water)
         do j = jmin,jmax
            do i = imin,imax
               mask13c(i,j,bi,bj) = 1. _d 0
               if (_hFacC(i,j,k,bi,bj) .eq. 0.)
     &              mask13c(i,j,bi,bj) = 0. _d 0

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c                 set mask13c=0 in areas shallower than 1000m
               if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                  mask13c(i,j,bi,bj) = 0. _d 0
               endif

            enddo
         enddo

         i6min=1

         do j = jmin,jmax-1,2
           do i = i6min,imax-5,6
             masktmp(i,j,bi,bj) =
     &           (mask13c(i,j,bi,bj)+mask13c(i+1,j,bi,bj)
     &           +mask13c(i+2,j,bi,bj)+mask13c(i+3,j,bi,bj)
     &           +mask13c(i+4,j,bi,bj)+mask13c(i+5,j,bi,bj)
     &           +mask13c(i,j+1,bi,bj)+mask13c(i+1,j+1,bi,bj)
     &           +mask13c(i+2,j+1,bi,bj)+mask13c(i+3,j+1,bi,bj)
     &           +mask13c(i+4,j+1,bi,bj)+mask13c(i+5,j+1,bi,bj))
             if ( masktmp(i,j,bi,bj) .eq. 0.0 ) then
                u6bar(i,j,bi,bj) = 0.0
             else
                u6bar(i,j,bi,bj) = (
     &                  udmod(i,j,bi,bj)*mask13c(i,j,bi,bj)
     &                + udmod(i+1,j,bi,bj)*mask13c(i+1,j,bi,bj)
     &                + udmod(i+2,j,bi,bj)*mask13c(i+2,j,bi,bj)
     &                + udmod(i+3,j,bi,bj)*mask13c(i+3,j,bi,bj)
     &                + udmod(i+4,j,bi,bj)*mask13c(i+4,j,bi,bj)
     &                + udmod(i+5,j,bi,bj)*mask13c(i+5,j,bi,bj)
     &                + udmod(i,j+1,bi,bj)*mask13c(i,j+1,bi,bj)
     &                + udmod(i+1,j+1,bi,bj)*mask13c(i+1,j+1,bi,bj)
     &                + udmod(i+2,j+1,bi,bj)*mask13c(i+2,j+1,bi,bj)
     &                + udmod(i+3,j+1,bi,bj)*mask13c(i+3,j+1,bi,bj)
     &                + udmod(i+4,j+1,bi,bj)*mask13c(i+4,j+1,bi,bj)
     &                + udmod(i+5,j+1,bi,bj)*mask13c(i+5,j+1,bi,bj) )
     &             / ( masktmp(i,j,bi,bj) )
             endif
           enddo
         enddo

         do j = jmin,jmax-1,2
           do i = i6min,imax-5,6
             masktmp(i,j,bi,bj) =
     &             (mask13c(i,j,bi,bj)+mask13c(i+1,j,bi,bj)
     &             +mask13c(i+2,j,bi,bj)+mask13c(i+3,j,bi,bj)
     &             +mask13c(i+4,j,bi,bj)+mask13c(i+5,j,bi,bj)
     &             +mask13c(i,j+1,bi,bj)+mask13c(i+1,j+1,bi,bj)
     &             +mask13c(i+2,j+1,bi,bj)+mask13c(i+3,j+1,bi,bj)
     &             +mask13c(i+4,j+1,bi,bj)+mask13c(i+5,j+1,bi,bj))
             if ( masktmp(i,j,bi,bj) .eq.0.0 ) then
                v6bar(i,j,bi,bj) = 0.0
             else
                v6bar(i,j,bi,bj) = (
     &                  vdmod(i,j,bi,bj)*mask13c(i,j,bi,bj)
     &                + vdmod(i+1,j,bi,bj)*mask13c(i+1,j,bi,bj)
     &                + vdmod(i+2,j,bi,bj)*mask13c(i+2,j,bi,bj)
     &                + vdmod(i+3,j,bi,bj)*mask13c(i+3,j,bi,bj)
     &                + vdmod(i+4,j,bi,bj)*mask13c(i+4,j,bi,bj)
     &                + vdmod(i+5,j,bi,bj)*mask13c(i+5,j,bi,bj)
     &                + vdmod(i,j+1,bi,bj)*mask13c(i,j+1,bi,bj)
     &                + vdmod(i+1,j+1,bi,bj)*mask13c(i+1,j+1,bi,bj)
     &                + vdmod(i+2,j+1,bi,bj)*mask13c(i+2,j+1,bi,bj)
     &                + vdmod(i+3,j+1,bi,bj)*mask13c(i+3,j+1,bi,bj)
     &                + vdmod(i+4,j+1,bi,bj)*mask13c(i+4,j+1,bi,bj)
     &                + vdmod(i+5,j+1,bi,bj)*mask13c(i+5,j+1,bi,bj) )
     &             / ( masktmp(i,j,bi,bj) )
             endif
           enddo
         enddo

         do j = jmin,jmax-1,2
           do i = i6min,imax-5, 6
c--   change unit from cm/s to m/s
              uddat(i,j,bi,bj) = 0.01*udriftdat(i,j,bi,bj)
              vddat(i,j,bi,bj) = 0.01*vdriftdat(i,j,bi,bj)
c-- 5 cm/s lower limit
              wud(i,j,bi,bj) = 1e4*max(wudrift(i,j,bi,bj),5.D0)**(-2)
              wvd(i,j,bi,bj) = 1e4*max(wvdrift(i,j,bi,bj),5.D0)**(-2)
c                  wud(i,j,bi,bj) = 1.0
c                  wvd(i,j,bi,bj) = 1.0
              mask6c(i,j,bi,bj) = 1.0
              if ( uddat(i,j,bi,bj).eq.0.0) mask6c(i,j,bi,bj)=0.0
              if ( abs(uddat(i,j,bi,bj)).gt.900) mask6c(i,j,bi,bj)=0.0
              if ( vddat(i,j,bi,bj).eq.0.0) mask6c(i,j,bi,bj)=0.0
              if ( abs(vddat(i,j,bi,bj)).gt.900) mask6c(i,j,bi,bj)=0.0
           enddo
         enddo

CADJ STORE wud(:,:,bi,bj)
CADJ &     = tapelev_ini_bibj_k, key=ikey, byte=isbyte
CADJ STORE wvd(:,:,bi,bj)
CADJ &     = tapelev_ini_bibj_k, key=ikey, byte=isbyte
CADJ STORE u6bar(:,:,bi,bj)
CADJ &     = tapelev_ini_bibj_k, key=ikey, byte=isbyte
CADJ STORE v6bar(:,:,bi,bj)
CADJ &     = tapelev_ini_bibj_k, key=ikey, byte=isbyte

c--           Compute model data misfit and cost function term for
c             drifters.
         do j = jmin,jmax-1,2
            do i = i6min,imax-5, 6
               fctile_drift = fctile_drift
     &              + (wud(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                 mask6c(i,j,bi,bj)*
     &                (u6bar(i,j,bi,bj) - uddat(i,j,bi,bj))*
     &                (u6bar(i,j,bi,bj) - uddat(i,j,bi,bj))   )
     &              + (wvd(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                 mask6c(i,j,bi,bj)*
     &                (v6bar(i,j,bi,bj) - vddat(i,j,bi,bj))*
     &                (v6bar(i,j,bi,bj) - vddat(i,j,bi,bj))   )
               if ( cosphi(i,j,bi,bj)*mask6c(i,j,bi,bj) .ne. 0. ) then
                  if ( wud(i,j,bi,bj) .ne. 0. )
     &                 num_drift(bi,bj) = num_drift(bi,bj) + 1. _d 0
                  if ( wvd(i,j,bi,bj) .ne. 0. )
     &                 num_drift(bi,bj) = num_drift(bi,bj) + 1. _d 0
               endif
            enddo
         enddo

         fcthread_drift   = fcthread_drift + fctile_drift
         objf_drift(bi,bj) = objf_drift(bi,bj) + fctile_drift

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_drifter: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '     cost function (temperature) = ',
     &        fctile_drift
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

       enddo
      enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread_drift , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_drift: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (drifters) = ',fcthread_drift
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

#endif

      return
      end

