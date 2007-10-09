C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ctdtclim.F,v 1.5 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_Ctdtclim(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_Ctdtclim
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
c              - set ladinit to .true. to initialise adtbar file
c
c     ==================================================================
c     SUBROUTINE cost_Ctdtclim
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

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

      _RS        one_rs
      parameter( one_rs = 1. )

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer levmon
      integer levoff
      integer ilctdtclim

      _RL fctile
      _RL fcthread

      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval

      character*(80) fnametheta

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf
#ifdef GENERIC_BAR_MONTH
      integer mrec, nyears, iyear
#endif
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

      spval = -1.8

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_CTDTCLIM_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilctdtclim = ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &    tbarfile(1:ilctdtclim),'.',optimcycle
      endif

      fcthread = 0. _d 0

#ifdef GENERIC_BAR_MONTH
c--   Loop over month
      do irec = 1,12
         nyears=int((nmonsrec-irec)/12)+1
         if(nyears.gt.0) then
            do iyear=1,nyears
            mrec=irec+(iyear-1)*12
c--     Read time averages and the monthly mean data.
            call active_read_xyz( fnametheta, tbar, mrec,
     &           doglobalread, ladinit,
     &           optimcycle, mythid,
     &           xx_tbar_mean_dummy )
            do bj = jtlo,jthi
               do bi = itlo,ithi
                  do k = 1,nr
                     do j = jmin,jmax
                        do i = imin,imax
                           if(iyear.eq.1) then
                              tbar_gen(i,j,k,bi,bj) =tbar(i,j,k,bi,bj)
                           elseif(iyear.eq.nyears) then
                              tbar(i,j,k,bi,bj) =(tbar_gen(i,j,k,bi,bj)
     $                             +tbar(i,j,k,bi,bj))/float(nyears)
                           else
                          tbar_gen(i,j,k,bi,bj) =tbar_gen(i,j,k,bi,bj)
     $                             +tbar(i,j,k,bi,bj)
                           endif
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
#else
c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_tbar_mean_dummy )
#endif
c--     Determine the month to be read.
        levoff = mod(modelstartdate(1)/100,100)
        levmon = (irec-1) + levoff
        levmon = mod(levmon-1,12)+1

        call mdsreadfield( ctdtclimfile, cost_iprec, cost_yftype,
     &                     nr, tdat, levmon, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr

c--           Determine the mask on weights
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = 1. _d 0
                  if (tdat(i,j,k,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  endif

                  if (tdat(i,j,k,bi,bj) .lt. spval) then
                    cmask(i,j) = 0. _d 0
                  endif

cph(
cph               print *, 'WARNING: SPECIFIC SETUP FOR ECCO'
cph               below statement could be replaced by following
cph               to make it independnet of Nr:
cph
cph               if ( rC(K) .GT. -1000. ) then
cph)
c                 set cmask=0 in areas shallower than 1000m
                  if (_hFacC(i,j,13,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  endif
                enddo
              enddo

c--           Compute model data misfit and cost function term for
c             the temperature field.
              do j = jmin,jmax
                do i = imin,imax
                  if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                  (wtheta2(i,j,k,bi,bj)*cosphi(i,j,bi,bj)*
     &                    cmask(i,j)*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj))*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj))      )
                  endif
                enddo
              enddo

            enddo
c--         End of loop over layers.

            fcthread         = fcthread         + fctile
            objf_ctdtclim(bi,bj) = objf_ctdtclim(bi,bj) + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_Ctdtclim: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '     cost function (temperature) = ',
     &        fctile
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
#endif

          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_Ctdtclim: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (temperature) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

#ifdef GENERIC_BAR_MONTH
        endif
#endif
      enddo
c--   End of loop over records.

#else
c--   Do not enter the calculation of the temperature contribution to
c--   the final cost function.

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_Ctdtclim: no contribution of temperature field ',
     &                 'to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_Ctdtclim: number of records that would have',
     &                ' been processed: ',nmonsrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

#endif

      return
      end

