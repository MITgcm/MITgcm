C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_theta.F,v 1.9 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_theta( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_theta
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
c     SUBROUTINE cost_theta
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
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

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec, irectmp
      integer levmon
      integer levoff
      integer iltheta

      _RL fctile
      _RL fcthread

      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval
      _RL spmax

      character*(80) fnametheta

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf
#ifdef GENERIC_BAR_MONTH
      integer mrec, nyears, iyear
#endif

      _RL diagnosfld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

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
      spmax = 40.

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_THETA_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        iltheta = ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &    tbarfile(1:iltheta),'.',optimcycle
      endif

      fcthread = 0. _d 0

#ifdef GENERIC_BAR_MONTH
c--   Loop over month
      do irec = 1,min(nmonsrec,12)
         nyears=int((nmonsrec-irec)/12)+1
            do iyear=1,nyears
            mrec=irec+(iyear-1)*12
            irectmp=mrec
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

        irectmp = irec
c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_tbar_mean_dummy )
#endif
c--     Determine the month to be read.
        levoff = mod(modelstartdate(1)/100,100)
        levmon = (irectmp-1) + levoff
        levmon = mod(levmon-1,12)+1

        call mdsreadfield( tdatfile, cost_iprec, cost_yftype,
     &                     nr, tdat, levmon, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr

c--           Determine the mask on weights
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = cosphi(i,j,bi,bj)
                  if (tdat(i,j,k,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  else if (tdat(i,j,k,bi,bj) .lt. spval) then
                    cmask(i,j) = 0. _d 0
                  else if (tdat(i,j,k,bi,bj) .gt. spmax) then
                    cmask(i,j) = 0. _d 0
                  endif
                enddo
              enddo

c--           Compute model data misfit and cost function term for
c             the temperature field.
              do j = jmin,jmax
                do i = imin,imax
                  if ( _hFacC(i,j,k,bi,bj) .ne. 0. ) then
                     fctile = fctile +
     &                    (wthetaLev(i,j,k,bi,bj)*cmask(i,j)*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj))*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj)) )
                     if ( wthetaLev(i,j,k,bi,bj)*cmask(i,j) .ne. 0. )
     &                    num_temp(bi,bj) = num_temp(bi,bj) + 1. _d 0
                     diagnosfld3d(i,j,k,bi,bj) =
     &                    (wthetaLev(i,j,k,bi,bj)*cmask(i,j)*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj))*
     &                    (tbar(i,j,k,bi,bj) - tdat(i,j,k,bi,bj)) )
                  else
                     diagnosfld3d(i,j,k,bi,bj) = 0.
                  endif
                enddo
              enddo

            enddo
c--         End of loop over layers.

            call mdswritefield( 'DiagnosCost_ClimTheta',
     &           writeBinaryPrec, globalfiles, 'RL', Nr,
     &           diagnosfld3d, irec, optimcycle, mythid )

            fcthread         = fcthread         + fctile
            objf_temp(bi,bj) = objf_temp(bi,bj) + fctile

          enddo
        enddo

      enddo
c--   End of loop over records.

#endif

      return
      end

