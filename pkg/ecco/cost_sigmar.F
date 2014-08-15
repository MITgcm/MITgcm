C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_sigmar.F,v 1.1 2014/08/15 09:24:00 atn Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_sigmar( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_sigmar
c     ==================================================================
c
c     o Evaluate cost function contribution of sigmaR (drhodr).
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c              - set ladinit to .true. to initialise adsigmaRbar file
c     adapted: An Nguyen atnguyen@mit.edu 13-Aug-2014
c              - only allows fit to climatology (generic_bar_mean)
c
c     ==================================================================
c     SUBROUTINE cost_sigmar
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
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
c     == external functions ==
      integer  ilnblnk
      external ilnblnk

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
      integer ilsigmaR

      _RL fctile
      _RL fcthread

      _RL cmask (1-olx:snx+olx,1-oly:sny+oly)
      _RL spval
      _RL spmax

      character*(80) fnamesigmaR

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf

Catn#ifdef GENERIC_BAR_MONTH
Catn: make this default
      integer mrec, nyears, iyear
Catn#endif

      _RL diagnosfld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      spval = 0.
      spmax = 1.

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
        ilsigmaR = ilnblnk( sigmaRbarfile )
        write(fnamesigmaR(1:80),'(2a,i10.10)')
     &    sigmaRbarfile(1:ilsigmaR),'.',optimcycle
      endif

      fcthread = 0. _d 0

Catn#ifdef GENERIC_BAR_MONTH
c--   Loop over month
      do irec = 1,min(nmonsrec,12)
         nyears=int((nmonsrec-irec)/12)+1
            do bj = jtlo,jthi
               do bi = itlo,ithi
                  do k = 1,nr
                     do j = jmin,jmax
                        do i = imin,imax
                          sigmaRbar_gen(i,j,k,bi,bj) = 0. _d 0
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            do iyear=1,nyears
            mrec=irec+(iyear-1)*12
            irectmp=mrec
c--     Read time averages and the monthly mean data.
            call active_read_xyz( fnamesigmaR, sigmaRbar, mrec,
     &           doglobalread, ladinit,
     &           optimcycle, mythid,
     &           xx_sigmaRbar_mean_dummy )
            do bj = jtlo,jthi
               do bi = itlo,ithi
                  do k = 1,nr
                     do j = jmin,jmax
                        do i = imin,imax
                           sigmaRbar_gen(i,j,k,bi,bj) =
     $                           sigmaRbar_gen(i,j,k,bi,bj)
     $                          +sigmaRbar(i,j,k,bi,bj)/float(nyears)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
            enddo
Catn#endif
c--     Determine the month to be read.
        levoff = mod(modelstartdate(1)/100,100)
        levmon = (irectmp-1) + levoff
        levmon = mod(levmon-1,12)+1

        call mdsreadfield( sigmaRdatfile, cost_iprec, cost_yftype,
     &                     nr, sigmaRdat, levmon, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr

c--           Determine the mask or weights
              do j = jmin,jmax
                do i = imin,imax
                  cmask(i,j) = cosphi(i,j,bi,bj)
                  if (sigmaRdat(i,j,k,bi,bj) .eq. 0.) then
                    cmask(i,j) = 0. _d 0
                  else if (sigmaRdat(i,j,k,bi,bj) .lt. spval) then
                    cmask(i,j) = 0. _d 0
                  else if (sigmaRdat(i,j,k,bi,bj) .gt. spmax) then
                    cmask(i,j) = 0. _d 0
                  endif
                enddo
              enddo

c--           Compute model data misfit and cost function term for
c             the sigmaR field.
              do j = jmin,jmax
                do i = imin,imax
                  if ( _hFacC(i,j,k,bi,bj) .ne. 0. ) then
                     fctile = fctile +
     &                (wsigmaRLev(i,j,k,bi,bj)*cmask(i,j)*
     &                (sigmaRbar(i,j,k,bi,bj) - sigmaRdat(i,j,k,bi,bj))*
     &                (sigmaRbar(i,j,k,bi,bj) - sigmaRdat(i,j,k,bi,bj)))
                     if ( wsigmaRLev(i,j,k,bi,bj)*cmask(i,j) .ne. 0. )
     &                   num_sigmaR(bi,bj) = num_sigmaR(bi,bj) + 1. _d 0
C                     diagnosfld3d(i,j,k,bi,bj) =
C     &                (wsigmaRLev(i,j,k,bi,bj)*cmask(i,j)*
C     &                (sigmaRbar(i,j,k,bi,bj) - sigmaRdat(i,j,k,bi,bj))*
C     &                (sigmaRbar(i,j,k,bi,bj) - sigmaRdat(i,j,k,bi,bj)))
C                  else
C                     diagnosfld3d(i,j,k,bi,bj) = 0.
                  endif
                enddo
              enddo

            enddo
c--         End of loop over layers.

            fcthread         = fcthread         + fctile
            objf_sigmaR(bi,bj) = objf_sigmaR(bi,bj) + fctile

          enddo
        enddo

cph        CALL WRITE_REC_XYZ_RL( 'DiagnosCost_ClimSigmaR',
cph     &                 diagnosfld3d, irec, optimcycle, mythid )

      enddo
c--   End of loop over records.

#endif /* ALLOW_SIGMAR_COST_CONTRIBUTION */

      RETURN
      END
