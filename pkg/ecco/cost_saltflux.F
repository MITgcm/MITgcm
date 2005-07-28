C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_saltflux.F,v 1.5 2005/07/28 21:32:39 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_saltflux(
     I                           myiter,
     I                           mytime,
     I                           startrec,
     I                           endrec,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_saltflux
c     ==================================================================
c
c     o Calculate the salt flux contribution to the cost function.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_saltflux
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer startrec
      integer endrec
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,kk
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nrec
      integer irec
      integer ilfld

      _RL fctile
      _RL fctilem
      _RL fctilemm
      _RL fcthread
      _RL tmpx
      _RL sumcos

      _RL xx_sflux_mean (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnamefld

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

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

#ifdef ALLOW_SFLUX_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_sflux_file )
        write(fnamefld(1:80),'(2a,i10.10)') 
     &       xx_sflux_file(1:ilfld),'.',optimcycle
      endif

      fcthread = 0. _d 0

c--   Loop 1 to compute mean forcing:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              xx_sflux_mean(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      do irec = 1,nrec

        call active_read_xy_loc( fnamefld, tmpfld2d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_sflux_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                xx_sflux_mean(i,j,bi,bj) = xx_sflux_mean(i,j,bi,bj)
     &                                     + tmpfld2d(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo

      enddo

      do bj = jtlo,jthi
        do bi = itlo,ithi

c--       Determine the weights to be used.
          kk = 1
          fctilem = 0. _d 0
          do j = jmin,jmax
            do i = imin,imax
              xx_sflux_mean(i,j,bi,bj)
     &              = xx_sflux_mean(i,j,bi,bj)/float(nrec)
              tmpx = xx_sflux_mean(i,j,bi,bj)/1.6 _d -8
              if (maskC(i,j,kk,bi,bj) .ne. 0.) then
                if ( ABS(R_low(i,j,bi,bj)) .LT. 100. )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.
                fctilem = fctilem + cosphi(i,j,bi,bj)*tmpx*tmpx
                if ( cosphi(i,j,bi,bj) .ne. 0. )
     &               num_sfluxm(bi,bj) = num_sfluxm(bi,bj) + 1. _d 0
              endif
            enddo
          enddo

          objf_sfluxm(bi,bj) = objf_sfluxm(bi,bj) + fctilem
          fcthread           = fcthread + fctilem

#ifdef ECCO_VERBOSE
c--       Print cost function for each tile in each thread.
          write(msgbuf,'(a)') ' '
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &      ' cost_zonstress: irec,bi,bj          =  ',irec,bi,bj
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a,d22.15)')
     &      '               cost function (zonal) = ',
     &      fctilem
          call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

        enddo
      enddo

      fcthread = 0. _d 0

c--   Loop 2 over records.
      do irec = 1,nrec

        call active_read_xy_loc( fnamefld, tmpfld2d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_sflux_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            kk = 1
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,kk,bi,bj) .ne. 0.) then
                  tmpx = tmpfld2d(i,j,bi,bj)-xx_sflux_mean(i,j,bi,bj)
                  if ( ABS(R_low(i,j,bi,bj)) .LT. 100. )
     &              tmpx = tmpx*ABS(R_low(i,j,bi,bj))/100.
                  fctile = fctile
     &                 + wsflux(i,j,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
                  if ( wsflux(i,j,bi,bj)*cosphi(i,j,bi,bj) .ne. 0. )
     &                 num_sflux(bi,bj) = num_sflux(bi,bj) + 1. _d 0
                endif
              enddo
            enddo

            objf_sflux(bi,bj) = objf_sflux(bi,bj) + fctile
            fcthread          = fcthread + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_saltflux: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '               cost function (sflux) = ',
     &        fctile
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
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
     &    ' cost_saltflux:                       irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,d22.15)')
     &    '                 global cost function value = ',
     &    fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif
c--   End of loop over records.
      enddo

#endif

      end

