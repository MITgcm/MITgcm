C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_scat.F,v 1.7 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_scat(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_scat
c     ==================================================================
c
c     o Evaluate cost function contribution of surface wind stress observations.
c
c     started: Detlef Satmmer 01-mar-2002 copy from cost_sst.F
c
c     ==================================================================
c     SUBROUTINE cost_scat
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

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer levmon
      integer levoff
      integer ilsalt

      _RL fctile_scatx
      _RL fcthread_scatx
      _RL fctile_scaty
      _RL fcthread_scaty

      character*(80) fnametaux
      character*(80) fnametauy

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

#ifdef ALLOW_SCAT_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilsalt = ilnblnk( tauxbarfile )
        write(fnametaux(1:80),'(2a,i10.10)')
     &    tauxbarfile(1:ilsalt),'.',optimcycle
       ilsalt = ilnblnk( tauybarfile )
        write(fnametauy(1:80),'(2a,i10.10)')
     &    tauybarfile(1:ilsalt),'.',optimcycle
      endif

      fcthread_scatx = 0. _d 0
      fcthread_scaty = 0. _d 0

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xy( fnametaux, tauxbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_taux_mean_dummy )

        call active_read_xy( fnametauy, tauybar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_tauy_mean_dummy )


        do bj = jtlo,jthi
          do bi = itlo,ithi

            fctile_scatx = 0. _d 0
            fctile_scaty = 0. _d 0

            k = 1

c--         Compute cost rel. to monthly SCAT field.

            call cost_ReadscatXFields( irec, mythid )

            do j = jmin,jmax
              do i = imin,imax
                if (maskW(i,j,k,bi,bj) .ne. 0.) then
                   fctile_scatx = fctile_scatx +
     &                  wscatx(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                  ( (tauxbar(i,j,bi,bj)-scatxdat(i,j,bi,bj))*
     &                    (tauxbar(i,j,bi,bj)-scatxdat(i,j,bi,bj))*
     &                           scatxmask(i,j,bi,bj) )
                   if ( wscatx(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                  scatxmask(i,j,bi,bj) .ne. 0. )
     &                  num_scatx(bi,bj)  = num_scatx(bi,bj) + 1. _d 0
                endif
              enddo
            enddo

c--         Compute cost rel. to monthly SCAT field.

            call cost_ReadscatYFields( irec, mythid )

            do j = jmin,jmax
              do i = imin,imax
                if (maskS(i,j,k,bi,bj) .ne. 0.) then
                   fctile_scaty = fctile_scaty +
     &                  wscaty(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                  ( (tauybar(i,j,bi,bj)-scatydat(i,j,bi,bj))*
     &                    (tauybar(i,j,bi,bj)-scatydat(i,j,bi,bj))*
     &                           scatymask(i,j,bi,bj) )
                   if ( wscaty(i,j,bi,bj)*cosphi(i,j,bi,bj)*
     &                  scatymask(i,j,bi,bj) .ne. 0. )
     &                  num_scaty(bi,bj)  = num_scaty(bi,bj) + 1. _d 0
                endif
              enddo
            enddo


            fcthread_scatx     = fcthread_scatx + fctile_scatx
            objf_scatx(bi,bj)  = objf_scatx(bi,bj)  + fctile_scatx
            fcthread_scaty     = fcthread_scaty + fctile_scaty
            objf_scaty(bi,bj)  = objf_scaty(bi,bj)  + fctile_scaty

#ifdef ECCO_VERBOSE
c--         Print cost fscat for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_scat: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,2d22.15)')
     &        '     cost function (scat)         = ',
     &        fctile_scatx, fctile_scaty
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread_scatx   , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_scatx: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (    SCAT    ) = ',fcthread_scatx
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#else
c--   Do not enter the calculation of the temperature contribution to
c--   the final cost function.

      fctile_scatx     = 0. _d 0
      fcthread_scatx   = 0. _d 0
      fctile_scaty     = 0. _d 0
      fcthread_scaty   = 0. _d 0

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_scat: no contribution of temperature field ',
     &                 'to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_scat: number of records that would have',
     &                ' been processed: ',nmonsrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      return
      end

