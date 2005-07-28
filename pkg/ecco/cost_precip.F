
#include "COST_CPPOPTIONS.h"


      subroutine cost_precip(
     I                           myiter,
     I                           mytime,
     I                           startrec,
     I                           endrec,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_precip
c     ==================================================================
c
c     o Calculate the atmos. temp. contribution to the cost function.
c
c     ==================================================================
c     SUBROUTINE cost_precip
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
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
      _RL fcthread
      _RL tmpx

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

#ifdef ALLOW_PRECIP_COST_CONTRIBUTION

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_precip_file )
        write(fnamefld(1:80),'(2a,i10.10)') 
     &       xx_precip_file(1:ilfld),'.',optimcycle
      endif

      fcthread = 0. _d 0

c--   Loop over records.
      do irec = 1,nrec

        call active_read_xy_loc( fnamefld, tmpfld2d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_precip_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            kk = 1
            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,kk,bi,bj) .ne. 0.) then
                  tmpx = tmpfld2d(i,j,bi,bj)
                  fctile = fctile
     &                 + wprecip(i,j,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
                  if ( wprecip(i,j,bi,bj)*cosphi(i,j,bi,bj) .ne. 0. )
     &                 num_precip(bi,bj) = num_precip(bi,bj) + 1
                endif
              enddo
            enddo

            objf_precip(bi,bj) = objf_precip(bi,bj) + fctile
            fcthread         = fcthread + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_precip: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '               cost function (precip) = ',
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
     &    ' cost_precip:                       irec =  ',irec
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
#else
c--   Do not enter the calculation of the heat flux contribution
c--   to the final cost function.

      fctile   = 0. _d 0
      fcthread = 0. _d 0

      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' cost_precip: no contribution of heat flux ',
     &                    ' to cost function.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,i9.8)')
     &    ' cost_precip: number of records that would have',
     &                      ' been processed: ',nrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      return
      end

