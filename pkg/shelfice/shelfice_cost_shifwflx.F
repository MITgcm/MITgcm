#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_COST_SHIFWFLX
C     !INTERFACE:
      SUBROUTINE SHELFICE_COST_SHIFWFLX(
     I                       myThid
     &                     )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE shelfice_cost_shifwflx
c     ==================================================================
c
c     o cost function contribution of shifwflx
c
c     ==================================================================
c     SUBROUTINE shelfice_cost_shifwflx
c     ==================================================================
C     \ev

C     !USES:

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
# include "SHELFICE_COST.h"
#endif

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "optim.h"
#endif /* ALLOW_CTRL */

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==

      integer myThid

#if   (defined (ALLOW_SHELFICE) && \
       defined (ALLOW_SHIFWFLX_COST_CONTRIBUTION) && \
       defined (ALLOW_SHIFWFLX_CONTROL) )

C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer ilfld
      integer irec
      integer nrec
      integer startrec
      integer endrec

      _RL fctile
      _RL fcthread
      _RL tmpx
      _RL tmpfld2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      character*(80) fnamefld

      logical doglobalread
      logical ladinit

#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)

      startrec = ncvarrecstart(45)
      endrec   = ncvarrecsend(45)

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

      fcthread = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,i9.8)')
     &  ' shelfice_cost_shifwflx: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )
#endif

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_shifwflx_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_shifwflx_file(1:ilfld), '.', optimcycle
      endif

c--   Loop over records.
      do irec = 1,nrec

       call active_read_xy(
     &      fnamefld, tmpfld2d, irec, doglobalread,
     &      ladinit, optimcycle, myThid, xx_shifwflx_dummy )

c--   Loop over this thread s tiles.
       do bj = jtlo,jthi
        do bi = itlo,ithi

c--   Determine the weights to be used.
         fctile = 0. _d 0
         do j = 1,sny
          do i = 1,snx
           tmpx = tmpfld2d(i,j,bi,bj)
           fctile = fctile + tmpx*tmpx
     &           *wshifwflx(i,j,bi,bj)
           if ( wshifwflx(i,j,bi,bj) .ne. 0. )
     &          num_shifwflx(bi,bj) = num_shifwflx(bi,bj)
     &          + 1. _d 0
          enddo
         enddo

         objf_shifwflx(bi,bj) = objf_shifwflx(bi,bj) + fctile
         fcthread             = fcthread + fctile

        enddo
       enddo

c--   End of loop over records.
      enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
      _GLOBAL_SUM_RL( fcthread , myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,i8.8)')
     &     ' shelfice_cost_shifwflx: irec = ',irec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,a,d22.15)')
     &     ' global cost function value',
     &     ' (shifwflx) = ',fcthread
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

#endif /* ALLOW_SHEFLICE and (SHIFWFLX_COST_CONTRIBUTION or CONTROL) */

      return
      end
