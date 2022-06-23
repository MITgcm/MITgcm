#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_OBCSE
C     !INTERFACE:
      subroutine cost_obcse(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       mythid )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE cost_obcse
c     ==================================================================
c
c     o cost function contribution obc
c
c     ==================================================================
c     SUBROUTINE cost_obcse
c     ==================================================================
C     \ev

C     !USES:

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
c#ifdef ALLOW_OBCS
c# include "OBCS_GRID.h"
c#endif

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "optim.h"
# include "CTRL_OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer startrec
      integer endrec
      integer mythid

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))

#ifdef ALLOW_OBCSE_COST_CONTRIBUTION

c     == external functions ==
      integer  ilnblnk
      external ilnblnk

C     !LOCAL VARIABLES:
c     == local variables ==
      integer bi,bj
      integer j,k
      integer jmin,jmax
      integer irec
      integer iobcs
      integer nrec
      integer ilfld
      integer igg
      _RL fctile
      _RL fcthread
      _RL dummy
      _RL gg
      _RL tmpx
cgg(
      _RL tmpfield (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL maskyz   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      character*(80) fnamefld
      logical doglobalread
      logical ladinit
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif
c     == end of interface ==
CEOP

      jmin = 1
      jmax = sNy

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

c     ip1 = 1
      fcthread = 0. _d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8)')
     &  ' cost_obcse: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_obcse_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_obcse_file(1:ilfld), '.', optimcycle
      endif

c--   Loop over records.
      do irec = 1,nrec

        call active_read_yz( fnamefld, tmpfield, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_obcse_dummy )

cgg    Need to solve for iobcs would have been.
          gg    = (irec-1)/nobcs
          igg   = int(gg)
          iobcs = irec - igg*nobcs

          call active_read_yz( 'maskobcse', maskyz,
     &                         iobcs,
     &                         doglobalread, ladinit, 0,
     &                         mythid, dummy )

c--     Loop over this thread s tiles.
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)

c--         Determine the weights to be used.
            fctile = 0. _d 0

            do k = 1, Nr
              do j = jmin,jmax
c               i = OB_Iw(j,bi,bj)
cgg                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfield(j,k,bi,bj)
CMM                  fctile = fctile + wobcse2(j,k,bi,bj,iobcs)
                  fctile = fctile + wobcse(k,iobcs)
     &                        *tmpx*tmpx*maskyz(j,k,bi,bj)
cgg                endif
CMM                  if (wobcsw2(j,k,bi,bj,iobcs)*maskyz(j,k,bi,bj).ne.0.)
                  if (wobcse(k,iobcs)*maskyz(j,k,bi,bj).ne.0.)
     &                    num_obcse(bi,bj) = num_obcse(bi,bj) + 1. _d 0
              enddo
            enddo

            objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
            fcthread         = fcthread + fctile
          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_RL( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_obcse: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcse) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#endif /* ALLOW_OBCSE_COST_CONTRIBUTION */

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      return
      end
