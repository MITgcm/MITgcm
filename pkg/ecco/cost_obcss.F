#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_OBCSS
C     !INTERFACE:
      subroutine cost_obcss(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       mythid )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE cost_obcss
c     ==================================================================
c
c     o cost function contribution obc
c
c     ==================================================================
c     SUBROUTINE cost_obcss
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

#ifdef ALLOW_OBCSS_COST_CONTRIBUTION

c     == external functions ==
      integer  ilnblnk
      external ilnblnk

C     !LOCAL VARIABLES:
c     == local variables ==
      integer bi,bj
      integer i,k
      integer imin,imax
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
      _RL tmpfield (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL maskxz   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      character*(80) fnamefld
      logical doglobalread
      logical ladinit
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif
c     == end of interface ==
CEOP

      imin = 1
      imax = sNx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c     Number of records to be used.
      nrec = endrec-startrec+1

c     jp1 = 1
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
     &  ' cost_obcss: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_obcss_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_obcss_file(1:ilfld), '.', optimcycle
      endif

c--   Loop over records.
      do irec = 1,nrec

        call active_read_xz( fnamefld, tmpfield, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_obcss_dummy )

cgg    Need to solve for iobcs would have been.
          gg    = (irec-1)/nobcs
          igg   = int(gg)
          iobcs = irec - igg*nobcs
cgg          print*,'S IREC, IOBCS',irec, iobcs

          call active_read_xz( 'maskobcss', maskxz,
     &                         iobcs,
     &                         doglobalread, ladinit, 0,
     &                         mythid, dummy )

c--     Loop over this thread s tiles.
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)

c--         Determine the weights to be used.
            fctile = 0. _d 0

            do k = 1, Nr
              do i = imin,imax
c                j = OB_Js(I,bi,bj)
cgg                 if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfield(i,k,bi,bj)
CMM                  fctile = fctile + wobcss2(i,k,bi,bj,iobcs)
                  fctile = fctile + wobcss(k,iobcs)
     &                        *tmpx*tmpx*maskxz(i,k,bi,bj)
cgg                endif
CMM                  if (wobcss2(i,k,bi,bj,iobcs)*maskxz(i,k,bi,bj).ne.0.)
                  if (wobcss(k,iobcs)*maskxz(i,k,bi,bj).ne.0.)
     &                    num_obcss(bi,bj) = num_obcss(bi,bj) + 1. _d 0
cgg                print*,'S fctile',fctile
              enddo
            enddo

            objf_obcss(bi,bj) = objf_obcss(bi,bj) + fctile
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
     &    ' cost_obcss: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcss) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#endif /* ALLOW_OBCSS_COST_CONTRIBUTION */

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      return
      end
