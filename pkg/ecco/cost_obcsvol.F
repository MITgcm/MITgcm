#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_obcsvol(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       mythid )

c     ==================================================================
c     SUBROUTINE cost_obcsvol
c     ==================================================================
c
c     o cost function contribution obc -- Volume flux imbalance.
c
c     ==================================================================
c     SUBROUTINE cost_obcsvol
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "optim.h"
#endif

c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer startrec
      integer endrec
      integer mythid

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
#ifdef BAROTROPIC_OBVEL_CONTROL
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
      integer irec
      integer iobcs
      integer nrec
      integer ilfld
      integer igg
      _RL fctile
      _RL sumvol
      _RL gg
      _RL tmpx
      _RL tmpy
      _RL wobcsvol
      character*(80) fnamefldn
      character*(80) fnameflds
      character*(80) fnamefldw
      character*(80) fnameflde
#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      logical doglobalread
      logical ladinit
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == end of interface ==

      stop 's/r cost_obcsvol needs to be fixed'

      jtlo = myByLo(mythid)
      jthi = myByHi(mythid)
      itlo = myBxLo(mythid)
      ithi = myBxHi(mythid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

cgg   Assume the number of records is the same for
cgg   all boundaries.
c     Number of records to be used.
      nrec = endrec-startrec+1

      sumvol = 0. _d 0
      wobcsvol = .01
cgg   Acceptable volume flux is 10^-3. Corresponds to 5 mm change over a year.
cgg   Added a factor of 1000 because its very important to me.
      wobcsvol = 1./(wobcsvol * wobcsvol)

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8)')
     &  ' cost_obcsvol: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
#ifdef ALLOW_OBCSN_CONTROL
        ilfld=ilnblnk( xx_obcsn_file )
        write(fnamefldn(1:80),'(2a,i10.10)')
     &        xx_obcsn_file(1:ilfld),'.', optimcycle
#endif
#ifdef ALLOW_OBCSS_CONTROL
        ilfld=ilnblnk( xx_obcss_file )
        write(fnameflds(1:80),'(2a,i10.10)')
     &        xx_obcss_file(1:ilfld),'.',optimcycle
#endif
#ifdef ALLOW_OBCSW_CONTROL
        ilfld=ilnblnk( xx_obcsw_file )
        write(fnamefldw(1:80),'(2a,i10.10)')
     &        xx_obcsw_file(1:ilfld),'.',optimcycle
#endif
#ifdef ALLOW_OBCSE_CONTROL
        ilfld=ilnblnk( xx_obcse_file )
        write(fnameflde(1:80),'(2a,i10.10)')
     &        xx_obcse_file(1:ilfld),'.',optimcycle
#endif
      else
         print*
         print*,' obcs_obcsvol: optimcycle has a wrong value.'
         print*,'                 optimcycle = ',optimcycle
         print*
         stop   '  ... stopped in obcs_obcsvol.'
      endif

      do irec = 1,nrec
c--   Loop over records. For north boundary, we only need V velocity.

cgg    Need to solve for iobcs. Then only keep iobcs=3.or.4.
        gg   = (irec-1)/nobcs
        igg  = int(gg)
        iobcs = irec - igg*nobcs

#ifdef ALLOW_OBCSN_CONTROL
cgg   Assume that nobcs=4, and V velocity is the 4th record. I cannot
cgg   think of a more general way to do this.
        if (iobcs.eq.4) then
          call active_read_xz( fnamefldn, tmpfldxz, irec, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcsn_dummy )

cgg  At this point, do not be concerned with the overlap halos.
cgg  From experience, there is no control contribution in the
cgg  velocity points outside the boundaries. This has something
cgg  to do with the computational stencil, and the fact that we
cgg  are diagonally offset. Could check later by employing both
cgg  BALANCE_CONTROL_VOLFLUX and VOLFLUX_COST_CONTRIBUTION.
cgg
cgg  25-jan-03 --- no idea what i was talking about ^^^^
c--     Loop over this thread tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi

c--         Determine the weights to be used.
              fctile = 0. _d 0

              do k = 1, Nr
                do i = imin,imax
                  j = OB_Jn(I,bi,bj)
                  IF ( j.EQ.OB_indexNone ) j = 1
cgg    Barotropic velocity is stored in level 1.
                  tmpx = tmpfldxz(i,1,bi,bj)
                  if (maskS(i,j,k,bi,bj) .ne. 0.) then
cgg -- Positive is flux in.
                    fctile = fctile - tmpx* drF(k) *dxg(i,j,bi,bj)
     &                  * _hFacS(i,j,k,bi,bj)
                  endif
                enddo
              enddo

              sumvol         = sumvol + fctile
            enddo
          enddo
        endif
#endif

#ifdef ALLOW_OBCSS_CONTROL
cgg   Assume that nobcs=4, and V velocity is the 4th record. I cannot
cgg   think of a more general way to do this.
        if (iobcs.eq.4) then
          call active_read_xz( fnameflds, tmpfldxz, irec, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

cgg  At this point, do not be concerned with the overlap halos.
cgg  From experience, there is no control contribution in the
cgg  velocity points outside the boundaries. This has something
cgg  to do with the computational stencil, and the fact that we
cgg  are diagonally offset. Could check later by employing both
cgg  BALANCE_CONTROL_VOLFLUX and VOLFLUX_COST_CONTRIBUTION.

c--     Loop over this thread tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi

c--         Determine the weights to be used.
              fctile = 0. _d 0

              do k = 1, Nr
                do i = imin,imax
                  j = OB_Js(I,bi,bj)
                  IF ( j.EQ.OB_indexNone ) j = 1
cgg    Barotropic velocity is stored in level 1.
                  tmpx = tmpfldxz(i,1,bi,bj)
                  if (maskS(i,j+1,k,bi,bj) .ne. 0.) then
cgg -- Positive is flux in.
                    fctile = fctile + tmpx* drF(k) *dxg(i,j+1,bi,bj)
     &                  * _hFacS(i,j+1,k,bi,bj)
                  endif
                enddo
              enddo

              sumvol         = sumvol + fctile
            enddo
          enddo
        endif

#endif

#ifdef ALLOW_OBCSW_CONTROL
cgg   Assume that nobcs=4, and V velocity is the 4th record. I cannot
cgg   think of a more general way to do this.
        if (iobcs.eq.3) then
          call active_read_yz( fnamefldw, tmpfldyz, irec, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

cgg  At this point, do not be concerned with the overlap halos.
cgg  From experience, there is no control contribution in the
cgg  velocity points outside the boundaries. This has something
cgg  to do with the computational stencil, and the fact that we
cgg  are diagonally offset. Could check later by employing both
cgg  BALANCE_CONTROL_VOLFLUX and VOLFLUX_COST_CONTRIBUTION.

c--     Loop over this thread tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi

c--         Determine the weights to be used.
              fctile = 0. _d 0

              do k = 1, Nr
                do j = jmin,jmax
                  i = OB_Iw(j,bi,bj)
                  IF ( i.EQ.OB_indexNone ) i = 1
cgg    Barotropic velocity is stored in the level 1.
                  tmpy = tmpfldyz(j,1,bi,bj)
                  if (maskW(i+1,j,k,bi,bj) .ne. 0.) then
cgg -- Positive is flux in.
                    fctile = fctile + tmpy* drF(k) *dyg(i+1,j,bi,bj)
     &                  * _hFacW(i+1,j,k,bi,bj)
                  endif
                enddo
              enddo

              sumvol         = sumvol + fctile
            enddo
          enddo
        endif

#endif

#ifdef ALLOW_OBCSE_CONTROL
cgg   Assume that nobcs=4, and V velocity is the 4th record. I cannot
cgg   think of a more general way to do this.
        if (iobcs.eq.3) then
          call active_read_yz( fnameflde, tmpfldyz, irec, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

cgg  At this point, do not be concerned with the overlap halos.
cgg  From experience, there is no control contribution in the
cgg  velocity points outside the boundaries. This has something
cgg  to do with the computational stencil, and the fact that we
cgg  are diagonally offset. Could check later by employing both
cgg  BALANCE_CONTROL_VOLFLUX and VOLFLUX_COST_CONTRIBUTION.

c--     Loop over this thread tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi

c--         Determine the weights to be used.
              fctile = 0. _d 0

              do k = 1, Nr
                do j = jmin,jmax
                  i = OB_Ie(j,bi,bj)
                  IF ( i.EQ.OB_indexNone ) i = 1
cgg    Barotropic velocity stored in level 1.
                  tmpy = tmpfldyz(j,1,bi,bj)
                  if (maskW(i,j,k,bi,bj) .ne. 0.) then
cgg -- Positive is flux in.
                    fctile = fctile - tmpy* drF(k) *dyg(i,j,bi,bj)
     &                  * _hFacW(i,j,k,bi,bj)
                  endif
                enddo
              enddo

              sumvol         = sumvol + fctile
            enddo
          enddo
        endif

#endif

      enddo
c--   End of loop over records.

c--   Do the global summation.
      _GLOBAL_SUM_RL( sumvol, mythid )
      objf_obcsvol =  wobcsvol * sumvol* sumvol

#endif /* BAROTROPIC_OBVEL_CONTROL */
#endif /* OBCS_VOLFLUX_COST_CONTRIBUTION */

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      return
      end
