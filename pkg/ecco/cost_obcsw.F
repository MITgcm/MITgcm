
#include "COST_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine cost_obcsw(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_obcsw
c     ==================================================================
c
c     o cost function contribution obc
c
c     ==================================================================
c     SUBROUTINE cost_obcsw
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBCS
# include "OBCS.h"
#endif

#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid
      integer startrec
      integer endrec

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer il
      integer iobcs
      integer ip1
      integer nrec
      integer ilfld
      integer igg

      _RL fctile
      _RL fcthread
      _RL dummy
      _RL gg
      _RL tmpx
cgg(
      _RL tmpfield (1-oly:sny+oly,nr,nsx,nsy)
      _RL barofield (1-oly:sny+oly,nsx,nsy)
      _RL maskyz   (1-oly:sny+oly,nr,nsx,nsy)
      _RL utop

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

c     Number of records to be used.
      nrec = endrec-startrec+1

#ifdef ALLOW_OBCSW_COST_CONTRIBUTION

      ip1 = 1
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
     &  ' cost_obcsw: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_obcsw_file )
        write(fnamefld(1:80),'(2a,i10.10)') 
     &       xx_obcsw_file(1:ilfld), '.', optimcycle
      endif

c--   Loop over records.
      do irec = 1,nrec

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              barofield(j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo

        call active_read_yz( fnamefld, tmpfield, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_obcsw_dummy )

cgg    Need to solve for iobcs would have been.
          gg    = (irec-1)/nobcs
          igg   = int(gg)
          iobcs = irec - igg*nobcs

          call active_read_yz( 'maskobcsw', maskyz, 
     &                         iobcs,
     &                         doglobalread, ladinit, 0,
     &                         mythid, dummy )

cgg     Need to change xx field to baroclinic vel.
cgg     Level Nr contains barotropic vel, remove it.
cgg     The deepest wet level velocity should be 
cgg     computed in order to ensure zero integrated flux.
        if (iobcs .eq. 3 .or. iobcs .eq. 4) then
c--     Loop over this thread's tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do j = jmin,jmax

cgg         The barotropic velocity is stored in level 1.
cgg         Penalize for deviation in it.
                barofield(j,bi,bj) = tmpfield(j,1,bi,bj)
     &                             * maskyz(j,1,bi,bj)

                tmpfield(j,1,bi,bj) = 0.d0
                utop = 0.d0

                do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    lowest wet level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                  utop = tmpfield(j,k,bi,bj)*
     &               maskyz(j,k,bi,bj) * delZ(k) + utop
                enddo
                tmpfield(j,1,bi,bj) = -utop / delZ(1)
              enddo
            enddo
          enddo

        endif

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            fctile = 0. _d 0

            if (iobcs .eq. 3 .or. iobcs .eq. 4) then
              do j = jmin,jmax
                i = OB_Iw(j,bi,bj)
                tmpx = barofield(j,bi,bj)
                  fctile = fctile
     &               + wbaro*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
     &                 *maskyz(j,1,bi,bj)
cgg                print*,'fctile barotropic W',fctile
              enddo
            endif

            do k = 1, Nr
              do j = jmin,jmax
                i = OB_Iw(j,bi,bj)
cgg                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfield(j,k,bi,bj)
                  fctile = fctile
     &                 + wobcswLev(j,k,bi,bj,iobcs)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
     &                 *maskyz(j,k,bi,bj)
cgg                endif
              enddo
            enddo

            objf_obcsw(bi,bj) = objf_obcsw(bi,bj) + fctile
            fcthread         = fcthread + fctile
          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_obcsw: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcsw) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#endif

      return
      end







