
#include "COST_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine cost_obcsn(
     I                       myiter,
     I                       mytime,
     I                       startrec,
     I                       endrec,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_obcsn
c     ==================================================================
c
c     o cost function contribution obc
c
c     o G. Gebbie, gebbie@mit.edu, 18-Mar-2003
c     ==================================================================
c     SUBROUTINE cost_obcsn
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
cgg(  
      integer startrec
      integer endrec
cgg)

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
      integer jp1
      integer nrec
      integer ilfld
      integer igg
      integer ihh

      _RL fctile
      _RL fcthread
      _RL dummy
      _RL gg
      _RL tmpx
      _RL tmpfield (1-olx:snx+olx,nr,nsx,nsy)
      _RL barofield (1-olx:snx+olx,nsx,nsy)
      _RL maskxz   (1-olx:snx+olx,nr,nsx,nsy)
      _RL vtop
      _RL area
      _RL volflux

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

#ifdef ALLOW_OBCSN_COST_CONTRIBUTION

      jp1 = 0
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
     &  ' cost_obcsn: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
        ilfld=ilnblnk( xx_obcsn_file )
        write(fnamefld(1:80),'(2a,i10.10)') 
     &       xx_obcsn_file(1:ilfld), '.', optimcycle
      endif

c--   Loop over records.
      do irec = 1,nrec

        area    = 0. _d 0
        volflux = 0. _d 0
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do i = imin,imax
              barofield(i,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo

        call active_read_xz( fnamefld, tmpfield, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_obcsn_dummy )

cgg    Need to solve for iobcs would have been.
          gg    = (irec-1)/nobcs
          igg   = int(gg)
          iobcs = irec - igg*nobcs

          call active_read_xz( 'maskobcsn', maskxz, 
     &                         iobcs,
     &                         doglobalread, ladinit, 0,
     &                         mythid, dummy )

cgg     Need to change xx field to baroclinic vel.
cgg     Level Nr contains barotropic vel, remove it.
cgg     The deepest wet level velocity should be 
cgg     computed in order to ensure zero integrated flux.
        if (iobcs .eq. 3 .or. iobcs .eq. 4) then
          if (iobcs .eq .3) then
            ihh = igg+1   
            call ctrl_volflux( ihh, area, volflux, mythid)
            _GLOBAL_SUM_R8( volflux, mythid )
            _GLOBAL_SUM_R8( area,mythid )
            print*,'volflux,area',volflux,area
          endif

c--     Loop over this thread's tiles.
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do i = imin,imax

cgg         The barotropic velocity is stored in level 1.
cgg         Penalize for deviation in it.
                barofield(i,bi,bj) = tmpfield(i,1,bi,bj)
     &                             * maskxz(i,1,bi,bj)

                vtop = 0.d0
                tmpfield(i,1,bi,bj) = 0.d0

                j = OB_Jn(I,bi,bj)
                if (iobcs .eq. 3) then
                  if (ob_iw(j,bi,bj).eq.(i-1).and.
     &                ob_iw(j,bi,bj).ne. 0) then
                    barofield(i,bi,bj)= volflux/area
     &                                * maskxz(i,1,bi,bj)
                    print*,'volflux2,area2',volflux,area
                    print*,'barofield',barofield(i,bi,bj)
                  endif
                endif

                do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    lowest wet level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                  vtop = tmpfield(i,k,bi,bj)*
     &               maskxz(i,k,bi,bj) * delZ(k) + vtop
                enddo
                tmpfield(i,1,bi,bj) = -vtop / delZ(1)
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
              do i = imin,imax
                j = OB_Jn(I,bi,bj)
                tmpx = barofield(i,bi,bj)
                  fctile = fctile
     &               + wbaro*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
     &                 *maskxz(i,1,bi,bj)
cgg                print*,'S bt fctile',i,j,wbaro,
cgg     &                 barofield(i,bi,bj),fctile
              enddo
            endif

            do k = 1, Nr
              do i = imin,imax
                 j = OB_Jn(I,bi,bj)
cgg                 if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfield(i,k,bi,bj)
                  fctile = fctile
     &                 + wobcsnLev(i,k,bi,bj,iobcs)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
     &                 *maskxz(i,k,bi,bj)
cgg                endif
cgg                print*,'S fctile',fctile
              enddo
            enddo

            objf_obcsn(bi,bj) = objf_obcsn(bi,bj) + fctile
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
     &    ' cost_obcsn: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcsn) = ',fcthread
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







