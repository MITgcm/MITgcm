C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init.F,v 1.1 2001/03/25 22:33:55 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_Init(
     I                      mythid
     &                    )

c     ==================================================================
c     SUBROUTINE ctrl_Init
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c       The vector of control variables is initialized here. The
c       temperature and salinity contributions are read from file.
c       Subsequently, the latter are dimensionalized and the tile
c       edges are updated.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Feb-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Patrick Heimbach heimbach@mit.edu 30-May-2000
c              - diffsec was falsely declared.
c
c              Patrick Heimbach heimbach@mit.edu 06-Jun-2000
c              - Transferred some filename declarations
c                from ctrl_pack/ctrl_unpack to here
c              - Transferred mask-per-tile to here
c              - computation of control vector length here
c
c              Patrick Heimbach heimbach@mit.edu 16-Jun-2000
c              - Added call to ctrl_pack
c              - Alternatively: transfer writing of scale files to
c                ctrl_unpack
c
c     ==================================================================
c     SUBROUTINE ctrl_Init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ntmp

      integer il
      integer errio
      integer startrec
      integer endrec

      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Read the namelist input.
      namelist /ctrl_nml/
     &  xx_theta_file,
     &  xx_salt_file,
     &  xx_tauu_file,
     &  xx_tauv_file,
     &  xx_sflux_file,
     &  xx_hflux_file,
     &  xx_sss_file,
     &  xx_sst_file,
     &  xx_diffkr_file,
     &  xx_kapgm_file

      namelist /ctrl_packnames/
     &  yadmark, expId,
     &  ctrlname, costname, scalname, maskname, metaname

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      _BEGIN_MASTER( myThid )

c--     Set default values.
        xx_theta_file      = ' '
        xx_salt_file       = ' '
        xx_tauu_file       = ' '
        xx_tauv_file       = ' '
        xx_sflux_file      = ' '
        xx_hflux_file      = ' '
        xx_sss_file        = ' '
        xx_sst_file        = ' '
        xx_diffkr_file     = ' '
        xx_kapgm_file      = ' '
        yadmark            = 'ad'
        expId              = ' '
        ctrlname           = ' '
        costname           = ' '
        scalname           = ' '
        maskname           = ' '
        metaname           = ' '

c--     Check versions.

        open(unit=scrunit1,status='scratch')

c--     Next, read the ecco data file.
        open(unit = modeldataunit,file = 'data.ctrl',
     &       status = 'old', iostat = errio)
        if ( errio .lt. 0 ) then
          stop ' stopped in ctrl_init'
        endif

        do while ( .true. )
          read(modeldataunit, fmt='(a)', end=1001) record
          il = max(ilnblnk(record),1)
          if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
        enddo
 1001   continue
        close( modeldataunit )

        rewind( scrunit1 )
        read(unit = scrunit1, nml = ctrl_nml)
        read(unit = scrunit1, nml = ctrl_packnames)
        close( scrunit1 )

c--     Set default values.
        do i = 1,maxcvars
          ncvarindex(i) = -1
          ncvarrecs(i)  =  0
          ncvarxmax(i)  =  0
          ncvarymax(i)  =  0
          ncvarnrmax(i) =  0
          ncvargrd(i)   = '?'
        enddo

        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    ' ctrl_init: Initializing temperature and salinity'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    '                   part of the control vector.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    '                   The initial surface fluxes are set',
     &                      ' to zero.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )

      _BARRIER

c--   =====================
c--   Initial state fields.
c--   =====================

#ifdef ALLOW_THETA0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(1) = 101
        ncvarrecs(1)  =   1
        ncvarxmax(1)  = snx
        ncvarymax(1)  = sny
        ncvarnrmax(1) =  nr
        ncvargrd(1)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_THETA0_CONTROL */

#ifdef ALLOW_SALT0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(2) = 102
        ncvarrecs(2)  =   1
        ncvarxmax(2)  = snx
        ncvarymax(2)  = sny
        ncvarnrmax(2) =  nr
        ncvargrd(2)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SALT0_CONTROL */

#ifdef ALLOW_HFLUX0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(3) = 103
        ncvarrecs(3)  =   1
        ncvarxmax(3)  = snx
        ncvarymax(3)  = sny
        ncvarnrmax(3) =   1
        ncvargrd(3)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_HFLUX0_CONTROL */

#ifdef ALLOW_SFLUX0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(4) = 104
        ncvarrecs(4)  =   1
        ncvarxmax(4)  = snx
        ncvarymax(4)  = sny
        ncvarnrmax(4) =   1
        ncvargrd(4)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SFLUX0_CONTROL */

#ifdef ALLOW_TAUU0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(5) = 105
        ncvarrecs(5)  =   1
        ncvarxmax(5)  = snx
        ncvarymax(5)  = sny
        ncvarnrmax(5) =   1
        ncvargrd(5)   = 'w'
      _END_MASTER( mythid )
#endif /* ALLOW_TAUU0_CONTROL */

#ifdef ALLOW_TAUV0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(6) = 106
        ncvarrecs(6)  =   1
        ncvarxmax(6)  = snx
        ncvarymax(6)  = sny
        ncvarnrmax(6) =   1
        ncvargrd(6)   = 's'
      _END_MASTER( mythid )
#endif /* ALLOW_TAUV0_CONTROL */

#ifdef ALLOW_SST0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(7) = 107
        ncvarrecs(7)  =   1
        ncvarxmax(7)  = snx
        ncvarymax(7)  = sny
        ncvarnrmax(7) =   1
        ncvargrd(7)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SST0_CONTROL */

#ifdef ALLOW_SSS0_CONTROL
      _BEGIN_MASTER( mythid )
        ncvarindex(8) = 108
        ncvarrecs(8)  =   1
        ncvarxmax(8)  = snx
        ncvarymax(8)  = sny
        ncvarnrmax(8) =   1
        ncvargrd(8)   = 'c'
      _END_MASTER( mythid )
#endif /* ALLOW_SSS0_CONTROL */

c--   Determine the number of wet points in each tile:
c--   maskc, masks, and maskw.

c--   Set loop ranges.
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Initialise the counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            nwetctile(bi,bj,k) = 0
            nwetstile(bi,bj,k) = 0
            nwetwtile(bi,bj,k) = 0
          enddo
        enddo
      enddo

c--   Count wet points on each tile.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
c--             Center mask.
                if (hFacC(i,j,k,bi,bj) .ne. 0.) then
                  nwetctile(bi,bj,k) = nwetctile(bi,bj,k) + 1
                endif
c--             South mask.
                if (maskS(i,j,k,bi,bj) .eq. 1.) then
                  nwetstile(bi,bj,k) = nwetstile(bi,bj,k) + 1
                endif
c--             West mask.
                if (maskW(i,j,k,bi,bj) .eq. 1.) then
                  nwetwtile(bi,bj,k) = nwetwtile(bi,bj,k) + 1
                endif
              enddo
            enddo
          enddo
        enddo
      enddo


      _BEGIN_MASTER( mythid )
c--   Determine the total number of control variables.
      nvartype   = 0
      nvarlength = 0
      do i = 1,maxcvars
         if ( ncvarindex(i) .ne. -1 ) then
            nvartype = nvartype + 1
            do bj = jtlo,jthi
               do bi = itlo,ithi
                  if ( ncvargrd(i) .eq. 'c' ) then
                     do k = 1,ncvarnrmax(i)
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetctile(bi,bj,k)
                     enddo
                  else if ( ncvargrd(i) .eq. 's' ) then
                     do k = 1,ncvarnrmax(i)
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetstile(bi,bj,k)
                     enddo
                  else if ( ncvargrd(i) .eq. 'w' ) then
                     do k = 1,ncvarnrmax(i)
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetwtile(bi,bj,k)
                     enddo
                  else
                     print*,'ctrl_init: invalid grid location'
                     print*,'     control variable = ',ncvarindex(i)
                     print*,'     grid location    = ',ncvargrd(i)
                     stop   ' ... stopped in ctrl_init'
                  endif
               enddo
            enddo
         endif
      enddo
      
c
c     Summation of wet point counters 
c
      CALL GLOBAL_SUM_INT( nvarlength,  myThid )
      ntmp=0
      do bj=1,nSy
         do bi=1,nSx
            ntmp=ntmp+nWetcTile(bi,bj,k)
         enddo
      enddo
      CALL GLOBAL_SUM_INT( ntmp,  myThid )
      nWetcTile(1,1,k)=ntmp
      ntmp=0
      do bj=1,nSy
         do bi=1,nSx
            ntmp=ntmp+nWetsTile(bi,bj,k)
         enddo
      enddo
      CALL GLOBAL_SUM_INT( ntmp,  myThid )
      nWetsTile(1,1,k)=ntmp
      ntmp=0
      do bj=1,nSy
         do bi=1,nSx
            ntmp=ntmp+nWetwTile(bi,bj,k)
         enddo
      enddo
      CALL GLOBAL_SUM_INT( ntmp,  myThid )
      nWetwTile(1,1,k)=ntmp

      print*, 'ctrl_init: no. of control variables: ', nvartype
      print*, 'ctrl_init: control vector length:    ', nvarlength
      _END_MASTER( mythid )

      _BARRIER

      return
      end

