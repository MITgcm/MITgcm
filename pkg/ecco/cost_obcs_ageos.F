#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_obcs_ageos( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_obcs_ageos
c     ==================================================================
c
c     o cost function contribution obc -- Ageostrophic boundary flow.
c
c     started: G. Gebbie gebbie@mit.edu 4-Feb-2003
c
c     warning: masks redundantly assume no gradient of topography at
c              boundary.
c
c     ==================================================================
c     SUBROUTINE cost_obcs_ageos
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
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
# include "CTRL_OBCS.h"
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_OBCS) && \
     defined (ALLOW_ECCO))

#ifdef OBCS_AGEOS_COST_CONTRIBUTION
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
      integer iltheta
      integer ilsalt
      integer iluvel
      integer ilvvel
      integer ip1, jp1

      _RL fctile
      _RL fcthread

      _RL rholoc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL xzgrdrho(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL yzgrdrho(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL xzdvel1   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL xzdvel2   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL yzdvel1   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL yzdvel2   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL maskxzageos   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL maskyzageos   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL dummy

      character*(80) fnametheta
      character*(80) fnamesalt
      character*(80) fnameuvel
      character*(80) fnamevvel

      logical doglobalread
      logical ladinit

      character*(MAX_LEN_MBUF) msgbuf

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

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

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_obcs_ageos: number of records to process = ',nmonsrec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif /* ECCO_VERBOSE */

      if (optimcycle .ge. 0) then
        iltheta = ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &    tbarfile(1:iltheta),'.',optimcycle
        ilsalt = ilnblnk( sbarfile )
        write(fnamesalt(1:80),'(2a,i10.10)')
     &    sbarfile(1:ilsalt),'.',optimcycle
        iluvel = ilnblnk( ubarfile )
        write(fnameuvel(1:80),'(2a,i10.10)')
     &    ubarfile(1:iluvel),'.',optimcycle
        ilvvel = ilnblnk( vbarfile )
        write(fnamevvel(1:80),'(2a,i10.10)')
     &    vbarfile(1:ilvvel),'.',optimcycle
      endif

      fcthread = 0. _d 0
      fctile   = 0. _d 0

cgg   Code safe: always initialize to zero.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do i = 1-OLx,sNx+OLx
              maskxzageos(i,k,bi,bj) = 0. _d 0
              xzdvel1(i,k,bi,bj)     = 0. _d 0
              xzdvel2(i,k,bi,bj)     = 0. _d 0
              xzgrdrho(i,k,bi,bj)    = 0. _d 0
            enddo
            do j = 1-OLy,sNy+OLy
              maskyzageos(j,k,bi,bj) = 0. _d 0
              yzdvel1(j,k,bi,bj)     = 0. _d 0
              yzdvel2(j,k,bi,bj)     = 0. _d 0
              yzgrdrho(j,k,bi,bj)    = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1-OLy,sNy+OLy
            do i = 1-OLx,sNx+OLx
              rholoc(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

c--   Loop over records.
      do irec = 1,nmonsrec

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_tbar_mean_dummy )

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnamesalt, sbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_sbar_mean_dummy )

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnameuvel, ubar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_ubar_mean_dummy )

c--     Read time averages and the monthly mean data.
        call active_read_xyz( fnamevvel, vbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_vbar_mean_dummy )

cgg     Minor problem : grad T,S needs overlap values.
        _BARRIER
        _EXCH_XYZ_RL(tbar,myThid)
        _EXCH_XYZ_RL(sbar,myThid)

        do bj = jtlo,jthi
          do bi = itlo,ithi

#ifdef ALLOW_OBCSN_CONTROL
            jp1 = 0

cgg    Make a mask for the velocity shear comparison.
            do k = 1,Nr-1
              do i = imin, imax
                j = OB_Jn(i,bi,bj)
cgg    All these points need to be wet.
                if ( j.eq.OB_indexNone ) then
                  maskxzageos(i,k,bi,bj) = 0.
                else
                  maskxzageos(i,k,bi,bj) =
     &       hfacC(i,j+jp1,k,bi,bj)*hfacC(i+1,j+jp1,k,bi,bj) *
     &       hfacC(i-1,j+jp1,k,bi,bj)*hfacC(i,j+jp1,k+1,bi,bj)*
     &       hfacC(i-1,j+jp1,k+1,bi,bj)*hfacC(i+1,j+jp1,k+1,bi,bj)*
     &       hfacS(i,j+jp1,k,bi,bj)*hfacS(i,j+jp1,k+1,bi,bj)
                endif
              enddo
            enddo

            do k = 1,Nr

C-- jmc: both calls below are wrong if more than 1 tile => stop here
       IF ( bi.NE.1 .OR. bj.NE.1 )
     &  STOP 'COST_OBCS_AGEOS wrong with more than 1 tile/proc'
              call find_rho_2d(
     I                         iMin, iMax, jMin, jMax, k,
     I                         tbar(1-OLx,1-OLy,k,bi,bj),
     I                         sbar(1-OLx,1-OLy,k,bi,bj),
     O                         rholoc,
     I                         k, bi, bj, myThid )
              _EXCH_XY_RL(rholoc , myThid)

cgg   Compute centered difference horizontal gradient on bdy.
              do i = imin, imax
                j = OB_Jn(i,bi,bj)
                if ( j.eq.OB_indexNone ) j = 1
                  xzgrdrho(i,k,bi,bj) =
     &            (rholoc(i-1,j+jp1,bi,bj)-rholoc(i+1,j+jp1,bi,bj))
     &            /(2.*dxc(i,j+jp1,bi,bj))
              enddo
            enddo

cgg         Compute vertical shear from geostrophy/thermal wind.
cgg         Above level 4 needs not be geostrophic.
cgg         Please get rid of the "4" ASAP. Ridiculous.
            do k = 4,Nr-1
              do i = imin,imax
                j = OB_Jn(i,bi,bj)
                if ( j.eq.OB_indexNone ) j = 1
                  xzdvel1(i,k,bi,bj) = vbar(i,j+jp1,k  ,bi,bj)
     &                               - vbar(i,j+jp1,k+1,bi,bj)
                 xzdvel2(i,k,bi,bj)=((xzgrdrho(i,k,bi,bj)*delz(k)/2.)+
     &                             (xzgrdrho(i,k+1,bi,bj)*delz(k+1)/2.))
     &                        * gravity / f0 / rhonil

                  fctile = fctile + 100*wcurrent(k,bi,bj) *
     &            maskxzageos(i,k,bi,bj)*
     &          (xzdvel2(i,k,bi,bj) - xzdvel1(i,k,bi,bj))*
     &          (xzdvel2(i,k,bi,bj) - xzdvel1(i,k,bi,bj))
      if (maskxzageos(i,k,bi,bj) .ne. 0) then
cgg      print*,'XX shear,geos shear',xzdvel1(i,k,bi,bj),xzdvel2(i,k,bi,bj)
cgg      print*,'i,j,k,fctile N',i,j,k,fctile
      endif
              enddo
            enddo
c--         End of loop over layers.
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
            jp1 = 1

cgg    Make a mask for the velocity shear comparison.
            do k = 1,Nr-1
              do i = imin, imax
                j = OB_Js(i,bi,bj)
                if ( j.eq.OB_indexNone ) then
                  maskxzageos(i,k,bi,bj) = 0.
                else
cgg    All these points need to be wet.
                  maskxzageos(i,k,bi,bj) =
     &       hfacC(i,j+jp1,k,bi,bj)*hfacC(i+1,j+jp1,k,bi,bj) *
     &       hfacC(i-1,j+jp1,k,bi,bj)*hfacC(i,j+jp1,k+1,bi,bj)*
     &       hfacC(i-1,j+jp1,k+1,bi,bj)*hfacC(i+1,j+jp1,k+1,bi,bj)*
     &       hfacS(i,j+jp1,k,bi,bj)*hfacS(i,j+jp1,k+1,bi,bj)
                endif
              enddo
            enddo

            do k = 1,Nr

C-- jmc: both calls below are wrong if more than 1 tile => stop here
       IF ( bi.NE.1 .OR. bj.NE.1 )
     &  STOP 'COST_OBCS_AGEOS wrong with more than 1 tile/proc'
              call find_rho_2d(
     I                         iMin, iMax, jMin, jMax, k,
     I                         tbar(1-OLx,1-OLy,k,bi,bj),
     I                         sbar(1-OLx,1-OLy,k,bi,bj),
     O                         rholoc,
     I                         k, bi, bj, myThid )

              _EXCH_XY_RL(rholoc , myThid)

cgg   Compute centered difference horizontal gradient on bdy.
               do i = imin, imax
                 j = OB_Js(i,bi,bj)
                 if ( j.eq.OB_indexNone ) j = 1
                 xzgrdrho(i,k,bi,bj) =
     &            (rholoc(i-1,j+jp1,bi,bj)-rholoc(i+1,j+jp1,bi,bj))
     &            /(2.*dxc(i,j+jp1,bi,bj))
               enddo
             enddo

cgg         Compute vertical shear from geostrophy/thermal wind.
             do k = 4,Nr-1
               do i = imin,imax
                 j = OB_Js(i,bi,bj)
                 if ( j.eq.OB_indexNone ) j = 1
cgg         Retrieve the model vertical shear.
                 xzdvel1(i,k,bi,bj) = vbar(i,j+jp1,k  ,bi,bj)
     &                               - vbar(i,j+jp1,k+1,bi,bj)

cgg         Compute vertical shear from geostrophy/thermal wind.
                 xzdvel2(i,k,bi,bj) =((xzgrdrho(i,k,bi,bj)*delz(k)/2.)+
     &                             (xzgrdrho(i,k+1,bi,bj)*delz(k+1)/2.))
     &                        * gravity / f0 /rhonil

cgg   Make a comparison.
                  fctile = fctile + 100*wcurrent(k,bi,bj) *
     &          maskxzageos(i,k,bi,bj)*
     &          (xzdvel2(i,k,bi,bj) - xzdvel1(i,k,bi,bj))*
     &          (xzdvel2(i,k,bi,bj) - xzdvel1(i,k,bi,bj))
cgg        print*,'fctile S',fctile
              enddo
            enddo
c--         End of loop over layers.
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
            ip1 = 1

cgg    Make a mask for the velocity shear comparison.
            do k = 1,Nr-1
              do j = jmin, jmax
                i = OB_Iw(j,bi,bj)
cgg    All these points need to be wet.
                if ( i.eq.OB_indexNone ) then
                  maskyzageos(j,k,bi,bj) = 0.
                else
                  maskyzageos(j,k,bi,bj) =
     &       hfacC(i+ip1,j,k,bi,bj)*hfacC(i+ip1,j+1,k,bi,bj) *
     &       hfacC(i+ip1,j-1,k,bi,bj)*hfacC(i+ip1,j,k+1,bi,bj)*
     &       hfacC(i+ip1,j-1,k+1,bi,bj)*hfacC(i+ip1,j+1,k+1,bi,bj)*
     &       hfacW(i+ip1,j,k,bi,bj)*hfacW(i+ip1,j,k+1,bi,bj)
                endif
              enddo
            enddo

            do k = 1,Nr

       IF ( bi.NE.1 .OR. bj.NE.1 )
     &  STOP 'COST_OBCS_AGEOS wrong with more than 1 tile/proc'
              call find_rho_2d(
     I                         iMin, iMax, jMin, jMax, k,
     I                         tbar(1-OLx,1-OLy,k,bi,bj),
     I                         sbar(1-OLx,1-OLy,k,bi,bj),
     O                         rholoc,
     I                         k, bi, bj, myThid )
              _EXCH_XY_RL(rholoc , myThid)

cgg   Compute centered difference horizontal gradient on bdy.
              do j = jmin, jmax
                i = OB_Iw(j,bi,bj)
                if ( i.eq.OB_indexNone ) i = 1
cgg             Negative sign due to geostrophy.
                yzgrdrho(j,k,bi,bj) =
     &            (rholoc(i+ip1,j+1,bi,bj)-rholoc(i+ip1,j-1,bi,bj))
     &            /(2.*dyc(i+ip1,j,bi,bj))
              enddo
            enddo

cgg         Compute vertical shear from geostrophy/thermal wind.
            do k = 4,Nr-1
              do j = jmin,jmax
                i = OB_Iw(j,bi,bj)
                if ( i.eq.OB_indexNone ) i = 1
cgg         Retrieve the model vertical shear.
                yzdvel1(j,k,bi,bj) = ubar(i+ip1,j,k  ,bi,bj)
     &                               - ubar(i+ip1,j,k+1,bi,bj)

cgg         Compute vertical shear from geostrophy/thermal wind.
                yzdvel2(j,k,bi,bj) =((yzgrdrho(j,k  ,bi,bj)*delz(k)/2.)+
     &                             (yzgrdrho(j,k+1,bi,bj)*delz(k+1)/2.))
     &                        * gravity / f0 / rhonil

cgg   Make a comparison.
                fctile = fctile + 100*wcurrent(k,bi,bj) *
     &           maskyzageos(j,k,bi,bj) *
     &          (yzdvel2(j,k,bi,bj) - yzdvel1(j,k,bi,bj))*
     &          (yzdvel2(j,k,bi,bj) - yzdvel1(j,k,bi,bj))
              enddo
            enddo
c--         End of loop over layers.
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
            ip1 = 0

cgg    Make a mask for the velocity shear comparison.
            do k = 1,Nr-1
              do j = jmin, jmax
                i = OB_Ie(j,bi,bj)
                if ( i.eq.OB_indexNone ) then
                  maskyzageos(j,k,bi,bj) =0.
                else
cgg    All these points need to be wet.
                  maskyzageos(j,k,bi,bj) =
     &       hfacC(i+ip1,j,k,bi,bj)*hfacC(i+ip1,j+1,k,bi,bj) *
     &       hfacC(i+ip1,j-1,k,bi,bj)*hfacC(i+ip1,j,k+1,bi,bj)*
     &       hfacC(i+ip1,j-1,k+1,bi,bj)*hfacC(i+ip1,j+1,k+1,bi,bj)*
     &       hfacW(i+ip1,j,k,bi,bj)*hfacW(i+ip1,j,k+1,bi,bj)
                endif
              enddo
            enddo

            do k = 1,Nr

       IF ( bi.NE.1 .OR. bj.NE.1 )
     &  STOP 'COST_OBCS_AGEOS wrong with more than 1 tile/proc'
              call find_rho_2d(
     I                         iMin, iMax, jMin, jMax, k,
     I                         tbar(1-OLx,1-OLy,k,bi,bj),
     I                         sbar(1-OLx,1-OLy,k,bi,bj),
     O                         rholoc,
     I                         k, bi, bj, myThid )
              _EXCH_XY_RL(rholoc , myThid)

cgg   Compute centered difference horizontal gradient on bdy.
              do j = jmin, jmax
                i = OB_Ie(j,bi,bj)
                if ( i.eq.OB_indexNone ) i = 1
cgg             Negative sign due to geostrophy.
                yzgrdrho(j,k,bi,bj) =
     &            (rholoc(i+ip1,,j+1,bi,bj)-rholoc(i+ip1,j-1,bi,bj))
     &            /(2.*dyc(i+ip1,j,bi,bj))
              enddo
            enddo

cgg         Compute vertical shear from geostrophy/thermal wind.
            do k = 4,Nr-1
              do j = jmin,jmax
                i = OB_Ie(j,bi,bj)
                if ( i.eq.OB_indexNone ) i = 1
cgg         Retrieve the model vertical shear.
                yzdvel1(j,k,bi,bj) = ubar(i+ip1,j,k  ,bi,bj)
     &                             - ubar(i+ip1,j,k+1,bi,bj)

cgg         Compute vertical shear from geostrophy/thermal wind.
                yzdvel2(j,k,bi,bj) =((yzgrdrho(j,k  ,bi,bj)*delz(k)/2.)+
     &                             (yzgrdrho(j,k+1,bi,bj)*delz(k+1)/2.))
     &                        * gravity / f0 /rhonil

cgg   Make a comparison.
                fctile = fctile + 100*wcurrent(k,bi,bj) *
     &           maskyzageos(j,k,bi,bj) *
     &          (yzdvel2(j,k,bi,bj) - yzdvel1(j,k,bi,bj))*
     &          (yzdvel2(j,k,bi,bj) - yzdvel1(j,k,bi,bj))

              enddo
            enddo
c--         End of loop over layers.
#endif /* ALLOW_OBCSE_CONTROL */

            fcthread          = fcthread          + fctile
            objf_ageos(bi,bj) = objf_ageos(bi,bj) + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_Theta: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '     cost function (temperature) = ',
     &        fctile
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
#endif /* ECCO_VERBOSE */

          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_RL( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_Theta: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (temperature) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif /* ECCO_VERBOSE */

      enddo
c--   End of loop over records.

#endif /* OBCS_AGEOS_COST_CONTRIBUTION */

#endif /* ALLOW_CTRL, ALLOW_OBCS and ALLOW_ECCO */

      return
      end
