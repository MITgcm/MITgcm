#include "CPP_OPTIONS.h"

      subroutine adecco_check_exp( 
     &     mythid, mycurrentiter, mycurrenttime, yprefix )

c     =================================================================
c     SUBROUTINE adecco_check_exp
c     =================================================================
c
c     o Check details of the model run
c
c       This routine dumps a collection of model fields for diagnostic
c       or testimg purposes, respectively.
c
c     Variables for experiment 06:
c
c     Dynamical core:
c       Potential temperature                      theta        [C]
c       Salinity                                   salt         [psu]
c       Zonal velocity                             uvel         [m/s]
c       Meridional velocity                        vvel         [m/s]
c       Vertical velocity ( --> check_fld)         rvel         [m/s]
c       Surface pressure                           cg2d_x       [m]
c       Surface heat flux                          qnet         [K/s]
c       Qnet contrib. from external forcing        tflux        [K/s]
c       Qnet contrib. from relaxation to Levitus   qlev         [K/s]
c       Qnet contrib. from relaxation to Reynolds  qrey         [K/s]
c       Surface virtual salt flux                  empmr        [psu/s]
c       Surface zonal wind stress                  fu           [m/s^2]
c       Surface meridional wind stress             fv           [m/s^2]
c
c     Control vector contributions:
c       Heat flux correction                       xx_hflux     [W/m^2]
c       Virtual salt flux correction               xx_sflux     [psu/s/m^2]
c       Zonal wind stress correction               xx_tauu      [N/m^2]
c       Meridional wind stress correction          xx_tauv      [N/m^2]
c
c     Bulk formulae:
c       Atmospheric zonal wind                     uwind        [m/s]
c       Atmospheric meridional wind                vwind        [m/s]
c       Air temperature                            atemp        [K]
c       Specific humidity                          aqh          [kg/kg]
c       Precipitation                              precip       [kg/s/m^2]
c       Short wave radiative flux                  swflux/qsw   [W/m^2]
c       Long wave radiative flux                   lwflux/qlw   [W/m^2]
c
c     Non-local K-Profile Parameterization (KPP):
c       Short wave radiative flux                  swflux/qsw   [W/m^2]
c       Boundary layer depth                       kpphbl       [m]
c
c
c     Beta Version: Christian Eckert (MIT) 15-Nov-1999
c
c     =================================================================
c     SUBROUTINE adecco_check_exp
c     =================================================================

      implicit none

c--   == global variables ==

cph#ifdef ALLOW_SNAPSHOTS

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
cph#include "CG2D_EXTERNAL.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "ctrl.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "adcommon.h"
#endif

#ifdef ALLOW_KPP
# include "KPP_OPTIONS.h"
# include "KPP_PARAMS.h"
# include "KPP.h"
#endif

cph#endif

c     == routine arguments ==
c     mythid        - thread number for this instance of the routine.

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime
      character yprefix*3

cph#ifdef ALLOW_SNAPSHOTS

#ifdef ALLOW_AUTODIFF_MONITOR

c--   == local variables ==

      INTEGER bi,bj,i,j
      integer irec
      integer mydate(4)
      character yfname*128

      _RS tmpflux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c     == end of interface ==

      irec = 0

      if ( mod((mycurrentiter),1) .eq. 0 ) then
        irec = (mycurrentiter)/1 + 1

cph(
cph        call cal_GetDate(
cph     I                    mycurrentiter,
cph     I                    mycurrenttime,
cph     O                    mydate,
cph     I                    mythid
cph     &                  )

      print *, 'pathei: in check_exp: iter/time/rec/yprefix ', 
     &       mycurrentiter, mycurrenttime, irec, ' ', yprefix
      print *, 'pathei: in check_exp: date  ', mycurrentiter
      print *, 'pathei: in check_exp: adtheta ', adtheta(10,10,1,1,1)
      print *, 'pathei: in check_exp: adsalt  ', adsalt(10,10,1,1,1)
      print *, 'pathei: in check_exp: aduvel  ', aduvel(10,10,1,1,1)
      print *, 'pathei: in check_exp: advvel  ', advvel(10,10,1,1,1)
      print *, 'pathei: in check_exp: adqnet  ', adqnet(10,10,1,1)
      print *, 'pathei: in check_exp: adempmr ', adempmr(10,10,1,1)
      print *, 'pathei: in check_exp: adfu    ', adfu(10,10,1,1)
      print *, 'pathei: in check_exp: adfv    ', adfv(10,10,1,1)
      print *, 'pathei: in check_exp: adetan  ', adetan(10,10,1,1)
#ifdef ALLOW_PASSIVE_TRACER
      print *, 'pathei: in check_exp: adtr1   ', adtr1(10,10,1,1,1)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
      print *, 'pathei: in check_exp: addiffkr', addiffkr(10,10,2,1,1)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      print *, 'pathei: in check_exp: adkapgm ', adkapgm(10,10,1,1,1)
#endif
cph)

c--     Potential temperature:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adtheta'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, adtheta, irec,
     &                      mycurrentiter, mythid )

c--     Salinity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adsalt'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, adsalt, irec,
     &                      mycurrentiter, mythid )

c--     Zonal velocity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_aduvel'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, aduvel, irec,
     &                      mycurrentiter, mythid )

c--     Meridional velocity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_advvel'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, advvel, irec,
     &                      mycurrentiter, mythid )

c--     Surface pressure:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adetan'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', 1, adetan, irec,
     &                      mycurrentiter, mythid )

#ifdef ALLOW_PASSIVE_TRACER
c--     Passive tracer:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adtr1'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, adtr1, irec,
     &                      mycurrentiter, mythid )
#endif

#ifdef ALLOW_DIFFKR_CONTROL
c--     diapycnal diffusion:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_addiffkr'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, addiffkr, irec,
     &                      mycurrentiter, mythid )
#endif

#ifdef ALLOW_KAPGM_CONTROL
c--     isopycnal diffusion:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adkapgm'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, adkapgm, irec,
     &                      mycurrentiter, mythid )
#endif

c--     Surface heat flux:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &           - adQnet(i,j,bi,bj)*HeatCapacity_Cp*rhoNil*dRf(1)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adqnet'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface virtual salt flux:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &           adEmPmR(i,j,bi,bj)*dRf(1)/35.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adempmr'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface zonal wind stress:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &            -adfu(i,j,bi,bj)*rhoNil*dRf(1)/horiVertRatio
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adfu'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface meridional wind stress:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &            -adfv(i,j,bi,bj)*rhoNil*dRf(1)/horiVertRatio
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_adfv'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Control vector contributions:

c--     Heat flux (control):
cph        call mdswritefield( yprefix//'snapshot_xx_hfl', 32, .false.,
cph     &                      'RS', 1, xx_hfl, irec,
cph     &                      mycurrentiter, mythid )

c--     Virtual salt flux (control):
cph        call mdswritefield( yprefix//'snapshot_xx_sfl', 32, .false.,
cph     &                      'RS', 1, xx_sfl, irec,
cph     &                      mycurrentiter, mythid )

c--     Zonal wind stress (control):
cph        call mdswritefield( yprefix//'snapshot_xx_tauu', 32, .false.,
cph     &                      'RS', 1, xx_tauu, irec,
cph     &                      mycurrentiter, mythid )

c--     Meridional wind stress (control):
cph        call mdswritefield( yprefix//'snapshot_xx_tauv', 32, .false.,
cph     &                      'RS', 1, xx_tauv, irec,
cph     &                      mycurrentiter, mythid )

#ifdef ALLOW_BULKFORMULAE
c--     Atmospheric zonal wind:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_uwind'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, uwind, irec,
     &                      mycurrentiter, mythid )

c--     Atmospheric meridional wind:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_vwind'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1,vwind, irec,
     &                      mycurrentiter, mythid )

c--     Air temperature:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_atemp'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, atemp, irec,
     &                      mycurrentiter, mythid )

c--     Relative humidity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_aqh'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, aqh, irec,
     &                      mycurrentiter, mythid )

c--     Precipitation:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_precip'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, precip, irec,
     &                      mycurrentiter, mythid )

#ifndef ALLOW_KPP
c--     Short wave radiative flux:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_swflux'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, swflux, irec,
     &                      mycurrentiter, mythid )
#endif

c--     Long wave radiative flux:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_lwflux'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, lwflux, irec,
     &                      mycurrentiter, mythid )
#endif / * ALLOW_BULKFORMULAE * /

#ifdef ALLOW_KPP
c--     Short wave radiative flux:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &            -Qsw(i,j,bi,bj)*HeatCapacity_Cp*rhoNil*dRf(1)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_swflux'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Boundary layer depth:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_kpphbl'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', 1, kpphbl, irec,
     &                      mycurrentiter, mythid )
#endif / * ALLOW_KPP * /

#ifdef ALLOW_CLIMSST_RELAXATION
c--     SST climatology:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_sst'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, sst, irec,
     &                      mycurrentiter, mythid )
#endif / * ALLOW_CLIMSST_RELAXATION * /

#ifdef ALLOW_CLIMSSS_RELAXATION
c--     SSS climatology:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_sss'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, sss, irec,
     &                      mycurrentiter, mythid )
#endif / * ALLOW_CLIMSSS_RELAXATION * /

      endif

#endif /* ALLOW_AUTODIFF_MONITOR */

cph#endif / * ALLOW_SNAPSHOTS * /

      return
      end


