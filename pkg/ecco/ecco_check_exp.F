#include "CPP_OPTIONS.h"

      subroutine ecco_check_exp( 
     &     mythid, mycurrentiter, mycurrenttime, yprefix )

c     =================================================================
c     SUBROUTINE ecco_check_exp
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
c     SUBROUTINE check_exp
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
cph#include "cal.h"
cph#include "exf_clim_param.h"
#ifdef ALLOW_BULKFORMULAE
#include "exf_fields.h"
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

c--   == local variables ==

      INTEGER bi,bj,i,j
      integer irec
      integer mydate(4)
      character yfname*128

      _RS tmpflux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c     == end of interface ==

      irec = 0

      if ( mod((mycurrentiter),240) .eq. 0 ) then
        irec = (mycurrentiter)/240 + 1

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
      print *, 'pathei: in check_exp: theta ', theta(10,10,1,1,1)
      print *, 'pathei: in check_exp: salt  ', salt(10,10,1,1,1)
      print *, 'pathei: in check_exp: uvel  ', uvel(10,10,1,1,1)
      print *, 'pathei: in check_exp: vvel  ', vvel(10,10,1,1,1)
      print *, 'pathei: in check_exp: qnet  ', qnet(10,10,1,1)
      print *, 'pathei: in check_exp: empmr ', empmr(10,10,1,1)
      print *, 'pathei: in check_exp: fu    ', fu(10,10,1,1)
      print *, 'pathei: in check_exp: fv    ', fv(10,10,1,1)
cph)

c--     Potential temperature:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_theta'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, theta, irec,
     &                      mycurrentiter, mythid )

c--     Salinity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_salt'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, salt, irec,
     &                      mycurrentiter, mythid )

c--     Zonal velocity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_uvel'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, uvel, irec,
     &                      mycurrentiter, mythid )

c--     Meridional velocity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_vvel'
        call mdswritefield( yfname, 32, .false.,
     &                      'RL', nr, vvel, irec,
     &                      mycurrentiter, mythid )

c--     Surface pressure:
cph        write(yfname,'(128a)') ' '
cph        write(yfname,'(2a)') yprefix, 'snapshot_cg2d_x'
cph        call mdswritefield( yfname, 32, .false.,
cph     &                      'RL', 1, cg2d_x, irec,
cph     &                      mycurrentiter, mythid )

c--     Surface heat flux:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &           - Qnet(i,j,bi,bj)*HeatCapacity_Cp*rhoNil*dRf(1)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_qnet'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface virtual salt flux:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &           EmPmR(i,j,bi,bj)*dRf(1)/35.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_empmr'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface zonal wind stress:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &            -fu(i,j,bi,bj)*rhoNil*dRf(1)/horiVertRatio
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_fu'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, tmpflux, irec,
     &                      mycurrentiter, mythid )

c--     Surface meridional wind stress:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oLy,sNy+oLy
         DO i=1-oLx,sNx+oLx
            tmpflux(i,j,bi,bj) =
     &            -fv(i,j,bi,bj)*rhoNil*dRf(1)/horiVertRatio
         ENDDO
        ENDDO
       ENDDO
      ENDDO
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_fv'
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

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_WIND))
c--     Atmospheric zonal wind:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_uwind'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, uwind, irec,
     &                      mycurrentiter, mythid )
#endif

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_WIND))
c--     Atmospheric meridional wind:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_vwind'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1,vwind, irec,
     &                      mycurrentiter, mythid )
#endif

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))
c--     Air temperature:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_atemp'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, atemp, irec,
     &                      mycurrentiter, mythid )
#endif

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))
c--     Relative humidity:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_aqh'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, aqh, irec,
     &                      mycurrentiter, mythid )
#endif

#if (defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_TEMP))
c--     Precipitation:
        write(yfname,'(128a)') ' '
        write(yfname,'(2a)') yprefix, 'snapshot_precip'
        call mdswritefield( yfname, 32, .false.,
     &                      'RS', 1, precip, irec,
     &                      mycurrentiter, mythid )
#endif

#ifdef ALLOW_BULKFORMULAE
c--     Long wave radiative flux:
cph        write(yfname,'(128a)') ' '
cph        write(yfname,'(2a)') yprefix, 'snapshot_lwflux'
cph        call mdswritefield( yfname, 32, .false.,
cph     &                      'RS', 1, lwflux, irec,
cph     &                      mycurrentiter, mythid )
#endif

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

cph#endif / * ALLOW_SNAPSHOTS * /

      return
      end


