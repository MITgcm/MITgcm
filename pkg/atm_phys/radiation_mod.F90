module radiation_mod

! ==================================================================================
! ==================================================================================

!  use fms_mod,               only: open_file, check_nml_error, &
!                                   mpp_pe, close_file

   use    gcm_params_mod,     only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit
   use    constants_mod,      only: stefan, cp_air, grav, pstd_mks

!  use    diag_manager_mod,   only: register_diag_field, send_data

!  use    time_manager_mod,   only: time_type, &
!                                   operator(+), operator(-), operator(/=)

!==================================================================================
implicit none
private
!==================================================================================

! version information

character(len=128) :: version='$Id: radiation_mod.F90,v 1.7 2017/08/11 20:48:51 jmc Exp $'
character(len=128) :: tag='homemade'

!==================================================================================

! public interfaces

public :: radiation_init, radiation_down, radiation_up, radiation_end
!==================================================================================

! module variables
! select_incSW :: select expression for Incoming SW radiation @ the top
!              :: =0 : no season ; =1 : circular orbit planet (obliquity only)
! ozone_in_SW  :: =1 : account for Ozone in downward SW absorption ; =0: ignore it
! two_stream_SW :: =1 : also accont for Ozone in upward SW absorption ; =0: ignore
!                 Note: requires ozone_in_SW=1 to use two_stream_SW=1
! yearLength   :: length of solar year in seconds
! yearPhase    :: phase in solar year [0-1] relative to NH winter solstice
! obliquity    :: obliquity of Earth rotation axis in degre
logical :: initialized =.false.
integer :: select_incSW    = 0
! MK added switches for O3 scheme (default off):
integer :: ozone_in_SW     = 0
integer :: two_stream_SW   = 0

real    :: solar_constant  = 1360.0
real    :: del_sol         = 1.4
! modif omp: winter/summer hemisphere
real    :: del_sw          = 0.0
real    :: yearLength = 86400.*360.
real    :: yearPhase  = 10./365.    ! winter solstice = 22.Dec.h00
real    :: obliquity  = 23.45
real    :: atm_abs         = 0.0
real    :: sw_diff         = 0.0
real    :: albedo_value    = 0.06
real    :: solar_exponent  = 4.0
real    :: sw_co2          = 0.0596
real    :: wv_exponent     = 4.0
! only used with wv_exponent > 0:
real    :: ir_tau_eq       = 6.0
real    :: ir_tau_pole     = 1.5
real    :: linear_tau      = 0.1
! only used with wv_exponent=-1 :
real    :: ir_tau_co2_win  = 0.2150
real    :: ir_tau_wv_win1  = 147.11
real    :: ir_tau_wv_win2  = 1.0814e4
! default value set later (according to wv_exponent):
real    :: ir_tau_co2      = -999.
real    :: ir_tau_wv       = -999.
real    :: ir_tau_wv2      = -999.
real    :: window          = -999.
!RG: added carbon dioxide concentration to namelist
real    :: carbon_conc     = 360.0

real, save :: pi, deg_to_rad , rad_to_deg

namelist/radiation_nml/ select_incSW, solar_constant, del_sol, &
           ir_tau_eq, ir_tau_pole, linear_tau, ir_tau_co2, ir_tau_wv,   &
           ir_tau_wv2, atm_abs, sw_diff, del_sw, albedo_value, window,  &
           wv_exponent, solar_exponent, yearLength, yearPhase, obliquity, &
           sw_co2, ir_tau_co2_win, ir_tau_wv_win1, ir_tau_wv_win2, &
           carbon_conc, ozone_in_SW, two_stream_SW

!==================================================================================
!-------------------- diagnostics fields -------------------------------

!integer :: id_olr, id_swdn_sfc, id_swdn_toa, id_lwdn_sfc, id_lwup_sfc, &
!           id_tdt_rad, id_flux_rad, id_flux_lw, id_flux_sw, id_entrop_rad, id_tdt_sw

character(len=10), parameter :: mod_name = 'two_stream'

real :: missing_value = -999.

contains

! ==================================================================================
! ==================================================================================

!subroutine radiation_init(is, ie, js, je, num_levels, axes, Time)
subroutine radiation_init( is, ie, js, je, num_levels, nSx,nSy, axes,   &
                           Time, cst_albedo, myThid )

!-------------------------------------------------------------------------------------
integer, intent(in)               :: is, ie, js, je, num_levels
integer, intent(in)               :: nSx, nSy
integer, intent(in), dimension(4) :: axes
!type(time_type), intent(in)       :: Time
real, intent(in)                  :: Time
real, intent(out)                 :: cst_albedo
integer, intent(in)               :: myThid
!-------------------------------------------------------------------------------------
!integer, dimension(3) :: half = (/1,2,4/)
!integer :: ierr, io
integer         :: iUnit
CHARACTER*(gcm_LEN_MBUF) :: msgBuf
!-----------------------------------------------------------------------------------------
! read namelist and copy to logfile

!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

     WRITE(msgBuf,'(A)') 'RADIATION_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'RADIATION_INIT',       &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=radiation_nml)
     WRITE(msgBuf,'(A)')                                            &
          'RADIATION_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
#ifdef SINGLE_DISK_IO
     CLOSE(iUnit)
#else
     CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

pi    = 4.0*atan(1.)
deg_to_rad = 2.*pi/360.
rad_to_deg = 360.0/2./pi

if ( wv_exponent .eq. -1. ) then
! default value for wv_exponent=-1:
   if ( ir_tau_co2.eq. -999. ) ir_tau_co2 =   3.14
   if ( ir_tau_wv .eq. -999. ) ir_tau_wv  = 199.25
   if ( ir_tau_wv2.eq. -999. ) ir_tau_wv2 =  14.78
   if ( window    .eq. -999. ) window     = 0.3732 ! spectral window for Water Vapour
else
! default value for wv_exponent= 0:
   if ( ir_tau_co2.eq. -999. ) ir_tau_co2 = 0.8678
   if ( ir_tau_wv .eq. -999. ) ir_tau_wv  = 1.9979e+3
   if ( window    .eq. -999. ) window     = 0.0    ! spectral window transparent to LW
endif

initialized = .true.

     ENDIF
     CALL BARRIER(myThid)
     cst_albedo = albedo_value

!-----------------------------------------------------------------------
!------------ initialize diagnostic fields ---------------

!   id_olr = &
!   register_diag_field ( mod_name, 'olr', axes(1:2), Time, &
!              'outgoing longwave radiation', &
!              'watts/m2', missing_value=missing_value               )
!   id_swdn_sfc = &
!   register_diag_field ( mod_name, 'swdn_sfc', axes(1:2), Time, &
!              'SW flux down at surface', &
!              'watts/m2', missing_value=missing_value               )
!   id_swdn_toa = &
!   register_diag_field ( mod_name, 'swdn_toa', axes(1:2), Time, &
!              'SW flux down at TOA', &
!              'watts/m2', missing_value=missing_value               )
!   id_lwup_sfc = &
!   register_diag_field ( mod_name, 'lwup_sfc', axes(1:2), Time, &
!              'LW flux up at surface', &
!              'watts/m2', missing_value=missing_value               )
!   id_lwdn_sfc = &
!   register_diag_field ( mod_name, 'lwdn_sfc', axes(1:2), Time, &
!              'LW flux down at surface', &
!              'watts/m2', missing_value=missing_value               )
!   id_tdt_rad = &
!       register_diag_field ( mod_name, 'tdt_rad', axes(1:3), Time, &
!              'Temperature tendency due to radiation', &
!              'K/s', missing_value=missing_value               )
!   id_tdt_sw  = &
!       register_diag_field ( mod_name, 'tdt_sw', axes(1:3), Time, &
!              'Temperature tendency due to SW radiation', &
!              'K/s', missing_value=missing_value               )
!   id_flux_rad = &
!       register_diag_field ( mod_name, 'flux_rad', axes(half), Time, &
!              'Total radiative flux (positive up)', &
!              'W/m^2', missing_value=missing_value               )
!   id_flux_lw = &
!       register_diag_field ( mod_name, 'flux_lw', axes(half), Time, &
!              'Net longwave radiative flux (positive up)', &
!              'W/m^2', missing_value=missing_value               )
!   id_flux_sw = &
!       register_diag_field ( mod_name, 'flux_sw', axes(half), Time, &
!              'Net shortwave radiative flux (positive up)', &
!              'W/m^2', missing_value=missing_value               )
!   id_entrop_rad = &
!           register_diag_field ( mod_name, 'entrop_rad', axes(1:3), Time, &
!              'Entropy production by radiation', &
!              '1/s', missing_value=missing_value               )

return
end subroutine radiation_init

! ==================================================================================

subroutine radiation_down (is, js, Time_diag, lat, p_half, t, q,      &
                           albedo, o3conc, ozone_column,              &
                           net_surf_sw_down, surf_lw_down,            &
                           dtrans, dtrans_win, b, b_win,              &
                           down, solar_down,                          &
                           myThid )

! Begin the radiation calculation by computing downward fluxes.
! This part of the calculation does not depend on the surface temperature.

integer, intent(in)                 :: is, js
!type(time_type), intent(in)         :: Time_diag
real, intent(in)                    :: Time_diag
real, intent(in) , dimension(:,:)   :: lat
real, intent(out), dimension(:,:)   :: net_surf_sw_down
real, intent(out), dimension(:,:)   :: surf_lw_down
real, intent(in) , dimension(:,:,:) :: t, q, p_half
real, intent(in) , dimension(:,:)   :: albedo
real, intent(in) , dimension(:,:,:) :: o3conc
real, intent(out), dimension(:,:,:) :: ozone_column
real, intent(out), dimension(:,:,:) :: dtrans
real, intent(out), dimension(:,:,:) :: dtrans_win
real, intent(out), dimension(:,:,:) :: b
real, intent(out), dimension(:,:,:) :: b_win
real, intent(out), dimension(:,:,:) :: down
real, intent(out), dimension(:,:,:) :: solar_down
integer, intent(in)                 :: myThid

!integer :: i, j
integer :: k, n
integer :: im, jm
! Variables for seasonal Incoming SW
real    :: tYear, largeTan, cDecl, sDecl, tanDecl
real    :: lgCO2conc

!logical :: used

! -------------------------------------------------------------------------
!real, allocatable, dimension(:,:)   :: swin
real, allocatable, dimension(:,:)   :: ss, solar, solar_tau_0, p2
real, allocatable, dimension(:,:)   :: solar_tau_k, sw_wv, del_sol_tau, mag_fac
real, allocatable, dimension(:,:,:) :: solar_tau, dtrans_sol, down_win
real, allocatable, dimension(:,:)   :: del_tau, tau_0, tau_km, tau_kp, del_tau_win
! MK: Variables for Lacis & Hansen ozone SW scheme
real, allocatable, dimension(:,:,:) :: ozone_mag, ozone_dF0_down
real, allocatable, dimension(:,:)   :: abs_uv_LH, abs_uv_LH_FS
real, allocatable, dimension(:,:)   :: abs_vis_LH, abs_vis_LH_FS
! Variables for seasonal Incoming SW
real, allocatable, dimension(:,:)   :: cLat, sLat, cos_H, HourAng
! -------------------------------------------------------------------------

n = size(t,3)
im = size(t,1)
jm = size(t,2)

! -------------------------------------------------------------------------
!allocate (swin             (im, jm))
allocate (ss               (im, jm))
allocate (solar            (im, jm))
if ( select_incSW .eq. 0 ) then
  allocate (p2             (im, jm))
else
! Variables for seasonal Incoming SW
  allocate (cLat           (im, jm))
  allocate (sLat           (im, jm))
  allocate (cos_H          (im, jm))
  allocate (HourAng        (im, jm))
endif
if ( ozone_in_SW .eq. 1 ) then
  allocate (ozone_mag      (im, jm, n+1))
  allocate (ozone_dF0_down (im, jm, n+1))
  allocate (abs_uv_LH      (im, jm))
  allocate (abs_vis_LH     (im, jm))
  allocate (abs_uv_LH_FS   (im, jm))
  allocate (abs_vis_LH_FS  (im, jm))
endif
if ( solar_exponent .eq. 0. ) then
  allocate (solar_tau_k    (im, jm))
  allocate (sw_wv          (im, jm))
  allocate (del_sol_tau    (im, jm))
  allocate (mag_fac        (im, jm))
  allocate (dtrans_sol     (im, jm, n))
else
  allocate (solar_tau_0    (im, jm))
  allocate (solar_tau      (im, jm, n+1))
endif
if ( wv_exponent .eq. -1. ) then
  allocate (del_tau        (im, jm))
  allocate (del_tau_win    (im, jm))
  allocate (down_win       (im, jm, n+1))
elseif ( wv_exponent .eq. 0. ) then
  allocate (del_tau        (im, jm))
else
  allocate (tau_0          (im, jm))
  allocate (tau_km         (im, jm))
  allocate (tau_kp         (im, jm))
endif
! -------------------------------------------------------------------------

ss  = sin(lat)

if ( select_incSW .eq. 1 .or. ozone_in_SW .eq. 1 ) then
! daily-mean Incoming SW with simple seasonal cycle accounting
!  only for obliquity (i.e., circular orbit planet)

! tYear = time in the solar year, in [0-1]
   tYear = MOD( Time_diag/yearLength + yearPhase , 1.0 )

! Compute the declination angle
! a) approximate estimate of declination angle: relative error is less
!    than 0.03 for current obliq but as large as 0.21 for obliq=60^o
!    xDecl = - obliquity*deg_to_rad * cos(2.*pi*tYear)
! b) unapproximate expression:
   sDecl = -sin( obliquity*deg_to_rad ) * cos(2.*pi*tYear)
   cDecl =  cos( asin( sDecl ) )
endif

if ( select_incSW .eq. 0 ) then
! Original Incoming SW (no seasonal cycle):
   p2 = (1. - 3.*ss*ss)/4.
   solar = 0.25*solar_constant*(1.0 + del_sol*p2 + del_sw * ss)

elseif ( select_incSW .eq. 1 ) then

   largeTan = 1.e+16
   if ( cDecl.EQ.0. ) then
    tanDecl = sign( largeTan, sDecl )
   else
    tanDecl = sDecl / cDecl
   endif

! Compute Insolation
   cLat = cos(lat)
!  sLat = sin(lat)
   sLat = ss
   cos_H = sign( largeTan, sLat )
   where ( cLat .ne. 0. )
     cos_H = sLat/cLat
   endwhere
   cos_H = max( min( - cos_H * tanDecl, 1. ) , -1. )
   HourAng = acos( cos_H )
   solar = (solar_constant/pi)                             &
         *( HourAng*sLat*sDecl + cLat*cDecl*sin(HourAng) )
else
  stop 'invalid select_incSW'
endif

! set the log of CO2 concentration
lgCO2conc = log(carbon_conc/360.)

! set a constant albedo for testing
!albedo(:,:) = albedo_value

if ( solar_exponent .eq. 0. ) then
! (RG) scheme based on dtau/dsigma = bq + amu where b is a function of solar_tau
! ref: Ruth Geen etal, GRL 2016 (supp. information).
! RG: added carbon dioxide log function
  solar_tau_k = 0.
  mag_fac(:,:) = 1. ! 35.0 / sqrt(1224 * (cos(lat(:,:))) ** 2 + 1)

  do k = 1, n
!   sw_wv = exp( 0.01887 / (solar_tau_k + 0.009522)                            &
!              + 1.603 / ( (solar_tau_k + 0.5194)**2 ) )
    sw_wv = solar_tau_k + 0.5194
    sw_wv = exp( 0.01887 / (solar_tau_k + 0.009522)                            &
                 + 1.603 / ( sw_wv*sw_wv ) )
    del_sol_tau(:,:) = ( sw_co2 + 0.0029 * lgCO2conc                           &
                                + sw_wv(:,:) * q(:,:,k) ) * mag_fac(:,:)       &
                     * ( p_half(:,:,k+1) - p_half(:,:,k) ) / p_half(:,:,n+1)
    dtrans_sol(:,:,k) = exp( - del_sol_tau(:,:) )
    solar_tau_k = solar_tau_k + del_sol_tau(:,:)
  end do

  solar_down(:,:,1) = solar(:,:)
  do k = 1,n
    solar_down(:,:,k+1) = solar_down(:,:,k)*dtrans_sol(:,:,k)
  end do

else
! tau proportional to p^solar_exponent, scaled by atm_abs

  solar_tau_0 = (1.0 - sw_diff*ss*ss)*atm_abs

  do k = 1, n+1
    solar_tau(:,:,k) = solar_tau_0(:,:)                                        &
                     *(p_half(:,:,k)/p_half(:,:,n+1))**solar_exponent
  end do

  do k = 1,n+1
    solar_down(:,:,k) = solar(:,:)*exp(-solar_tau(:,:,k))
  end do

endif

if ( ozone_in_SW .eq. 1 ) then
!-------
! MK 2017: Add Lacis and Hansen (JAS, 1974) absorption by ozone,
! and apply Forster and Shine (JGR, 1997) correction.
!-------
! Computes broadband absorption, as fraction of total solar flux at TOA.
! However, O3 band does not significantly overlap with water vapour SW absorption,
! so can safely add O3 heating on top of water vapour heating without changing the
! radiation seen by the water vapour optical depths in Ruth's scheme above.

! o3conc = Ozone VMR on pressure levels.  Need to convert to centimetre of ozone
! to use with Lacis and Hansen scheme.  Make ozone_column array containing
! ozone amount in cm from TOA down to top of level k

! ozone_column(:,:,1:n+1) = Column O3 in cm above level 1 <= k <= n+1

  ozone_column(:,:,1) = 0.
  do k = 1, n
    ozone_column(:,:,k+1) = ozone_column(:,:,k) + (                            &
                          ( 287.1 / (1.38065e-23 * grav) )                     &
                          * ( p_half(:,:,k+1) - p_half(:,:,k) )                &
                          * o3conc(:,:,k) / 2.687e+23                          &
                                                  )
  end do

! Apply magnification factor:
  ozone_mag(:,:,:) = ozone_column(:,:,:) * 35.                                  &
                   / sqrt( 1224.*cDecl*cDecl + 1. )

! Parameterization for fraction of total flux absorbed from LH74:
  do k = 1, n+1

    abs_vis_LH(:,:)   = ( 0.02118 * ozone_mag(:,:,k) ) / (                     &
                          1. + 0.042 * ozone_mag(:,:,k)                        &
                             + 0.000323*ozone_mag(:,:,k)*ozone_mag(:,:,k)      &
                                                         )
  ! FS97 correction:
    abs_vis_LH_FS(:,:) = ( -0.002894 * ozone_mag(:,:,k) + 1.0663 )             &
                       * abs_vis_LH(:,:)

    abs_uv_LH(:,:)   = ( 1.082 * ozone_mag(:,:,k) ) / (                        &
                         ( 1. + 138.6 * ozone_mag(:,:,k) )**0.805              &
                                                      )                        &
                     + ( 0.0658 * ozone_mag(:,:,k) ) / (                       &
                         1. + ( 103.6 * ozone_mag(:,:,k) )**3                  &
                                                       )
  ! FS97 correction:
    abs_uv_LH_FS(:,:) = ( -0.01632 * ozone_mag(:,:,k) + 1.08964 )              &
                      * abs_uv_LH(:,:)

  ! Total downward SW flux absorbed above level k due to ozone heating.
  ! N.B. Flux absorbed in level k = ozone_dF0_down(:,:,k+1) - ozone_dF0_down(:,:,k)
    ozone_dF0_down(:,:,k) = solar(:,:) * ( abs_vis_LH_FS(:,:)                  &
                                         + abs_uv_LH_FS(:,:)                   &
                                         )

  ! Subtract off ozone absorption from total SW flux:
    solar_down(:,:,k) = solar_down(:,:,k) - ozone_dF0_down(:,:,k)

  end do

endif

if ( wv_exponent .eq. -1. ) then
! split LW in 2 bands: water-vapour window and remaining = non-window:
! ref: Ruth Geen etal, GRL 2016 (supp. information).
! RG: added carbon dioxide log function
  do k = 1, n
    del_tau    = ( ir_tau_co2 + 0.2023 * lgCO2conc                             &
                    + ir_tau_wv*log(ir_tau_wv2*q(:,:,k) + 1) )                 &
               * ( p_half(:,:,k+1)-p_half(:,:,k) ) / p_half(:,:,n+1)
    dtrans(:,:,k) = exp( - del_tau )
    del_tau_win   = ( ir_tau_co2_win + 0.0954 * lgCO2conc                      &
                                     + ir_tau_wv_win1*q(:,:,k)                 &
                                     + ir_tau_wv_win2*q(:,:,k)*q(:,:,k) )      &
                  * ( p_half(:,:,k+1)-p_half(:,:,k) ) / p_half(:,:,n+1)
    dtrans_win(:,:,k) = exp( - del_tau_win )
  end do

elseif ( wv_exponent .eq. 0. ) then
  dtrans_win = 1.
! longwave optical thickness function of specific humidity (M.Byrne & P.O'Gorman):
  do k = 1, n
    del_tau    = ( ir_tau_co2 + ir_tau_wv * q(:,:,k) )                         &
               * ( p_half(:,:,k+1)-p_half(:,:,k) ) / p_half(:,:,n+1)
    dtrans(:,:,k) = exp( - del_tau )
  end do

else
  dtrans_win = 1.
! longwave optical thickness function of latitude and pressure
  tau_0 = ir_tau_eq + (ir_tau_pole - ir_tau_eq)*ss*ss

  k = 1
  tau_kp =   tau_0(:,:) * (                                                    &
                  linear_tau * p_half(:,:,k)/p_half(:,:,n+1)                   &
         + (1.0 - linear_tau)*(p_half(:,:,k)/p_half(:,:,n+1))**wv_exponent     &
                          )
  do k = 1, n
    tau_km = tau_kp
    tau_kp = tau_0(:,:) * (                                                    &
                  linear_tau * p_half(:,:,k+1)/p_half(:,:,n+1)                 &
         + (1.0 - linear_tau)*(p_half(:,:,k+1)/p_half(:,:,n+1))**wv_exponent   &
                          )
    dtrans(:,:,k) = exp( -(tau_kp - tau_km) )
  end do

endif

! no radiation from spectral window
b = stefan*t*t*t*t
b_win = window*b
b = (1.0-window)*b

down(:,:,1) = 0.0
do k = 1,n
  down(:,:,k+1)     = down(:,:,k)*dtrans(:,:,k)                        &
                    + b(:,:,k)*(1.0 - dtrans(:,:,k))
end do

if ( wv_exponent .eq. -1. ) then
  down_win(:,:,1) = 0.0
  do k = 1,n
    down_win(:,:,k+1) = down_win(:,:,k)*dtrans_win(:,:,k)              &
                      + b_win(:,:,k)*(1.0 - dtrans_win(:,:,k))
  end do
  down = down + down_win
endif

surf_lw_down     = down(:,:,n+1)
net_surf_sw_down = solar_down(:,:,n+1)*(1. - albedo(:,:))
!swin = solar_down(:,:,1)

!------- downward sw flux surface -------
!     if ( id_swdn_sfc > 0 ) then
!         used = send_data ( id_swdn_sfc, net_surf_sw_down, Time_diag)
!     endif
!------- incoming sw flux toa -------
!     if ( id_swdn_toa > 0 ) then
!         used = send_data ( id_swdn_toa, swin, Time_diag)
!     endif
!------- downward lw flux surface -------
!     if ( id_lwdn_sfc > 0 ) then
!         used = send_data ( id_lwdn_sfc, surf_lw_down, Time_diag)
!     endif

! -------------------------------------------------------------------------
!deallocate (swin)
deallocate (ss, solar)
if ( select_incSW .eq. 0 ) then
  deallocate (p2)
else
! Variables for seasonal Incoming SW
  deallocate (cLat, sLat)
  deallocate (cos_H, HourAng)
endif
if ( solar_exponent .eq. 0. ) then
  deallocate (solar_tau_k, sw_wv, del_sol_tau, mag_fac, dtrans_sol)
else
  deallocate (solar_tau_0, solar_tau)
endif
if ( ozone_in_SW .eq. 1 ) then
  deallocate (ozone_mag, ozone_dF0_down)
  deallocate (abs_uv_LH, abs_vis_LH, abs_uv_LH_FS, abs_vis_LH_FS)
endif
if ( wv_exponent .eq. -1. ) then
  deallocate (del_tau, del_tau_win, down_win)
elseif ( wv_exponent .eq. 0. ) then
  deallocate (del_tau)
else
  deallocate (tau_0, tau_km, tau_kp)
endif
! -------------------------------------------------------------------------

return
end subroutine radiation_down

! ==================================================================================

!subroutine radiation_up (is, js, Time_diag, lat, p_half, t_surf, t, tdt)
subroutine radiation_up ( is, js, Time_diag, lat, p_half, t_surf, t, tdt,      &
                          flux_lw, flux_sw, albedo, ozone_column,              &
                          dtrans, dtrans_win, b, b_win, down, solar_down,      &
                          myThid )

! Now complete the radiation calculation by computing the upward and net fluxes.

integer, intent(in)                 :: is, js
!type(time_type), intent(in)         :: Time_diag
real, intent(in)                    :: Time_diag
real, intent(in) , dimension(:,:)   :: lat
real, intent(in) , dimension(:,:)   :: t_surf
real, intent(in) , dimension(:,:,:) :: t, p_half
real, intent(inout), dimension(:,:,:) :: tdt
real, intent(out), dimension(:,:,:) :: flux_lw
real, intent(out), dimension(:,:,:) :: flux_sw
real, intent(in),  dimension(:,:)   :: albedo
real, intent(in) , dimension(:,:,:) :: ozone_column
real, intent(in),  dimension(:,:,:) :: dtrans
real, intent(in),  dimension(:,:,:) :: dtrans_win
real, intent(in),  dimension(:,:,:) :: b
real, intent(in),  dimension(:,:,:) :: b_win
real, intent(in),  dimension(:,:,:) :: down
real, intent(in),  dimension(:,:,:) :: solar_down
integer, intent(in)                 :: myThid

!integer :: i, j
integer :: k, n
integer :: im, jm
! Variables for seasonal Incoming SW
real    :: tYear, cDecl, sDecl

!logical :: used

! -------------------------------------------------------------------------
real, allocatable, dimension(:,:)   :: solar, b_surf
real, allocatable, dimension(:,:,:) :: tdt_rad, up, up_win, solar_up
!real, allocatable, dimension(:,:,:) :: tdt_sw, flux_rad   ! not used
! MK: Variables for Lacis & Hansen ozone SW scheme
real, allocatable, dimension(:,:,:) :: ozone_mag_up, ozone_dF0_up
real, allocatable, dimension(:,:)   :: abs_uv_LH_up, abs_uv_LH_FS_up
real, allocatable, dimension(:,:)   :: abs_vis_LH_up, abs_vis_LH_FS_up
real, allocatable, dimension(:,:)   :: r_bar
! -------------------------------------------------------------------------

n = size(t,3)
im = size(t,1)
jm = size(t,2)

! -------------------------------------------------------------------------
if( two_stream_SW .eq. 1 ) then
  allocate (solar            (im, jm))
  if ( ozone_in_SW .eq. 1 ) then
    allocate (ozone_mag_up    (im, jm, n+1))
    allocate (ozone_dF0_up    (im, jm, n+1))
    allocate (abs_uv_LH_up    (im, jm))
    allocate (abs_vis_LH_up   (im, jm))
    allocate (abs_uv_LH_FS_up (im, jm))
    allocate (abs_vis_LH_FS_up(im, jm))
    allocate (r_bar           (im, jm))
  endif
endif
allocate (tdt_rad          (im, jm, n))
allocate (up               (im, jm, n+1))
allocate (up_win           (im, jm, n+1))
allocate (solar_up         (im, jm, n+1))
!allocate (tdt_sw           (im, jm, n))    ! not used
!allocate (flux_rad         (im, jm, n+1))  ! not used
allocate (b_surf           (im, jm))
! -------------------------------------------------------------------------

! MK add in solar_up variables
do k = 1,n+1
  solar_up(:,:,k) = albedo(:,:) * solar_down(:,:,n+1)
end do

if ( two_stream_SW .eq. 1 ) then
! MK do upward SW for ozone

! Need solar variables again:
  solar = solar_down(:,:,1)

  if ( ozone_in_SW .eq. 1 ) then
  ! tYear = time in the solar year, in [0-1]
    tYear = MOD( Time_diag/yearLength + yearPhase , 1.0 )

  ! Compute the declination angle
  ! a) approximate estimate of declination angle: relative error is less
  !    than 0.03 for current obliq but as large as 0.21 for obliq=60^o
  !    xDecl = - obliquity*deg_to_rad * cos(2.*pi*tYear)
  ! b) unapproximate expression:
    sDecl = -sin(  obliquity*deg_to_rad ) * cos(2.*pi*tYear)
    cDecl =  cos( asin( sDecl ) )

  ! Lacis and Hansen scheme for absorption of upward SW flux by O3:
  ! Apply magnification factor:

    do k = n+1,1,-1
      ozone_mag_up(:,:,k) = ozone_column(:,:,n+1) * 35. /                      &
                            sqrt( 1224.*cDecl*cDecl + 1. )                     &
                          + 1.9 * ( ozone_column(:,:,n+1)                      &
                                    - ozone_column(:,:,k)                      &
                                  )
    end do

    r_bar(:,:) = ( 0.291 / ( 1. + 0.816*cDecl ) ) + (                          &
                   ( 1. - ( 0.291 / ( 1. + 0.816*cDecl ) ) )                   &
                   * 0.856 * albedo(:,:) / ( 1. - 0.144*albedo(:,:) )          &
                                                    )
  ! LH74 parameterisation for fraction of total solar flux absorbed:

    do k = n+1,1,-1

      abs_vis_LH_up(:,:) = r_bar(:,:) * (0.02118 * ozone_mag_up(:,:,k)) / (    &
                         1. + 0.042 * ozone_mag_up(:,:,k)                      &
                            + 0.000323*ozone_mag_up(:,:,k)*ozone_mag_up(:,:,k) &
                                                                          )
    ! FS97 correction
      abs_vis_LH_FS_up(:,:) = ( -0.002894 * ozone_mag_up(:,:,k) + 1.0663 )     &
                              * abs_vis_LH_up(:,:)

      abs_uv_LH_up(:,:) = r_bar(:,:) * (1.082 * ozone_mag_up(:,:,k)) / (       &
                            ( 1. + 138.6 * ozone_mag_up(:,:,k) )**0.805        &
                                                                       )       &
                        + ( 0.0658 * ozone_mag_up(:,:,k) ) / (                 &
                            1. + ( 103.6 * ozone_mag_up(:,:,k) )**3            &
                                                             )
    ! FS97 correction
      abs_uv_LH_FS_up(:,:) = ( -0.01632 * ozone_mag_up(:,:,k) + 1.08964 )      &
                             * abs_uv_LH_up(:,:)

    ! Total SW flux absorbed along path by reflected radiation up till and including level k.
    ! N.B. Flux absorbed in level k = ozone_dF0_up(:,:,k) - ozone_dF0_up(:,:,k+1)
      ozone_dF0_up(:,:,k) = solar(:,:) * ( abs_vis_LH_FS_up(:,:)               &
                                         + abs_uv_LH_FS_up(:,:)                &
                                         )

    end do

    do k = n,1,-1
    ! Subtract off ozone absorption from total SW flux:
      solar_up(:,:,k) = solar_up(:,:,k)                                        &
                      - ( ozone_dF0_up(:,:,k) - ozone_dF0_up(:,:,n+1) )
    end do

  endif   ! ozone_in_SW

endif   ! two_stream_SW

! total flux from surface
b_surf = stefan*t_surf*t_surf*t_surf*t_surf

! first deal with non-window upward flux
up(:,:,n+1) = b_surf*(1.0-window)
up_win(:,:,n+1) = b_surf*window
do k = n,1,-1
  up(:,:,k) = up(:,:,k+1)*dtrans(:,:,k) + b(:,:,k)*(1.0 - dtrans(:,:,k))
  up_win(:,:,k) = up_win(:,:,k+1)*dtrans_win(:,:,k)            &
                + b_win(:,:,k)*(1.0 - dtrans_win(:,:,k))
end do

! add upward flux in spectral window
up = up + up_win

! MK Compute flux_sw in terms of solar_up, and use flux_sw in tdt calculation
do k = 1,n+1
  flux_lw(:,:,k) = up(:,:,k)-down(:,:,k)   ! LW only, upward +ve
  flux_sw(:,:,k) = solar_down(:,:,k) - solar_up(:,:,k)  ! Net downward SW
! flux_rad(:,:,k) = flux_lw(:,:,k) + flux_sw(:,:,k)   ! not used
end do

if ( two_stream_SW .eq. 1 ) then
  do k = 1,n
    tdt_rad(:,:,k) = ( flux_lw(:,:,k+1) - flux_lw(:,:,k)                  &
                     - flux_sw(:,:,k+1) + flux_sw(:,:,k)                  &
                     )                                                    &
                   * grav / ( cp_air*( p_half(:,:,k+1)-p_half(:,:,k) ) )
  end do
else
  do k = 1,n
    tdt_rad(:,:,k) = ( flux_lw(:,:,k+1) - flux_lw(:,:,k)                  &
                     - solar_down(:,:,k+1) + solar_down(:,:,k) )          &
                   * grav / ( cp_air*( p_half(:,:,k+1)-p_half(:,:,k) ) )
  end do
endif

do k = 1,n
! tdt_sw is not used:
! tdt_sw(:,:,k) = ( - flux_sw(:,:,k+1) + flux_sw(:,:,k) )                 &
!                  * grav / ( cp_air*( p_half(:,:,k+1)-p_half(:,:,k) ) )
  tdt(:,:,k) = tdt(:,:,k) + tdt_rad(:,:,k)
end do

!------- outgoing lw flux toa (olr) -------
!     if ( id_olr > 0 ) then
!         used = send_data ( id_olr, olr, Time_diag)
!     endif
!------- upward lw flux surface -------
!     if ( id_lwup_sfc > 0 ) then
!         used = send_data ( id_lwup_sfc, b_surf, Time_diag)
!     endif
!------- temperature tendency due to radiation ------------
!     if ( id_tdt_rad > 0 ) then
!        used = send_data ( id_tdt_rad, tdt_rad, Time_diag)
!     endif
!     if ( id_tdt_sw > 0 ) then
!        used = send_data ( id_tdt_sw, tdt_sw, Time_diag)
!     endif
!------- total radiative flux (at half levels) -----------
!     if ( id_flux_rad > 0 ) then
!        used = send_data ( id_flux_rad, flux_rad, Time_diag)
!     endif
!------- longwave radiative flux (at half levels) --------
!     if ( id_flux_lw > 0 ) then
!        used = send_data ( id_flux_lw, net, Time_diag)
!     endif
!     if ( id_flux_sw > 0 ) then
!        used = send_data ( id_flux_sw, flux_sw, Time_diag)
!     endif
!     if ( id_entrop_rad > 0 ) then
!        do k=1,n
!           entrop_rad(:,:,k) =tdt_rad(:,:,k)/t(:,:,k)*p_half(:,:,n+1)/1.e5
!        end do
!        used = send_data ( id_entrop_rad, entrop_rad, Time_diag)
!     endif

! -------------------------------------------------------------------------
if ( two_stream_SW .eq. 1 ) then
  deallocate (solar)
  if ( ozone_in_SW .eq. 1 ) then
    deallocate (ozone_mag_up, ozone_dF0_up)
    deallocate (abs_uv_LH_up, abs_vis_LH_up)
    deallocate (abs_uv_LH_FS_up, abs_vis_LH_FS_up, r_bar)
  endif
endif
deallocate (tdt_rad, up, up_win, solar_up)
!deallocate (tdt_sw, flux_rad)  ! not used
deallocate (b_surf)
! -------------------------------------------------------------------------

return
end subroutine radiation_up

! ==================================================================================

subroutine radiation_end

!deallocate (b, tdt_rad, tdt_sw, entrop_rad)
!!deallocate (up, down, net, solar_down, flux_rad, flux_sw)
!deallocate (up, net, flux_rad, flux_sw)
!deallocate (b_surf, olr, swin, albedo)
!!deallocate (dtrans)
!deallocate (tau, solar_tau)
!deallocate (ss, solar, tau_0, solar_tau_0, p2)

end subroutine radiation_end

! ==================================================================================

end module radiation_mod
