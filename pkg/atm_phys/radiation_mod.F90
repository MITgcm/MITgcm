! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/radiation_mod.F90,v 1.3 2014/09/24 00:16:51 jmc Exp $
! $Name:  $

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

character(len=128) :: version='$Id: radiation_mod.F90,v 1.3 2014/09/24 00:16:51 jmc Exp $'
character(len=128) :: tag='homemade'

!==================================================================================

! public interfaces

public :: radiation_init, radiation_down, radiation_up, radiation_end
!==================================================================================

! module variables
! select_incSW :: select expression for Incoming SW radiation @ the top
!              :: =0 : no season ; =1 : circular orbit planet (obliquity only)
! yearLength   :: length of solar year in seconds
! yearPhase    :: phase in solar year [0-1] relative to NH winter solstice
! obliquity    :: obliquity of Earth rotation axis in degre
logical :: initialized =.false.
integer :: select_incSW    = 0

real    :: solar_constant  = 1360.0
real    :: del_sol         = 1.4
! modif omp: winter/summer hemisphere
real    :: del_sw          = 0.0
real    :: yearLength = 86400.*360.
real    :: yearPhase  = 10./365.    ! winter solstice = 22.Dec.h00
real    :: obliquity  = 23.45
real    :: ir_tau_eq       = 6.0
real    :: ir_tau_pole     = 1.5
real    :: linear_tau      = 0.1
real    :: ir_tau_co2      = 0.8678
real    :: ir_tau_wv       = 1.9979e+3
real    :: atm_abs         = 0.0
real    :: sw_diff         = 0.0
real    :: albedo_value    = 0.06
real    :: window          = 0.0 ! spectral window transparent to LW
real    :: wv_exponent     = 4.0
real    :: solar_exponent  = 4.0

real, save :: pi, deg_to_rad , rad_to_deg

namelist/radiation_nml/ select_incSW, solar_constant, del_sol, &
           ir_tau_eq, ir_tau_pole, linear_tau, ir_tau_co2, ir_tau_wv,   &
           atm_abs, sw_diff, del_sw, albedo_value, window, wv_exponent, &
           solar_exponent, yearLength, yearPhase, obliquity

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
subroutine radiation_init(is, ie, js, je, num_levels, nSx,nSy, axes, Time, myThid)

!-------------------------------------------------------------------------------------
integer, intent(in), dimension(4) :: axes
!type(time_type), intent(in)       :: Time
real, intent(in)                  :: Time
integer, intent(in)               :: is, ie, js, je, num_levels
integer, intent(in)               :: nSx, nSy
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
     CLOSE(iUnit)

pi    = 4.0*atan(1.)
deg_to_rad = 2.*pi/360.
rad_to_deg = 360.0/2./pi

initialized = .true.

     ENDIF
     CALL BARRIER(myThid)

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
!                          net_surf_sw_down, surf_lw_down)
                           net_surf_sw_down, surf_lw_down,            &
                           albedo, dtrans, b, down, solar_down,       &
                           myThid )

! Begin the radiation calculation by computing downward fluxes.
! This part of the calculation does not depend on the surface temperature.

integer, intent(in)                 :: is, js
!type(time_type), intent(in)         :: Time_diag
real, intent(in)                    :: Time_diag
real, intent(in) , dimension(:,:)   :: lat
real, intent(out) , dimension(:,:)   :: net_surf_sw_down
real, intent(out) , dimension(:,:)   :: surf_lw_down
real, intent(in) , dimension(:,:,:) :: t, q, p_half
real, intent(out), dimension(:,:)   :: albedo
real, intent(out), dimension(:,:,:) :: dtrans
real, intent(out), dimension(:,:,:) :: b
real, intent(out), dimension(:,:,:) :: down
real, intent(out), dimension(:,:,:) :: solar_down
integer, intent(in)                 :: myThid

!integer :: i, j
integer :: k, n
integer :: im, jm
! Variables for seasonal Incoming SW
real    :: tYear, largeTan, cDecl, sDecl, tanDecl

!logical :: used

! -------------------------------------------------------------------------
!real, allocatable, dimension(:,:)   :: swin
real, allocatable, dimension(:,:)   :: ss, solar, solar_tau_0, p2
real, allocatable, dimension(:,:,:) :: solar_tau
real, allocatable, dimension(:,:)   :: del_tau, tau_0, tau_km, tau_kp
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
allocate (solar_tau_0      (im, jm))
allocate (solar_tau        (im, jm, n+1))
if ( select_incSW .eq. 0 ) then
  allocate (p2             (im, jm))
elseif ( select_incSW .eq. 1 ) then
! Variables for seasonal Incoming SW
  allocate (cLat           (im, jm))
  allocate (sLat           (im, jm))
  allocate (cos_H          (im, jm))
  allocate (HourAng        (im, jm))
endif
if ( wv_exponent .eq. 0. ) then
  allocate (del_tau        (im, jm))
else
  allocate (tau_0          (im, jm))
  allocate (tau_km         (im, jm))
  allocate (tau_kp         (im, jm))
endif
! -------------------------------------------------------------------------

ss  = sin(lat)

if ( select_incSW .eq. 0 ) then
! Original Incoming SW (no saisonal cycle):
   p2 = (1. - 3.*ss*ss)/4.
   solar = 0.25*solar_constant*(1.0 + del_sol*p2 + del_sw * ss)

elseif ( select_incSW .eq. 1 ) then
! daily-mean Incoming SW with simple seasonal cycle acounting
!  only for obliquity (i.e., circular orbit planet)
   largeTan = 1.e+16

! tYear = time in the solar year, in [0-1]
   tYear = MOD( Time_diag/yearLength + yearPhase , 1.0 )

! Compute the declination angle
! a) approximate estimate of declination angle: relative error is less
!    than 0.03 for current obliq but as large as 0.21 for obliq=60^o
!  xDecl = - obliquity*deg_to_rad * cos(2.*pi*tYear)
! b) unaproximate expression:
   sDecl = -sin(  obliquity*deg_to_rad ) * cos(2.*pi*tYear)
   cDecl =  cos( asin( sDecl ) )
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

solar_tau_0 = (1.0 - sw_diff*ss*ss)*atm_abs

! set a constant albedo for testing
albedo(:,:) = albedo_value

do k = 1, n+1
  solar_tau(:,:,k) = solar_tau_0(:,:)                                         &
                             *(p_half(:,:,k)/p_half(:,:,n+1))**solar_exponent
end do

if ( wv_exponent .eq. 0. ) then
! longwave optical thickness function of specific humidity (M.Byrne & P.O'Gorman):
  do k = 1, n
    del_tau    = ( ir_tau_co2 + ir_tau_wv * q(:,:,k) )                         &
               * ( p_half(:,:,k+1)-p_half(:,:,k) ) / p_half(:,:,n+1)
    dtrans(:,:,k) = exp( - del_tau )
  end do

else
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
b = (1.0-window)*stefan*t*t*t*t

down(:,:,1) = 0.0
do k = 1,n
  down(:,:,k+1) = down(:,:,k)*dtrans(:,:,k) + b(:,:,k)*(1.0 - dtrans(:,:,k))
end do

do k = 1,n+1
  solar_down(:,:,k) = solar(:,:)*exp(-solar_tau(:,:,k))
end do

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
deallocate (solar_tau)
deallocate (ss, solar, solar_tau_0)
if ( select_incSW .eq. 0 ) then
  deallocate (p2)
elseif ( select_incSW .eq. 1 ) then
! Variables for seasonal Incoming SW
  deallocate (cLat, sLat, cos_H, HourAng)
endif
if ( wv_exponent .eq. 0. ) then
  deallocate (del_tau)
else
  deallocate (tau_0, tau_km, tau_kp)
endif
! -------------------------------------------------------------------------

return
end subroutine radiation_down

! ==================================================================================

!subroutine radiation_up (is, js, Time_diag, lat, p_half, t_surf, t, tdt)
subroutine radiation_up ( is, js, Time_diag, lat, p_half, t_surf, t, tdt,  &
                          olr, tsr,  albedo, dtrans, b, down, solar_down,  &
                           myThid )

! Now complete the radiation calculation by computing the upward and net fluxes.

integer, intent(in)                 :: is, js
!type(time_type), intent(in)         :: Time_diag
real, intent(in)                    :: Time_diag
real, intent(in) , dimension(:,:)   :: lat
real, intent(in) , dimension(:,:)   :: t_surf
real, intent(in) , dimension(:,:,:) :: t, p_half
real, intent(inout), dimension(:,:,:) :: tdt
real, intent(out), dimension(:,:)   :: olr
real, intent(out), dimension(:,:)   :: tsr      ! net Top SW (+=down)
real, intent(in),  dimension(:,:)   :: albedo
real, intent(in),  dimension(:,:,:) :: dtrans
real, intent(in),  dimension(:,:,:) :: b
real, intent(in),  dimension(:,:,:) :: down
real, intent(in),  dimension(:,:,:) :: solar_down
integer, intent(in)                 :: myThid

!integer :: i, j
integer :: k, n
integer :: im, jm

!logical :: used

! -------------------------------------------------------------------------
real, allocatable, dimension(:,:)     :: b_surf
real, allocatable, dimension(:,:,:)   :: tdt_rad, entrop_rad, tdt_sw
real, allocatable, dimension(:,:,:) :: up, net, flux_rad, flux_sw
! -------------------------------------------------------------------------

n = size(t,3)
im = size(t,1)
jm = size(t,2)

! -------------------------------------------------------------------------
allocate (tdt_rad          (im, jm, n))
allocate (tdt_sw           (im, jm, n))
allocate (entrop_rad       (im, jm, n))
allocate (up               (im, jm, n+1))
allocate (net              (im, jm, n+1))
allocate (flux_rad         (im, jm, n+1))
allocate (flux_sw          (im, jm, n+1))
allocate (b_surf           (im, jm))
! -------------------------------------------------------------------------

! total flux from surface
b_surf = stefan*t_surf*t_surf*t_surf*t_surf

! first deal with non-window upward flux
up(:,:,n+1) = b_surf*(1.0-window)
do k = n,1,-1
  up(:,:,k) = up(:,:,k+1)*dtrans(:,:,k) + b(:,:,k)*(1.0 - dtrans(:,:,k))
end do

! add upward flux in spectral window
do k = 1,n+1
  up(:,:,k) = up(:,:,k) + b_surf(:,:)*window
end do

do k = 1,n+1
  net(:,:,k) = up(:,:,k)-down(:,:,k)
  flux_sw(:,:,k) = albedo(:,:)*solar_down(:,:,n+1) - solar_down(:,:,k)
  flux_rad(:,:,k) = net(:,:,k) + flux_sw(:,:,k)
end do

do k = 1,n
  tdt_rad(:,:,k) = (net(:,:,k+1) - net(:,:,k) - solar_down(:,:,k+1) + solar_down(:,:,k))  &
             *grav/(cp_air*(p_half(:,:,k+1)-p_half(:,:,k)))
  tdt_sw(:,:,k) = (- solar_down(:,:,k+1) + solar_down(:,:,k))  &
             *grav/(cp_air*(p_half(:,:,k+1)-p_half(:,:,k)))
  tdt(:,:,k) = tdt(:,:,k) + tdt_rad(:,:,k)
end do

olr = up(:,:,1)
tsr=-flux_sw(:,:,1)

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
deallocate (tdt_rad, tdt_sw, entrop_rad)
deallocate (up, net, flux_rad, flux_sw)
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
