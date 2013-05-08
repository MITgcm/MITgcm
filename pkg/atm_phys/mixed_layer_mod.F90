! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/mixed_layer_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

module mixed_layer_mod

!
! Implementation of mixed layer boundary condition
!

!use                  fms_mod, only: set_domain, write_version_number, &
!                                    mpp_pe, mpp_root_pe, error_mesg, FATAL, WARNING

!use                  fms_mod, only: stdlog, check_nml_error, close_file,&
!                                    open_namelist_file, stdout, file_exist, &
!                                    read_data, write_data, open_file

use           gcm_params_mod, only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit
use            constants_mod, only: HLV, PI, RHO_CP, CP_AIR

!use         diag_manager_mod, only: register_diag_field, send_data

!use       time_manager_mod,   only: time_type

!use           transforms_mod, only: get_deg_lat

!use            vert_diff_mod, only: surf_diff_type

implicit none
private
!===================================================================================================

character(len=128) :: version = '$Id: mixed_layer_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
character(len=128) :: tagname = '$Name:  $'
character(len=128), parameter :: mod_name='mixed_layer'

!===================================================================================================

public :: mixed_layer_init, mixed_layer, mixed_layer_end

!===================================================================================================

logical :: evaporation = .true.
real    :: qflux_amp = 0.0
real    :: qflux_width = 16.0  ! width of qflux region in degrees
real    :: depth = 40.0

namelist/mixed_layer_nml/ evaporation, qflux_amp, depth, qflux_width

!===================================================================================================

logical :: module_is_initialized =.false.
logical :: used

integer :: iter
integer, dimension(4) :: axes
!integer ::                                                                    &
!     id_t_surf,            &   ! surface temperature
!     id_flux_lhe,          &   ! latent heat flux at surface
!     id_flux_oceanq,       &   ! oceanic Q flux
!     id_flux_t                 ! sensible heat flux at surface

!real, allocatable, dimension(:,:)   ::                                        &
!     ocean_qflux,           &   ! Q-flux
!     rad_lat_2d                 ! latitude in radians

!real, allocatable, dimension(:)   :: deg_lat

!real, allocatable, dimension(:,:)   ::                                        &
!     gamma_t,               &   ! Used to calculate the implicit
!     gamma_q,               &   ! correction to the diffusion in
!     fn_t,                  &   ! the lowest layer
!     fn_q,                  &   !
!     en_t,                  &   !
!     en_q,                  &   !
!     alpha_t,               &   !
!     alpha_q,               &   !
!     alpha_lw,              &   !
!     beta_t,                &   !
!     beta_q,                &   !
!     beta_lw,               &   !
!     t_surf_dependence,     &   !
!     corrected_flux,        &   !
!     eff_heat_capacity,     &   ! Effective heat capacity
!     delta_t_surf               ! Increment in surface temperature

real inv_cp_air

!===================================================================================================
contains
!===================================================================================================

!subroutine mixed_layer_init(is, ie, js, je, num_levels, t_surf, axes,  &
subroutine mixed_layer_init( is, ie, js, je, num_levels,         axes,  &
!                            rad_lat_2d, ocean_qflux,                   &
                             Time, myThid )

!type(time_type), intent(in)       :: Time
real, intent(in)                   :: Time
!real, intent(inout), dimension(:,:) :: t_surf
integer, intent(in), dimension(4) :: axes
integer, intent(in) :: is, ie, js, je, num_levels
integer, intent(in)                :: myThid
!real, intent(in), dimension(:,:)  :: rad_lat_2d
!real, intent(out), dimension(:,:) :: ocean_qflux

integer :: j
real    :: rad_qwidth
integer:: ierr, io, unit
integer         :: iUnit
CHARACTER*(gcm_LEN_MBUF) :: msgBuf

if(module_is_initialized) return

! read namelist and copy to logfile
!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

     WRITE(msgBuf,'(A)') 'MIXED_LAYER_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'MIXED_LAYER_INIT',       &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=mixed_layer_nml)
     WRITE(msgBuf,'(A)')                                            &
          'MIXED_LAYER_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
     CLOSE(iUnit)

!call write_version_number(version, tagname)

!unit = open_namelist_file ()
!ierr=1
!do while (ierr /= 0)
!  read  (unit, nml=mixed_layer_nml, iostat=io, end=10)
!  ierr = check_nml_error (io, 'mixed_layer_nml')
!enddo
!10 call close_file (unit)

!if ( mpp_pe() == mpp_root_pe() )   write (stdlog(), nml=mixed_layer_nml)

!allocate(rad_lat_2d              (is:ie, js:je))
!allocate(ocean_qflux             (is:ie, js:je))

!allocate(deg_lat                 (js:je))

!allocate(gamma_t                 (is:ie, js:je))
!allocate(gamma_q                 (is:ie, js:je))
!allocate(en_t                    (is:ie, js:je))
!allocate(en_q                    (is:ie, js:je))
!allocate(fn_t                    (is:ie, js:je))
!allocate(fn_q                    (is:ie, js:je))
!allocate(alpha_t                 (is:ie, js:je))
!allocate(alpha_q                 (is:ie, js:je))
!allocate(alpha_lw                (is:ie, js:je))
!allocate(beta_t                  (is:ie, js:je))
!allocate(beta_q                  (is:ie, js:je))
!allocate(beta_lw                 (is:ie, js:je))
!allocate(delta_t_surf            (is:ie, js:je))
!allocate(eff_heat_capacity       (is:ie, js:je))
!allocate(corrected_flux          (is:ie, js:je))
!allocate(t_surf_dependence       (is:ie, js:je))

inv_cp_air = 1.0 / CP_AIR

module_is_initialized = .true.

     ENDIF
     CALL BARRIER(myThid)

!see if restart file exists for the surface temperature
!
!if (file_exist('INPUT/mixed_layer.res')) then
!         unit = open_file (file='INPUT/mixed_layer.res', &
!                           form='native', action='read')
!         call read_data (unit, t_surf)
!         call close_file (unit)
!else if (file_exist('INPUT/swamp.res')) then
!         unit = open_file (file='INPUT/swamp.res', &
!                           form='native', action='read')
!         call read_data (unit, t_surf)
!         call close_file (unit)
!  call error_mesg('mixed_layer','mixed_layer restart file not found, using swamp restart file', WARNING)
!else
!  call error_mesg('mixed_layer','mixed_layer restart file not found', WARNING)
!endif

!id_t_surf = register_diag_field(mod_name, 't_surf',        &
!                                axes(1:2), Time, 'surface temperature','K')
!id_flux_t = register_diag_field(mod_name, 'flux_t',        &
!                                axes(1:2), Time, 'sensible heat flux up at surface','watts/m2')
!id_flux_lhe = register_diag_field(mod_name, 'flux_lhe',        &
!                                 axes(1:2), Time, 'latent heat flux up at surface','watts/m2')
!id_flux_oceanq = register_diag_field(mod_name, 'flux_oceanq',        &
!                                 axes(1:2), Time, 'oceanic Q-flux','watts/m2')

! latitude will be needed for oceanic q flux
!call get_deg_lat(deg_lat)
!do j=js,je
!  rad_lat_2d(:,j) = deg_lat(j)*PI/180.
!enddo

! calculate ocean Q flux
!rad_qwidth = qflux_width*PI/180.
!ocean_qflux = qflux_amp*(1-2.*rad_lat_2d**2/rad_qwidth**2) * &
!        exp(- ((rad_lat_2d)**2/(rad_qwidth)**2))

return
end subroutine mixed_layer_init

!===================================================================================================

subroutine mixed_layer (                                &
     Time,                                              &
     t_surf,                                            &
     flux_t,                                            &
     flux_q,                                            &
     flux_r,                                            &
     dt,                                                &
     net_surf_sw_down,                                  &
     surf_lw_down,                                      &
!    Tri_surf,                                          &
                tri_surf_dtmass,                               &
                tri_surf_dflux_t, tri_surf_dflux_q,            &
                tri_surf_delta_t, tri_surf_delta_q,            &
     dhdt_surf,                                         &
     dedt_surf,                                         &
     dedq_surf,                                         &
     drdt_surf,                                         &
     dhdt_atm,                                          &
     dedq_atm,                                          &
     ocean_qflux,                                       &
     delta_t_surf, &               ! Increment in surface temperature
     myThid )

! ---- arguments -----------------------------------------------------------
!type(time_type), intent(in)       :: Time
real, intent(in)                  :: Time
real, intent(in),  dimension(:,:) :: &
     net_surf_sw_down, surf_lw_down
real, intent(inout), dimension(:,:) :: &
     flux_t,    flux_q,     flux_r
real, intent(inout), dimension(:,:) :: t_surf
real, intent(in), dimension(:,:) :: &
   dhdt_surf, dedt_surf, dedq_surf, &
   drdt_surf, dhdt_atm, dedq_atm
real, intent(in) :: dt
!type(surf_diff_type), intent(inout) :: Tri_surf
real, intent(inout), dimension(:,:)  :: tri_surf_dtmass
real, intent(inout), dimension(:,:)  :: tri_surf_dflux_t, tri_surf_dflux_q
real, intent(inout), dimension(:,:)  :: tri_surf_delta_t, tri_surf_delta_q
real, intent(in),    dimension(:,:)  :: ocean_qflux
real, intent(out),   dimension(:,:)  :: delta_t_surf
integer, intent(in) :: myThid

! ---- local variables --------------------------------------------------
real, allocatable, dimension(:,:)   ::                                        &
     gamma_t,               &   ! Used to calculate the implicit
     gamma_q,               &   ! correction to the diffusion in
     fn_t,                  &   ! the lowest layer
     fn_q,                  &   !
     en_t,                  &   !
     en_q,                  &   !
     alpha_t,               &   !
     alpha_q,               &   !
     alpha_lw,              &   !
     beta_t,                &   !
     beta_q,                &   !
     beta_lw,               &   !
     t_surf_dependence,     &   !
     corrected_flux,        &   !
     eff_heat_capacity          ! Effective heat capacity
!    eff_heat_capacity,     &   ! Effective heat capacity
!    delta_t_surf               ! Increment in surface temperature
integer :: im, jm
!----------------------------------------------------------------

im = size(t_surf,1)
jm = size(t_surf,2)
allocate(gamma_t            (im,jm) )
allocate(gamma_q            (im,jm) )
allocate(en_t               (im,jm) )
allocate(en_q               (im,jm) )
allocate(fn_t               (im,jm) )
allocate(fn_q               (im,jm) )
allocate(alpha_t            (im,jm) )
allocate(alpha_q            (im,jm) )
allocate(alpha_lw           (im,jm) )
allocate(beta_t             (im,jm) )
allocate(beta_q             (im,jm) )
allocate(beta_lw            (im,jm) )
allocate(eff_heat_capacity  (im,jm) )
allocate(corrected_flux     (im,jm) )
allocate(t_surf_dependence  (im,jm) )
!allocate(delta_t_surf       (im,jm) )

if(.not.module_is_initialized) then
! call error_mesg('mixed_layer','mixed_layer module is not initialized',FATAL)
  call PRINT_ERROR('mixed_layer: mixed_layer module is not initialized',myThid)
  STOP 'ABNORMAL END: S/R MIXED_LAYER'
endif

! Need to calculate the implicit changes to the lowest level delta_q and delta_t
! - see the discussion in vert_diff.tech.ps

! Care is needed to differentiate between the sensible heat flux and the
! diffusive flux of temperature

gamma_t = 1.0 / (1.0 - Tri_surf_dtmass * (Tri_surf_dflux_t + dhdt_atm * inv_cp_air))
gamma_q = 1.0 / (1.0 - Tri_surf_dtmass * (Tri_surf_dflux_q + dedq_atm))

fn_t = gamma_t * (Tri_surf_delta_t + Tri_surf_dtmass * flux_t * inv_cp_air)
fn_q = gamma_q * (Tri_surf_delta_q + Tri_surf_dtmass * flux_q)

en_t = gamma_t * Tri_surf_dtmass * dhdt_surf * inv_cp_air
en_q = gamma_q * Tri_surf_dtmass * dedt_surf

!
! Note flux_sw doesn't depend on surface or lowest layer values
! Note drdt_atm is not used - should be fixed
!
alpha_t = flux_t * inv_cp_air + dhdt_atm * inv_cp_air * fn_t
alpha_q = flux_q + dedq_atm * fn_q
alpha_lw = flux_r

beta_t = dhdt_surf * inv_cp_air + dhdt_atm * inv_cp_air * en_t
beta_q = dedt_surf + dedq_atm * en_q
beta_lw = drdt_surf

!
! Implement mixed layer surface boundary condition
!
corrected_flux = - net_surf_sw_down - surf_lw_down + alpha_t * CP_AIR + alpha_lw + ocean_qflux
t_surf_dependence = beta_t * CP_AIR + beta_lw


if (evaporation) then
  corrected_flux = corrected_flux + alpha_q * HLV
  t_surf_dependence = t_surf_dependence + beta_q * HLV
endif

!
! Now update the mixed layer surface temperature using an implicit step
!
eff_heat_capacity = depth * RHO_CP + t_surf_dependence * dt

if (any(eff_heat_capacity .eq. 0.0))  then
  write(*,*) 'mixed_layer: error', eff_heat_capacity
! call error_mesg('mixed_layer', 'Avoiding division by zero',fatal)
  call PRINT_ERROR('mixed_layer: Avoiding division by zero',myThid)
  STOP 'ABNORMAL END: S/R MIXED_LAYER'
end if

delta_t_surf = - corrected_flux  * dt / eff_heat_capacity

t_surf = t_surf + delta_t_surf

! Finally calculate the increments for the lowest atmospheric layer
!
Tri_surf_delta_t = fn_t + en_t * delta_t_surf
Tri_surf_delta_q = fn_q + en_q * delta_t_surf

! Note:
! When using an implicit step there is not a clearly defined flux for a given timestep

! Final flux values (match mixed layer heat content variations)
  flux_t = ( alpha_t  + delta_t_surf*beta_t )*CP_AIR
  flux_r =   alpha_lw + delta_t_surf*beta_lw
  flux_q =   alpha_q  + delta_t_surf*beta_q

!if(id_t_surf > 0) used = send_data(id_t_surf, t_surf, Time)
!if(id_flux_t > 0) used = send_data(id_flux_t, flux_t, Time)
!if(id_flux_lhe > 0) used = send_data(id_flux_lhe, HLV * flux_q, Time)
!if(id_flux_oceanq > 0)   used = send_data(id_flux_oceanq, ocean_qflux, Time)

deallocate(gamma_t            )
deallocate(gamma_q            )
deallocate(en_t               )
deallocate(en_q               )
deallocate(fn_t               )
deallocate(fn_q               )
deallocate(alpha_t            )
deallocate(alpha_q            )
deallocate(alpha_lw           )
deallocate(beta_t             )
deallocate(beta_q             )
deallocate(beta_lw            )
deallocate(eff_heat_capacity  )
deallocate(corrected_flux     )
deallocate(t_surf_dependence  )
!deallocate(delta_t_surf       )

end subroutine mixed_layer

!===================================================================================================

subroutine mixed_layer_end(t_surf,myThid)

real, intent(inout), dimension(:,:) :: t_surf
integer, intent(in)                :: myThid

integer:: unit

if(.not.module_is_initialized) return

! write a restart file for the surface temperature
!unit = open_file ('RESTART/mixed_layer.res', &
!                  form='native', action='write')
!call write_data (unit, t_surf)
!call close_File (unit)

module_is_initialized = .false.

end subroutine mixed_layer_end

!===================================================================================================

end module mixed_layer_mod
