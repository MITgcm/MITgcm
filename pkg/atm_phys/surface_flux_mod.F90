! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/surface_flux_mod.F90,v 1.1 2013/05/08 22:14:15 jmc Exp $
! $Name:  $

! ============================================================================
module surface_flux_mod
! ============================================================================

! use             fms_mod, only: FATAL, close_file, mpp_pe, mpp_root_pe,        &
!                               write_version_number
! use             fms_mod, only: file_exist, check_nml_error, open_namelist_file, stdlog

use   monin_obukhov_mod, only: mo_drag, mo_profile
use  simple_sat_vapor_pres_mod, only: escomp, descomp
use      gcm_params_mod, only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit
use       constants_mod, only: cp_air, hlv, stefan, rdgas, rvgas, grav

implicit none
private

! ==== public interface ======================================================
public  surface_flux
! ==== end of public interface ===============================================


interface surface_flux
!    module procedure surface_flux_0d
!    module procedure surface_flux_1d
    module procedure surface_flux_2d
end interface

!-----------------------------------------------------------------------

character(len=*), parameter :: version = '$Id: surface_flux_mod.F90,v 1.1 2013/05/08 22:14:15 jmc Exp $'
character(len=*), parameter :: tagname = '$Name:  $'

logical :: do_init = .true.

real, parameter :: d622   = rdgas/rvgas
real, parameter :: d378   = 1.-d622
real, parameter :: hlars  = hlv/rvgas
real, parameter :: gcp    = grav/cp_air
real, parameter :: kappa  = rdgas/cp_air
real            :: d608   = d378/d622
      ! d608 set to zero at initialization if the use of
      ! virtual temperatures is turned off in namelist


! ---- namelist with default values ------------------------------------------
logical :: no_neg_q         = .false.  ! for backwards compatibility
logical :: use_virtual_temp = .true.
logical :: alt_gustiness    = .false.
logical :: use_mixing_ratio = .false.
real    :: gust_const       =  1.0

namelist /surface_flux_nml/ no_neg_q,         &
                            use_virtual_temp, &
                            alt_gustiness,    &
                            gust_const,       &
                            use_mixing_ratio



contains


! ============================================================================
subroutine surface_flux_1d (                                           &
     t_atm,     q_atm_in,   u_atm,     v_atm,     p_atm,     z_atm,    &
     p_surf,    t_surf,     t_ca,      q_surf,                         &
     u_surf,    v_surf,                                                &
     rough_mom, rough_heat, rough_moist, gust,  &
     flux_t, flux_q, flux_r, flux_u, flux_v,                &
     cd_m,      cd_t,       cd_q,                                      &
     w_atm,     u_star,     b_star,     q_star,                        &
     dhdt_surf, dedt_surf,  dedq_surf,  drdt_surf,                     &
     dhdt_atm,  dedq_atm,   dtaudv_atm,                                &
! + slm Mar 28 2002 -- it is not necessary here since it is just cd_q*wind
!     drag_q,                                                           &
! - slm Mar 28 2002
     dt,        land,      avail, myThid  )
! ============================================================================
  ! ---- arguments -----------------------------------------------------------
  logical, intent(in), dimension(:) :: land,  avail
  real, intent(in),  dimension(:) :: &
       t_atm,     q_atm_in,   u_atm,     v_atm,              &
       p_atm,     z_atm,      t_ca,                          &
       p_surf,    t_surf,     u_surf,    v_surf,  &
       rough_mom, rough_heat, rough_moist,  gust
  real, intent(out), dimension(:) :: &
       flux_t,    flux_q,     flux_r,    flux_u,  flux_v,    &
       dhdt_surf, dedt_surf,  dedq_surf, drdt_surf,          &
       dhdt_atm,  dedq_atm,   dtaudv_atm,                    &
       w_atm,     u_star,     b_star,    q_star,             &
       cd_m,      cd_t,       cd_q
  real, intent(inout), dimension(:) :: q_surf
  real, intent(in) :: dt
  integer, intent(in) :: myThid

  ! ---- local constants -----------------------------------------------------
  ! temperature increment and its reciprocal value for comp. of derivatives
  real, parameter:: del_temp=0.1, del_temp_inv=1.0/del_temp

  ! ---- local vars ----------------------------------------------------------
  real, dimension(size(t_atm)) ::                          &
       thv_atm,  th_atm,   tv_atm,    thv_surf,            &
       e_sat,    e_sat1,   q_sat,     q_sat1,    p_ratio,  &
       t_surf0,  t_surf1,  u_dif,     v_dif,               &
       rho_drag, drag_t,    drag_m,   drag_q,    rho,      &
       q_atm,    q_surf0

  integer :: i, nbad


  if (do_init) call surface_flux_init(myThid)

  !---- use local value of surf temp ----

  t_surf0 = 200.   !  avoids out-of-bounds in es lookup
  where (avail)
     where (land)
        t_surf0 = t_ca
     elsewhere
        t_surf0 = t_surf
     endwhere
  endwhere

  t_surf1 = t_surf0 + del_temp

  call escomp ( t_surf0, e_sat  )  ! saturation vapor pressure
  call escomp ( t_surf1, e_sat1 )  ! perturbed  vapor pressure

  if(use_mixing_ratio) then
    ! surface mixing ratio at saturation
    q_sat   = d622*e_sat /(p_surf-e_sat )
    q_sat1  = d622*e_sat1/(p_surf-e_sat1)
  else
    ! surface specific humidity at saturation
    q_sat   = d622*e_sat /(p_surf-d378*e_sat )
    q_sat1  = d622*e_sat1/(p_surf-d378*e_sat1)
  endif

  ! initilaize surface air humidity according to surface type
  where (land)
     q_surf0 = q_surf ! land calculates it
  elsewhere
     q_surf0 = q_sat  ! everything else assumes saturated sfc humidity
  endwhere

  ! check for negative atmospheric humidities
  where(avail) q_atm = q_atm_in
  if(no_neg_q) then
     where(avail .and. q_atm_in < 0.0) q_atm = 0.0
  endif

  ! generate information needed by monin_obukhov
  where (avail)
     p_ratio = (p_surf/p_atm)**kappa

     tv_atm  = t_atm  * (1.0 + d608*q_atm)     ! virtual temperature
     th_atm  = t_atm  * p_ratio  ! potential T, using p_surf as refernce
     thv_atm = tv_atm * p_ratio  ! virt. potential T, using p_surf as reference
     thv_surf= t_surf0 * (1.0 + d608*q_surf0 ) ! surface virtual (potential) T
!     thv_surf= t_surf0  ! surface virtual (potential) T -- just for testing tun off the q_surf

     u_dif = u_surf - u_atm      ! velocity components relative to surface
     v_dif = v_surf - v_atm
  endwhere

  if(alt_gustiness) then
     where(avail) &
          w_atm = max(sqrt(u_dif*u_dif + v_dif*v_dif), gust_const)
  else
     where(avail) &
          w_atm = sqrt(u_dif*u_dif + v_dif*v_dif + gust*gust)
  endif

  !  monin-obukhov similarity theory
  call mo_drag (thv_atm, thv_surf, z_atm,                  &
       rough_mom, rough_heat, rough_moist, w_atm,          &
       cd_m, cd_t, cd_q, u_star, b_star, myThid, avail     )


  where (avail)
     ! surface layer drag coefficients
     drag_t = cd_t * w_atm
     drag_q = cd_q * w_atm
     drag_m = cd_m * w_atm

     ! density
     rho = p_atm / (rdgas * tv_atm)

     ! sensible heat flux
     rho_drag = cp_air * drag_t * rho
     flux_t = rho_drag * (t_surf0 - th_atm)  ! flux of sensible heat (W/m**2)
     dhdt_surf =  rho_drag                   ! d(sensible heat flux)/d(surface temperature)
     dhdt_atm  = -rho_drag*p_ratio           ! d(sensible heat flux)/d(atmos temperature)

     ! evaporation
     rho_drag  =  drag_q * rho
     flux_q    =  rho_drag * (q_surf0 - q_atm) ! flux of water vapor  (Kg/(m**2 s))
     where (land)
        dedq_surf = rho_drag
        dedt_surf = 0
     elsewhere
        dedq_surf = 0
        dedt_surf =  rho_drag * (q_sat1 - q_sat) *del_temp_inv
     endwhere

     dedq_atm  = -rho_drag   ! d(latent heat flux)/d(atmospheric mixing ratio)

     q_star = flux_q / (u_star * rho)             ! moisture scale
     ! ask Chris and Steve K if we still want to keep this for diagnostics
     q_surf = q_atm + flux_q / (rho*cd_q*w_atm)   ! surface specific humidity

     ! upward long wave radiation
     flux_r    =   stefan*t_surf**4               ! (W/m**2)
     drdt_surf = 4*stefan*t_surf**3               ! d(upward longwave)/d(surface temperature)

     ! stresses
     rho_drag   = drag_m * rho
     flux_u     = rho_drag * u_dif   ! zonal      component of stress (Nt/m**2)
     flux_v     = rho_drag * v_dif   ! meridional component of stress
     dtaudv_atm = -rho_drag          ! d(stress component)/d(atmos wind)

  elsewhere
     ! zero-out un-available data in output only fields
     flux_t     = 0.0
     flux_q     = 0.0
     flux_r     = 0.0
     flux_u     = 0.0
     flux_v     = 0.0
     dhdt_surf  = 0.0
     dedt_surf  = 0.0
     dedq_surf  = 0.0
     drdt_surf  = 0.0
     dhdt_atm   = 0.0
     dedq_atm   = 0.0
     dtaudv_atm = 0.0
     u_star     = 0.0
     b_star     = 0.0
     q_star     = 0.0
     q_surf     = 0.0
     w_atm      = 0.0
  endwhere

end subroutine surface_flux_1d

!#######################################################################

subroutine surface_flux_0d (                                                 &
     t_atm_0,     q_atm_0,      u_atm_0,     v_atm_0,   p_atm_0, z_atm_0,    &
     p_surf_0,    t_surf_0,     t_ca_0,      q_surf_0,                       &
     u_surf_0,    v_surf_0,                                                  &
     rough_mom_0, rough_heat_0, rough_moist_0, gust_0,                       &
     flux_t_0,    flux_q_0,     flux_r_0,    flux_u_0,  flux_v_0,            &
     cd_m_0,      cd_t_0,       cd_q_0,                                      &
     w_atm_0,     u_star_0,     b_star_0,     q_star_0,                      &
     dhdt_surf_0, dedt_surf_0,  dedq_surf_0,  drdt_surf_0,                   &
     dhdt_atm_0,  dedq_atm_0,   dtaudv_atm_0,                                &
     dt,          land_0,       avail_0, myThid  )

  ! ---- arguments -----------------------------------------------------------
  logical, intent(in) :: land_0,  avail_0
  real, intent(in) :: &
       t_atm_0,     q_atm_0,      u_atm_0,     v_atm_0,              &
       p_atm_0,     z_atm_0,      t_ca_0,                          &
       p_surf_0,    t_surf_0,     u_surf_0,    v_surf_0,  &
       rough_mom_0, rough_heat_0, rough_moist_0,  gust_0
  real, intent(out) :: &
       flux_t_0,    flux_q_0,     flux_r_0,    flux_u_0,  flux_v_0,    &
       dhdt_surf_0, dedt_surf_0,  dedq_surf_0, drdt_surf_0,          &
       dhdt_atm_0,  dedq_atm_0,   dtaudv_atm_0,                    &
       w_atm_0,     u_star_0,     b_star_0,    q_star_0,             &
       cd_m_0,      cd_t_0,       cd_q_0
  real, intent(inout) :: q_surf_0
  real, intent(in)    :: dt
  integer, intent(in) :: myThid

  ! ---- local vars ----------------------------------------------------------
  logical, dimension(1) :: land,  avail
  real, dimension(1) :: &
       t_atm,     q_atm,      u_atm,     v_atm,              &
       p_atm,     z_atm,      t_ca,                          &
       p_surf,    t_surf,     u_surf,    v_surf,  &
       rough_mom, rough_heat, rough_moist,  gust
  real, dimension(1) :: &
       flux_t,    flux_q,     flux_r,    flux_u,  flux_v,    &
       dhdt_surf, dedt_surf,  dedq_surf, drdt_surf,          &
       dhdt_atm,  dedq_atm,   dtaudv_atm,                    &
       w_atm,     u_star,     b_star,    q_star,             &
       cd_m,      cd_t,       cd_q
  real, dimension(1) :: q_surf


  avail = .true.

  t_atm(1)       = t_atm_0
  q_atm(1)       = q_atm_0
  u_atm(1)       = u_atm_0
  v_atm(1)       = v_atm_0
  p_atm(1)       = p_atm_0
  z_atm(1)       = z_atm_0
  p_surf(1)      = p_surf_0
  t_surf(1)      = t_surf_0
  u_surf(1)      = u_surf_0
  v_surf(1)      = v_surf_0
  rough_mom(1)   = rough_mom_0
  rough_heat(1)  = rough_heat_0
  rough_moist(1) = rough_moist_0
  gust(1)        = gust_0

  call surface_flux_1d (                                           &
       t_atm,     q_atm,      u_atm,     v_atm,     p_atm,     z_atm,    &
       p_surf,    t_surf,     t_ca,      q_surf,                         &
       u_surf,    v_surf,                                                &
       rough_mom, rough_heat, rough_moist, gust,  &
       flux_t, flux_q, flux_r, flux_u, flux_v,                &
       cd_m,      cd_t,       cd_q,                                      &
       w_atm,     u_star,     b_star,     q_star,                        &
       dhdt_surf, dedt_surf,  dedq_surf,  drdt_surf,                     &
       dhdt_atm,  dedq_atm,   dtaudv_atm,                                &
       dt,        land,      avail, myThid  )

  flux_t_0     = flux_t(1)
  flux_q_0     = flux_q(1)
  flux_r_0     = flux_r(1)
  flux_u_0     = flux_u(1)
  flux_v_0     = flux_v(1)
  dhdt_surf_0  = dhdt_surf(1)
  dedt_surf_0  = dedt_surf(1)
  drdt_surf_0  = drdt_surf(1)
  dedq_surf_0  = dedq_surf(1)
  dhdt_atm_0   = dhdt_atm(1)
  dedq_atm_0   = dedq_atm(1)
  dtaudv_atm_0 = dtaudv_atm(1)
  w_atm_0      = w_atm(1)
  u_star_0     = u_star(1)
  b_star_0     = b_star(1)
  q_star_0     = q_star(1)
  q_surf_0     = q_surf(1)
  cd_m_0       = cd_m(1)
  cd_t_0       = cd_t(1)
  cd_q_0       = cd_q(1)

end subroutine surface_flux_0d

subroutine surface_flux_2d (                                           &
     t_atm,     q_atm_in,   u_atm,     v_atm,     p_atm,     z_atm,    &
     p_surf,    t_surf,     t_ca,      q_surf,                         &
     u_surf,    v_surf,                                                &
     rough_mom, rough_heat, rough_moist, gust,                         &
     flux_t,    flux_q,     flux_r,    flux_u,    flux_v,              &
     cd_m,      cd_t,       cd_q,                                      &
     w_atm,     u_star,     b_star,     q_star,                        &
     dhdt_surf, dedt_surf,  dedq_surf,  drdt_surf,                     &
     dhdt_atm,  dedq_atm,   dtaudv_atm,                                &
     dt,        land,       avail, myThid  )

  ! ---- arguments -----------------------------------------------------------
  logical, intent(in), dimension(:,:) :: land,  avail
  real, intent(in),  dimension(:,:) :: &
       t_atm,     q_atm_in,   u_atm,     v_atm,              &
       p_atm,     z_atm,      t_ca,                          &
       p_surf,    t_surf,     u_surf,    v_surf,  &
       rough_mom, rough_heat, rough_moist,  gust
  real, intent(out), dimension(:,:) :: &
       flux_t,    flux_q,     flux_r,    flux_u,  flux_v,    &
       dhdt_surf, dedt_surf,  dedq_surf, drdt_surf,          &
       dhdt_atm,  dedq_atm,   dtaudv_atm,                    &
       w_atm,     u_star,     b_star,    q_star,             &
       cd_m,      cd_t,       cd_q
  real, intent(inout), dimension(:,:) :: q_surf
  real, intent(in) :: dt
  integer, intent (in) :: myThid

  ! ---- local vars -----------------------------------------------------------
  integer :: j
  do j = 1, size(t_atm,2)
     call surface_flux_1d (                                           &
          t_atm(:,j),     q_atm_in(:,j),   u_atm(:,j),     v_atm(:,j),     p_atm(:,j),     z_atm(:,j),    &
          p_surf(:,j),    t_surf(:,j),     t_ca(:,j),      q_surf(:,j),                         &
          u_surf(:,j),    v_surf(:,j),                                                &
          rough_mom(:,j), rough_heat(:,j), rough_moist(:,j), gust(:,j),                         &
          flux_t(:,j),    flux_q(:,j),     flux_r(:,j),    flux_u(:,j),    flux_v(:,j),              &
          cd_m(:,j),      cd_t(:,j),       cd_q(:,j),                                      &
          w_atm(:,j),     u_star(:,j),     b_star(:,j),     q_star(:,j),                        &
          dhdt_surf(:,j), dedt_surf(:,j),  dedq_surf(:,j),  drdt_surf(:,j),                     &
          dhdt_atm(:,j),  dedq_atm(:,j),   dtaudv_atm(:,j),                                &
          dt,             land(:,j),       avail(:,j), myThid  )
  end do
end subroutine surface_flux_2d


! ============================================================================
subroutine surface_flux_init (myThid)
! ============================================================================
! initializes surface flux module
  ! ---- local vars ----------------------------------------------------------
 integer, intent(in) :: myThid
 integer :: unit, ierr, io
!-- added:
 integer         :: iUnit
 CHARACTER*(gcm_LEN_MBUF) :: msgBuf

  ! read namelist

!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

     WRITE(msgBuf,'(A)') 'SURFACE_FLUX_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'SURFACE_FLUX_INIT',       &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=surface_flux_nml)
     WRITE(msgBuf,'(A)')                                            &
          'SURFACE_FLUX_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
     CLOSE(iUnit)

!  if ( file_exist('input.nml')) then
!     unit = open_namelist_file ()
!     ierr=1;
!     do while (ierr /= 0)
!        read  (unit, nml=surface_flux_nml, iostat=io, end=10)
!        ierr = check_nml_error(io,'surface_flux_nml')
!     enddo
!10   call close_file (unit)
!  endif

!  ! write version number
!  call write_version_number(version, tagname)

!  if ( mpp_pe() == mpp_root_pe() ) write (stdlog(), nml=surface_flux_nml)


!  if(.not. use_virtual_temp) d608 = 0.0

    do_init = .false.

    ENDIF
    CALL BARRIER (myThid)

end subroutine surface_flux_init


end module surface_flux_mod
