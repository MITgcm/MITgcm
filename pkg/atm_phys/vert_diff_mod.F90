! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/vert_diff_mod.F90,v 1.1 2013/05/08 22:14:15 jmc Exp $
! $Name:  $

module vert_diff_mod

!=======================================================================
!
!                         VERTICAL DIFFUSION MODULE
!
!      Routines for computing the tendencies due to vertical diffusion
!
!=======================================================================

use   constants_mod, only:  GRAV, RDGAS, RVGAS, CP_air

!use         fms_mod, only:  error_mesg, FATAL, uppercase, &
!                            write_version_number, stdlog, &
!                            mpp_pe, mpp_root_pe

!use   field_manager_mod, only: MODEL_ATMOS
!use  tracer_manager_mod, only: query_method, get_tracer_index

implicit none
private


! public interfaces
!=======================================================================
public :: gcm_vert_diff_init,          &
          gcm_vert_diff_end,           &
          gcm_vert_diff,               &
          gcm_vert_diff_down,          &
          gcm_vert_diff_up,            &
          vert_diff,                   &
          surf_diff_type

!=======================================================================

! form of interfaces
!=======================================================================

type surf_diff_type

  real, pointer, dimension(:,:) :: dtmass,    &
                                   dflux_t,   &
                                   dflux_q,   &
                                   delta_t,   &
                                   delta_q

end type surf_diff_type

!real,    allocatable, dimension(:,:,:) :: e_global, f_t_global, f_q_global

logical :: do_init = .true.
logical :: do_conserve_energy = .true.
logical :: use_virtual_temp_vert_diff, do_mcm_plev
integer :: sphum

!--------------------- version number ---------------------------------

character(len=128) :: version = '$Id: vert_diff_mod.F90,v 1.1 2013/05/08 22:14:15 jmc Exp $'
character(len=128) :: tag = '$Name:  $'

real, parameter :: d608 = (RVGAS-RDGAS)/RDGAS

contains

!#######################################################################

subroutine gcm_vert_diff_init (idim, jdim, kdim,              &
                               do_conserve_energy_in, myThid, &
                               use_virtual_temp_vert_diff_in, &
                               do_mcm_plev_in )

!type(surf_diff_type), intent(inout) :: Tri_surf
 integer,              intent(in)    :: idim, jdim, kdim
 logical,              intent(in)    :: do_conserve_energy_in
 integer, intent(in)                 :: myThid
 logical, optional,    intent(in)    :: use_virtual_temp_vert_diff_in
 logical, optional,    intent(in)    :: do_mcm_plev_in

!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

!   call write_version_number ( version, tag )

! get the tracer number for specific humidity
! jmc: --- need to do something ---
    sphum = 2
!   sphum = get_tracer_index( MODEL_ATMOS, 'sphum')
!   if (mpp_pe() == mpp_root_pe()) &
!   write (stdlog(),'(a,i4)') 'Tracer number for specific humidity =',sphum

    if(present(use_virtual_temp_vert_diff_in)) then
      use_virtual_temp_vert_diff = use_virtual_temp_vert_diff_in
    else
      use_virtual_temp_vert_diff = .false.
    endif
    if(present(do_mcm_plev_in)) then
      do_mcm_plev = do_mcm_plev_in
    else
      do_mcm_plev = .false.
    endif

 if (do_init) then

!   if (allocated(  e_global ))    deallocate (  e_global )
!   if (allocated(f_t_global ))    deallocate (f_t_global )
!   if (allocated(f_q_global ))    deallocate (f_q_global )

!   allocate(  e_global (idim, jdim, kdim-1))
!   allocate(f_t_global (idim, jdim, kdim-1))
!   allocate(f_q_global (idim, jdim, kdim-1))

    do_init = .false.

 endif

!call alloc_surf_diff_type ( Tri_surf, idim, jdim, myThid )

 do_conserve_energy = do_conserve_energy_in

     ENDIF
     CALL BARRIER(myThid)

end subroutine gcm_vert_diff_init

!#######################################################################

subroutine alloc_surf_diff_type ( Tri_surf, idim, jdim, myThid )

type(surf_diff_type), intent(inout) :: Tri_surf
integer,              intent(in)    :: idim, jdim
integer,              intent(in)    :: myThid

    allocate( Tri_surf%dtmass    (idim, jdim) )
    allocate( Tri_surf%dflux_t   (idim, jdim) )
    allocate( Tri_surf%dflux_q   (idim, jdim) )
    allocate( Tri_surf%delta_t   (idim, jdim) )
    allocate( Tri_surf%delta_q   (idim, jdim) )

end subroutine alloc_surf_diff_type

!#######################################################################

subroutine dealloc_surf_diff_type ( Tri_surf, myThid )

type(surf_diff_type), intent(inout) :: Tri_surf
integer,intent(in)                  :: myThid

      deallocate( Tri_surf%dtmass    )
      deallocate( Tri_surf%dflux_t   )
      deallocate( Tri_surf%dflux_q   )
      deallocate( Tri_surf%delta_t   )
      deallocate( Tri_surf%delta_q   )

end subroutine dealloc_surf_diff_type

!#######################################################################

subroutine gcm_vert_diff_end (myThid)

integer, intent(in)  ::myThid

  if (.not.do_init) then

!   if (allocated(   e_global ))    deallocate (   e_global)
!   if (allocated( f_t_global ))    deallocate ( f_t_global)
!   if (allocated( f_q_global ))    deallocate ( f_q_global)

  endif

end subroutine gcm_vert_diff_end

!#######################################################################

subroutine gcm_vert_diff_down (is, js, delt,                 &
                         u, v, t, q,                         &
!                        tr,                                 &
                         diff_m, diff_t, p_half, p_full,     &
                         z_full, tau_u, tau_v, dtau_datmos,  &
!                        flux_tr,                            &
                         dt_u, dt_v, dt_t, dt_q,             &
!                        dt_tr,                              &
                         dissipative_heat, tri_surf_dtmass,  &
                         tri_surf_dflux_t, tri_surf_dflux_q, &
                         tri_surf_delta_t, tri_surf_delta_q, &
                         e_global, f_t_global, f_q_global,   &
                         myThid, kbot )

integer, intent(in)                        :: is, js
real,    intent(in)                        :: delt
real,    intent(in)   , dimension(:,:,:)   :: u, v, t, q,     &
                                              diff_m, diff_t, &
                                              p_half, p_full, &
                                              z_full
!real,    intent(in)   , dimension(:,:,:,:) :: tr
real,    intent(in)   , dimension(:,:)     :: dtau_datmos
real,    intent(inout), dimension(:,:)     :: tau_u, tau_v
!real,    intent(inout), dimension(:,:,:)   :: flux_tr
real,    intent(inout), dimension(:,:,:)   :: dt_u, dt_v, dt_t
real,    intent(in),    dimension(:,:,:)   :: dt_q
!real,    intent(inout), dimension(:,:,:,:) :: dt_tr
real,    intent(out)  , dimension(:,:,:)   :: dissipative_heat
!type(surf_diff_type), intent(inout)        :: Tri_surf
real,    intent(inout), dimension(:,:)     :: tri_surf_dtmass
real,    intent(inout), dimension(:,:)     :: tri_surf_dflux_t, tri_surf_dflux_q
real,    intent(inout), dimension(:,:)     :: tri_surf_delta_t, tri_surf_delta_q
real,    intent(out),   dimension(:,:,:)   :: e_global, f_t_global, f_q_global
integer, intent(in)                        :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(u,1),size(u,2),size(u,3)) :: tt, mu, nu

real, dimension(size(u,1),size(u,2)) :: f_t_delt_n1, f_q_delt_n1, &
            mu_delt_n, nu_n, e_n1, delta_t_n, delta_q_n

real    :: gcp
integer :: i, j, kb, ie, je

!-----------------------------------------------------------------------

  if(do_init) call PRINT_ERROR ( &
       'gcm_vert_diff_down in vert_diff_mod'//  &
      'the initialization routine gcm_vert_diff_init has not been called', &
       myThid )
! if(do_init) call error_mesg ('gcm_vert_diff_down in vert_diff_mod',  &
!     'the initialization routine gcm_vert_diff_init has not been called', &
!      FATAL)

 ie = is + size(t,1) -1
 je = js + size(t,2) -1

 gcp       = GRAV/CP_air
 tt  = t + z_full*gcp   ! the vertical gradient of tt determines the
                        ! diffusive flux of temperature

 call compute_mu (p_half, mu, myThid)
 call compute_nu (diff_m, p_half, p_full, z_full, t, q, nu, myThid)

!  diffuse u-momentum and v_momentum

 call uv_vert_diff (delt, mu, nu, u, v, dtau_datmos, tau_u, tau_v,  &
                    dt_u, dt_v, dt_t, dissipative_heat, myThid, kbot)

!  recompute nu for a different diffusivity
 call compute_nu   (diff_t, p_half, p_full, z_full, t, q, nu, myThid)

!  diffuse tracers
!call tr_vert_diff (delt, mu, nu, tr, flux_tr, dt_tr, myThid, kbot )

!  downward sweep of tridiagonal solver for temperature and specific humidity
 call vert_diff_down_2                            &
         (delt, mu, nu, tt, q, dt_t, dt_q,        &
         e_global             (is:ie,js:je,:),    &
         f_t_global           (is:ie,js:je,:),    &
         f_q_global           (is:ie,js:je,:),    &
         mu_delt_n, nu_n, e_n1, f_t_delt_n1, f_q_delt_n1, &
         delta_t_n, delta_q_n, myThid, kbot)

! store information needed by flux_exchange module

    tri_surf_delta_t (is:ie,js:je) = delta_t_n + mu_delt_n*nu_n*f_t_delt_n1
    tri_surf_delta_q (is:ie,js:je) = delta_q_n + mu_delt_n*nu_n*f_q_delt_n1
    tri_surf_dflux_t (is:ie,js:je) = -nu_n*(1.0 - e_n1)
    tri_surf_dflux_q (is:ie,js:je) = -nu_n*(1.0 - e_n1)
    tri_surf_dtmass  (is:ie,js:je) = mu_delt_n

!-----------------------------------------------------------------------

end subroutine gcm_vert_diff_down

!#######################################################################

subroutine gcm_vert_diff_up (is, js, delt,                   &
                         tri_surf_delta_t, tri_surf_delta_q, &
                         e_global, f_t_global, f_q_global,   &
                         dt_t, dt_q, myThid, kbot)

integer, intent(in)                      :: is, js
real,    intent(in)                      :: delt
integer, intent(in)                      :: myThid
!type(surf_diff_type), intent(in)         :: Tri_surf
real,    intent(in),    dimension(:,:)   :: tri_surf_delta_t, tri_surf_delta_q
real,    intent(in),    dimension(:,:,:) :: e_global, f_t_global, f_q_global
real,    intent(out),   dimension(:,:,:) :: dt_t, dt_q
integer, intent(in),    dimension(:,:), optional :: kbot

integer :: ie, je

 ie = is + size(dt_t,1) -1
 je = js + size(dt_t,2) -1


 call vert_diff_up (delt ,                              &
                    e_global          (is:ie,js:je,:) , &
                    f_t_global        (is:ie,js:je,:) , &
                    tri_surf_delta_t  (is:ie,js:je) ,   &
                    dt_t, myThid, kbot )

 call vert_diff_up (delt ,                              &
                    e_global          (is:ie,js:je,:) , &
                    f_q_global        (is:ie,js:je,:) , &
                    tri_surf_delta_q  (is:ie,js:je) ,   &
                    dt_q, myThid, kbot )


end subroutine gcm_vert_diff_up

!#######################################################################

subroutine gcm_vert_diff (delt, u, v, t, q, tr,                    &
                          diff_m, diff_t, p_half, p_full, z_full,  &
                          dtau_datmos, dsens_datmos, devap_datmos, &
                          sens, evap, tau_u, tau_v, flux_tr,       &
                          dt_u, dt_v, dt_t, dt_q, dt_tr,           &
                          dissipative_heat, myThid, kbot      )

!  one-step diffusion call for gcm in which there is no implicit dependence of
!    surface fluxes on surface temperature

real,    intent(in)                          :: delt
real,    intent(in)   , dimension(:,:,:)     :: u, v, t, q, p_half, p_full, &
                                                z_full, diff_m, diff_t
real,    intent(in)   , dimension(:,:,:,:)   :: tr
real,    intent(in)   , dimension(:,:)       :: dtau_datmos, dsens_datmos, &
                                                devap_datmos
real,    intent(inout), dimension(:,:,:)     :: flux_tr
real,    intent(inout), dimension(:,:)       :: tau_u, tau_v, sens, evap
real,    intent(inout), dimension(:,:,:)     :: dt_u, dt_v, dt_t, dt_q
real,    intent(inout), dimension(:,:,:,:)   :: dt_tr
real,    intent(out)  , dimension(:,:,:)     :: dissipative_heat
integer, intent(in)                          :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(u,1),size(u,2),size(u,3)) :: mu, nu


!-----------------------------------------------------------------------

 call compute_mu (p_half, mu, myThid)

 call compute_nu (diff_m, p_half, p_full, z_full, t, q, nu, myThid)

 call uv_vert_diff (delt, mu, nu, u, v, dtau_datmos, tau_u, tau_v, &
                    dt_u, dt_v, dt_t, dissipative_heat, myThid, kbot )

 call compute_nu   (diff_t, p_half, p_full, z_full, t, q, nu, myThid)

 call tq_vert_diff (delt, mu, nu, t, q, z_full,  &
                    dsens_datmos, devap_datmos,  &
                    sens, evap, dt_t, dt_q, myThid, kbot )

 call tr_vert_diff (delt, mu, nu, tr, flux_tr, dt_tr, myThid, kbot )

end subroutine gcm_vert_diff

!#######################################################################

subroutine vert_diff (delt, xi, t, q, diff, p_half, p_full, z_full, &
                      flux, dflux_datmos, factor, dt_xi, myThid, kbot)

! one-step diffusion of a single field

real,    intent(in)                          :: delt
real,    intent(in)   , dimension(:,:,:)     :: xi, t, q, diff, p_half, p_full, z_full
real,    intent(inout), dimension(:,:)       :: flux
real,    intent(in)   , dimension(:,:)       :: dflux_datmos
real,    intent(in)                          :: factor
real,    intent(inout), dimension(:,:,:)     :: dt_xi
integer, intent(in)                          :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(xi,1),size(xi,2),size(xi,3)  ) :: mu, nu
real, dimension(size(xi,1),size(xi,2),size(xi,3)-1) :: e, f

real, dimension(size(xi,1),size(xi,2))  :: mu_delt_n, nu_n, e_n1,  &
                                           f_delt_n1, delta_xi_n

!-----------------------------------------------------------------------

 call compute_mu    (p_half, mu, myThid)

 call compute_nu    (diff, p_half, p_full, z_full, t, q, nu, myThid)

 call vert_diff_down &
     (delt, mu, nu, xi, dt_xi, e, f, mu_delt_n, nu_n, e_n1,  &
      f_delt_n1, delta_xi_n, myThid, kbot)

 call diff_surface (mu_delt_n, nu_n, e_n1, f_delt_n1,     &
                    dflux_datmos, flux, factor, delta_xi_n, myThid)

 call vert_diff_up (delt, e, f, delta_xi_n, dt_xi, myThid, kbot)

end subroutine vert_diff


!#######################################################################

subroutine uv_vert_diff (delt, mu, nu, u, v,  &
                         dtau_datmos, tau_u, tau_v, dt_u, dt_v, dt_t, &
                         dissipative_heat, myThid, kbot )

real,    intent(in)                        :: delt
real,    intent(in)   , dimension(:,:,:)   :: u, v, mu, nu
real,    intent(in)   , dimension(:,:)     :: dtau_datmos
real,    intent(inout), dimension(:,:)     :: tau_u, tau_v
real,    intent(inout), dimension(:,:,:)   :: dt_u, dt_v, dt_t
real,    intent(out)  , dimension(:,:,:)   :: dissipative_heat
integer, intent(in)                        :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(u,1),size(u,2)) :: mu_delt_n, nu_n, e_n1,    &
                                        f_u_delt_n1, f_v_delt_n1, &
                                        delta_u_n, delta_v_n

real, dimension(size(u,1),size(u,2),size(u,3)) :: dt_u_temp, dt_v_temp

real, dimension(size(u,1),size(u,2),size(u,3)-1) :: e, f_u, f_v
integer :: i, j, kb

real    :: half_delt, cp_inv


!-----------------------------------------------------------------------

 half_delt = 0.5*delt
 cp_inv    = 1.0/CP_air

 if (do_conserve_energy) then
   dt_u_temp = dt_u
   dt_v_temp = dt_v
 endif

 call vert_diff_down_2 &
     (delt, mu, nu, u, v, dt_u, dt_v, e, f_u, f_v, &
      mu_delt_n, nu_n, e_n1, f_u_delt_n1, f_v_delt_n1,  &
      delta_u_n, delta_v_n, myThid, kbot)

 call diff_surface (mu_delt_n, nu_n, e_n1, f_u_delt_n1, &
                    dtau_datmos, tau_u, 1.0, delta_u_n, myThid)
 call diff_surface (mu_delt_n, nu_n, e_n1, f_v_delt_n1, &
                    dtau_datmos, tau_v, 1.0, delta_v_n, myThid)

 call vert_diff_up (delt, e, f_u, delta_u_n, dt_u, myThid, kbot)
 call vert_diff_up (delt, e, f_v, delta_v_n, dt_v, myThid, kbot)

 if (do_conserve_energy) then
    dt_u_temp = dt_u - dt_u_temp
    dt_v_temp = dt_v - dt_v_temp
    dissipative_heat = - cp_inv*( (u + half_delt*dt_u_temp)*dt_u_temp &
                                 +(v + half_delt*dt_v_temp)*dt_v_temp )
    dt_t = dt_t + dissipative_heat
 else
    dissipative_heat = 0.0
 endif

!-----------------------------------------------------------------------

end subroutine uv_vert_diff

!#######################################################################

subroutine tq_vert_diff (delt, mu, nu, t, q,  z_full, &
                         dsens_datmos, devap_datmos, sens, evap, &
                         dt_t, dt_q, myThid,  kbot)


real,    intent(in)                        :: delt
real,    intent(in)   , dimension(:,:,:)   :: t, q, z_full, mu, nu
real,    intent(in)   , dimension(:,:)     :: dsens_datmos, devap_datmos
real,    intent(inout), dimension(:,:)     :: sens, evap
real,    intent(inout), dimension(:,:,:)   :: dt_t, dt_q
integer, intent(in)                        :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(t,1),size(t,2)) :: mu_delt_n, nu_n,          &
                                        e_n1, f_t_delt_n1, f_q_delt_n1, &
                                        delta_t_n, delta_q_n,      &
                                        flux1, dflux1_dsurf

real, dimension(size(t,1),size(t,2),size(t,3)-1) :: e, f_t, f_q
real, dimension(size(t,1),size(t,2),size(t,3)  ) :: tt

integer :: i, j, kb
real    :: gcp
!-----------------------------------------------------------------------

 gcp = GRAV/CP_air
 tt  = t + z_full*gcp

 call vert_diff_down_2 &
     (delt, mu, nu, tt, q, dt_t, dt_q, e, f_t, f_q,    &
      mu_delt_n, nu_n, e_n1, f_t_delt_n1, f_q_delt_n1, &
      delta_t_n, delta_q_n, myThid, kbot)


 call diff_surface (mu_delt_n, nu_n, e_n1, f_t_delt_n1,  &
                    dsens_datmos, sens, CP_air, delta_t_n, myThid)

 call diff_surface (mu_delt_n, nu_n, e_n1, f_q_delt_n1,  &
                    devap_datmos, evap, 1.0, delta_q_n, myThid)

 call vert_diff_up (delt, e, f_t, delta_t_n, dt_t, myThid, kbot)
 call vert_diff_up (delt, e, f_q, delta_q_n, dt_q, myThid, kbot)


!-----------------------------------------------------------------------

end subroutine tq_vert_diff

!#######################################################################

subroutine tr_vert_diff (delt, mu, nu, tr, flux, dt_tr, myThid, kbot )

real,    intent(in)                        :: delt
real,    intent(in)   , dimension(:,:,:)   :: mu, nu
real,    intent(in)   , dimension(:,:,:,:) :: tr
real,    intent(inout), dimension(:,:,:)   :: flux
real,    intent(inout), dimension(:,:,:,:) :: dt_tr
integer, intent(in)                        :: myThid

integer, intent(in)   , dimension(:,:), optional :: kbot

real, dimension(size(tr,1),size(tr,2)) :: mu_delt_n, nu_n, e_n1

real, dimension(size(tr,1),size(tr,2),size(tr,4)) :: f_delt_n1, delta_tr_n
real, dimension(size(tr,1),size(tr,2)) :: dflux_dtr

real, dimension(size(tr,1),size(tr,2),size(tr,3)-1,size(tr,4)) :: f

real, dimension(size(tr,1),size(tr,2),size(tr,3)-1) :: e
integer :: i, j, n, kb, ntr
character(len=128) :: scheme
logical, dimension(size(dt_tr,4)) :: skip_tracer_diff
!-----------------------------------------------------------------------

 ntr  = size(tr,4) ! number of prognostic tracers


 ! setup flags for tracer diffusion
 ! this could be moved to the initialization
 skip_tracer_diff(1:ntr) = .true.
 do n=1,ntr
   ! skip specific humidity (done separately)
     if ( n == sphum ) cycle
   ! skip tracers if diffusion scheme truned off
! jmc: --- need to do something ---
!    if (query_method('diff_vert',MODEL_ATMOS,n,scheme)) then
!        if(uppercase(trim(scheme)) == 'NONE') cycle
!    endif
     skip_tracer_diff(n) = .false.
 enddo


 dflux_dtr = 0.0

 call vert_diff_down_n &
     (delt, mu, nu, tr, dt_tr, e, f, mu_delt_n, nu_n,  &
      e_n1, f_delt_n1, delta_tr_n, myThid, skip_tracer_diff, kbot)

 do n = 1, ntr
   if (skip_tracer_diff(n)) cycle
   call diff_surface (mu_delt_n, nu_n, e_n1, f_delt_n1(:,:,n),  &
                      dflux_dtr, flux(:,:,n), 1.0, delta_tr_n(:,:,n), myThid)

   call vert_diff_up (delt, e, f(:,:,:,n), delta_tr_n(:,:,n),  &
                      dt_tr(:,:,:,n), myThid, kbot)
 end do

!-----------------------------------------------------------------------

end subroutine tr_vert_diff

!#######################################################################

subroutine vert_diff_down &
      (delt, mu, nu, tr, dt_tr, e, f, mu_delt_n, nu_n,  &
       e_n1, f_delt_n1, delta_tr_n, myThid, kbot)

!-----------------------------------------------------------------------

real,    intent(in)                         :: delt
real,    intent(in)    , dimension(:,:,:)   :: mu, nu
real,    intent(in)    , dimension(:,:,:)   :: tr
real,    intent(inout) , dimension(:,:,:)   :: dt_tr
real,    intent(out)   , dimension(:,:,:)   :: e
real,    intent(out)   , dimension(:,:,:)   :: f
real,    intent(out)   , dimension(:,:)     :: mu_delt_n, nu_n, e_n1
real,    intent(out)   , dimension(:,:)     :: f_delt_n1, delta_tr_n
integer, intent(in)                         :: myThid

integer, intent(in),    dimension(:,:), optional :: kbot

real, dimension(size(tr,1),size(tr,2),size(tr,3)) :: a, b, c, g

integer :: i, j, k, kb, nlev

!-----------------------------------------------------------------------

 call explicit_tend (mu, nu, tr, dt_tr, myThid)

 call compute_e  (delt, mu, nu, e, a, b, c, g, myThid)

 call compute_f (dt_tr, b, c, g, f, myThid)


 if (present(kbot)) then
    do j=1,size(tr,2)
    do i=1,size(tr,1)
        kb = kbot(i,j)
        mu_delt_n(i,j) =  mu(i,j,kb  )*delt
             nu_n(i,j) =  nu(i,j,kb  )
             e_n1(i,j) =   e(i,j,kb-1)
    enddo
    enddo
    do j=1,size(tr,2)
    do i=1,size(tr,1)
        kb = kbot(i,j)
         f_delt_n1(i,j) =     f(i,j,kb-1)*delt
        delta_tr_n(i,j) = dt_tr(i,j,kb  )*delt
    enddo
    enddo
 else
        nlev = size(mu,3)
        mu_delt_n(:,:) =       mu(:,:,nlev  )*delt
             nu_n(:,:) =       nu(:,:,nlev  )
             e_n1(:,:) =        e(:,:,nlev-1)
        f_delt_n1(:,:) =        f(:,:,nlev-1)*delt
       delta_tr_n(:,:) =    dt_tr(:,:,nlev  )*delt
 endif



!-----------------------------------------------------------------------

end subroutine vert_diff_down

!#######################################################################

subroutine vert_diff_down_2 &
      (delt, mu, nu, xi_1, xi_2, dt_xi_1, dt_xi_2, e, f_1, f_2, &
       mu_delt_n, nu_n, e_n1, f_1_delt_n1, f_2_delt_n1,         &
       delta_1_n, delta_2_n, myThid, kbot)

!-----------------------------------------------------------------------

real,    intent(in)                       :: delt
real,    intent(in)    , dimension(:,:,:) :: mu, nu, xi_1, xi_2
real,    intent(in)    , dimension(:,:,:) :: dt_xi_1, dt_xi_2
real,    intent(out)   , dimension(:,:,:) :: e, f_1, f_2
real,    intent(out)   , dimension(:,:)   :: mu_delt_n, nu_n, e_n1,    &
                                             f_1_delt_n1, f_2_delt_n1, &
                                             delta_1_n, delta_2_n
integer, intent(in)                       :: myThid

integer, intent(in),    dimension(:,:), optional :: kbot

real, dimension(size(xi_1,1),size(xi_1,2),size(xi_1,3)) :: a, b, c, g, &
                                                      dt_xi_11, dt_xi_22

integer :: i, j, k, kb, nlev

!-----------------------------------------------------------------------

! local copy of input
  dt_xi_11 = dt_xi_1
  dt_xi_22 = dt_xi_2

 call explicit_tend (mu, nu, xi_1, dt_xi_11, myThid)
 call explicit_tend (mu, nu, xi_2, dt_xi_22, myThid)

 call compute_e (delt, mu, nu, e, a, b, c, g, myThid)

 call compute_f (dt_xi_11, b, c, g, f_1, myThid)
 call compute_f (dt_xi_22, b, c, g, f_2, myThid)

 if (present(kbot)) then
    do j=1,size(xi_1,2)
    do i=1,size(xi_1,1)
        kb = kbot(i,j)
        mu_delt_n(i,j)  =      mu(i,j,kb  )*delt
             nu_n(i,j)  =      nu(i,j,kb  )
            e_n1(i,j)  =       e(i,j,kb-1)
     f_1_delt_n1(i,j)  =     f_1(i,j,kb-1)*delt
     f_2_delt_n1(i,j)  =     f_2(i,j,kb-1)*delt
        delta_1_n(i,j)  = dt_xi_11(i,j,kb  )*delt
        delta_2_n(i,j)  = dt_xi_22(i,j,kb  )*delt
    enddo
    enddo
 else
        nlev = size(mu,3)
        mu_delt_n(:,:)  =      mu(:,:,nlev  )*delt
             nu_n(:,:)  =      nu(:,:,nlev  )
            e_n1(:,:)  =       e(:,:,nlev-1)
     f_1_delt_n1(:,:)  =     f_1(:,:,nlev-1)*delt
     f_2_delt_n1(:,:)  =     f_2(:,:,nlev-1)*delt
        delta_1_n(:,:)  = dt_xi_11(:,:,nlev  )*delt
        delta_2_n(:,:)  = dt_xi_22(:,:,nlev  )*delt
 endif



!-----------------------------------------------------------------------

end subroutine vert_diff_down_2

!#######################################################################

subroutine vert_diff_down_n &
      (delt, mu, nu, tr, dt_tr, e, f, mu_delt_n, nu_n,  &
       e_n1, f_delt_n1, delta_tr_n, myThid, skip, kbot)

!-----------------------------------------------------------------------

real,    intent(in)                         :: delt
real,    intent(in)    , dimension(:,:,:)   :: mu, nu
real,    intent(in)    , dimension(:,:,:,:) :: tr
real,    intent(inout) , dimension(:,:,:,:) :: dt_tr
real,    intent(out)   , dimension(:,:,:)   :: e
real,    intent(out)   , dimension(:,:,:,:) :: f
real,    intent(out)   , dimension(:,:)     :: mu_delt_n, nu_n, e_n1
real,    intent(out)   , dimension(:,:,:)   :: f_delt_n1, delta_tr_n
integer, intent(in)                         :: myThid

logical, intent(in),    dimension(:),   optional :: skip
integer, intent(in),    dimension(:,:), optional :: kbot

real, dimension(size(tr,1),size(tr,2),size(tr,3)) :: a, b, c, g

integer :: i, j, k, n, kb, nlev, ntr

!-----------------------------------------------------------------------

  ntr = size(tr,4)

 call compute_e  (delt, mu, nu, e, a, b, c, g, myThid)

 do n = 1, ntr
   if (present(skip)) then
       if(skip(n)) cycle
   endif
   call explicit_tend (mu, nu, tr(:,:,:,n), dt_tr(:,:,:,n), myThid)
   call compute_f (dt_tr(:,:,:,n), b, c, g, f(:,:,:,n), myThid)
 end do


 if (present(kbot)) then
    do j=1,size(tr,2)
    do i=1,size(tr,1)
        kb = kbot(i,j)
        mu_delt_n(i,j) =  mu(i,j,kb  )*delt
             nu_n(i,j) =  nu(i,j,kb  )
            e_n1(i,j) =   e(i,j,kb-1)
    enddo
    enddo
    do n=1,size(tr,4)
    if (present(skip)) then
       if(skip(n)) cycle
    endif
    do j=1,size(tr,2)
    do i=1,size(tr,1)
        kb = kbot(i,j)
       f_delt_n1(i,j,n)  =     f(i,j,kb-1,n)*delt
       delta_tr_n(i,j,n) = dt_tr(i,j,kb  ,n)*delt
    enddo
    enddo
    enddo
 else
    nlev = size(mu,3)
    mu_delt_n(:,:)  =  mu(:,:,nlev  )*delt
         nu_n(:,:)  =  nu(:,:,nlev  )
         e_n1(:,:)  =   e(:,:,nlev-1)
    do n=1,size(tr,4)
      if (present(skip)) then
          if(skip(n)) cycle
      endif
       f_delt_n1(:,:,n) =     f(:,:,nlev-1,n)*delt
      delta_tr_n(:,:,n) = dt_tr(:,:,nlev  ,n)*delt
    enddo
 endif



!-----------------------------------------------------------------------

end subroutine vert_diff_down_n

!#######################################################################

subroutine diff_surface (mu_delt, nu, e_n1, f_delt_n1,  &
                         dflux_datmos, flux, factor, delta_xi, myThid)

!-----------------------------------------------------------------------

real, intent(in)   , dimension(:,:) :: mu_delt, nu, e_n1, f_delt_n1,  &
                                       dflux_datmos
real, intent(inout), dimension(:,:) :: flux, delta_xi
real, intent(in) :: factor
integer, intent(in)                 :: myThid

!-----------------------------------------------------------------------

 real, dimension(size(flux,1),size(flux,2)) :: dflux
 real :: fff

 fff = 1.0/factor

 dflux    = - nu*(1.0 - e_n1)
 delta_xi = delta_xi + mu_delt*nu*f_delt_n1

 delta_xi = (delta_xi + mu_delt*flux*fff)/&
                      (1.0 - mu_delt*(dflux + dflux_datmos*fff))

 flux     = flux + dflux_datmos*delta_xi


!-----------------------------------------------------------------------

end subroutine diff_surface

!#######################################################################

subroutine vert_diff_up (delt, e, f, delta_xi_n, dt_xi, myThid, kbot)

!-----------------------------------------------------------------------

real,    intent(in)                      :: delt
real,    intent(in),    dimension(:,:,:) :: e, f
real,    intent(in) ,   dimension(:,:)   :: delta_xi_n
real,    intent(out),   dimension(:,:,:) :: dt_xi
integer, intent(in)                      :: myThid
integer, intent(in),    dimension(:,:), optional :: kbot

integer :: i, j, k, kb, nlev
!-----------------------------------------------------------------------

 if (present(kbot)) then
     do j = 1, size(dt_xi,2)
     do i = 1, size(dt_xi,1)
         kb = kbot(i,j)
         dt_xi(i,j,kb) = delta_xi_n(i,j)/delt
         do k = kb -1, 1, -1
           dt_xi(i,j,k) = e(i,j,k)*dt_xi(i,j,k+1) + f(i,j,k)
         end do
     end do
     end do
 else
    nlev = size(dt_xi,3)
    dt_xi(:,:,nlev) = delta_xi_n/delt
    do k = size(dt_xi,3)-1, 1, -1
      dt_xi(:,:,k) = e(:,:,k)*dt_xi(:,:,k+1) + f(:,:,k)
    end do
 endif

!-----------------------------------------------------------------------

end subroutine vert_diff_up

!#######################################################################

subroutine compute_e (delt, mu, nu, e, a, b, c, g, myThid)

!-----------------------------------------------------------------------

real,    intent(in)                       :: delt
real,    intent(in)    , dimension(:,:,:) :: mu, nu
real,    intent(out)   , dimension(:,:,:) :: e, a, b, c, g
integer, intent(in)                       :: myThid

integer :: k, nlev

!-----------------------------------------------------------------------

 nlev = size(mu,3)

 a(:,:,1:nlev-1) = - mu(:,:,1:nlev-1)*nu(:,:,2:nlev)*delt
 a(:,:,nlev    ) =   0.0
 c(:,:,2:nlev  ) = - mu(:,:,2:nlev  )*nu(:,:,2:nlev)*delt
 c(:,:,1       ) =   0.0

 b = 1.0 - a - c

 e(:,:,1)   =   - a(:,:,1)/b(:,:,1)
 do  k= 2, nlev - 1
    g(:,:,k) = 1.0/(b(:,:,k) + c(:,:,k)*e(:,:,k-1))
    e(:,:,k) = - a(:,:,k)*g(:,:,k)
 enddo

!-----------------------------------------------------------------------

end subroutine compute_e

!#######################################################################

subroutine compute_f (dt_xi, b, c, g, f, myThid)

!-----------------------------------------------------------------------
real,    intent(in)    , dimension(:,:,:) :: dt_xi, b, c, g
real,    intent(out)   , dimension(:,:,:) :: f
integer, intent(in)                       :: myThid

integer :: k
!-----------------------------------------------------------------------

 f(:,:,1) =   dt_xi(:,:,1)/b(:,:,1)

 do  k = 2, size(b,3)-1
    f(:,:,k) = (dt_xi(:,:,k) - c(:,:,k)*f(:,:,k-1))*g(:,:,k)
 enddo

!-----------------------------------------------------------------------

end subroutine compute_f

!#######################################################################

subroutine explicit_tend (mu, nu, xi, dt_xi, myThid)

!-----------------------------------------------------------------------

real,    intent(in)    , dimension(:,:,:) :: mu, nu, xi
real,    intent(inout) , dimension(:,:,:) :: dt_xi
integer, intent(in)                       :: myThid

real, dimension(size(xi,1),size(xi,2),size(xi,3)) :: fluxx

integer :: k, nlev

!-----------------------------------------------------------------------

 nlev = size(mu,3)

 fluxx(:,:,1)      = 0.0
 fluxx(:,:,2:nlev) = nu(:,:,2:nlev)*(xi(:,:,2:nlev) - xi(:,:,1:nlev-1))

 dt_xi(:,:,1:nlev-1) = dt_xi(:,:,1:nlev-1) +  &
    mu(:,:,1:nlev-1)*(fluxx(:,:,2:nlev) - fluxx(:,:,1:nlev-1))
 dt_xi(:,:,nlev)     = dt_xi(:,:,nlev) - mu(:,:,nlev)*fluxx(:,:,nlev)

!-----------------------------------------------------------------------

end subroutine explicit_tend

!#######################################################################

subroutine compute_mu (p_half, mu, myThid)

!-----------------------------------------------------------------------
real,    intent(in)    , dimension(:,:,:) :: p_half
real,    intent(out)   , dimension(:,:,:) :: mu
integer, intent(in)                       :: myThid

integer :: nlev
!-----------------------------------------------------------------------

nlev = size(mu,3)

mu(:,:,1:nlev) = GRAV / (p_half(:,:,2:nlev+1) -p_half(:,:,1:nlev))

!-----------------------------------------------------------------------

end subroutine compute_mu


!#######################################################################

subroutine compute_nu (diff, p_half, p_full, z_full, t, q, nu, myThid)

!-----------------------------------------------------------------------
real,    intent(in)    , dimension(:,:,:) :: diff, p_half, p_full, &
                                             z_full, t, q
real,    intent(out)   , dimension(:,:,:) :: nu
integer, intent(in)                       :: myThid

real, dimension(size(t,1),size(t,2),size(t,3)) :: rho_half, tt
integer :: nlev
!-----------------------------------------------------------------------

nlev = size(nu,3)

if ( use_virtual_temp_vert_diff ) then
  tt = t * (1.0 + d608*q)           ! virtual temperature
else
  tt = t ! Take out virtual temperature effect here to mimic supersource
endif

rho_half(:,:,2:nlev) =  &         ! density at half levels
      2.0*p_half(:,:,2:nlev)/(RDGAS*(tt(:,:,2:nlev)+tt(:,:,1:nlev-1)))

if(do_mcm_plev) then
  nu(:,:,2:nlev) = GRAV*rho_half(:,:,2:nlev)*rho_half(:,:,2:nlev)*diff(:,:,2:nlev)/ &
                    (p_full(:,:,2:nlev)-p_full(:,:,1:nlev-1))
else
  nu(:,:,2:nlev) = rho_half(:,:,2:nlev)*diff(:,:,2:nlev) /  &
                    (z_full(:,:,1:nlev-1)-z_full(:,:,2:nlev))
endif
!-----------------------------------------------------------------------

end subroutine compute_nu

!#######################################################################

end module vert_diff_mod

