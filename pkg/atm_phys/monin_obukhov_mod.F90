! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/monin_obukhov_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

module monin_obukhov_mod


!=======================================================================
!
!                         MONIN-OBUKHOV MODULE
!
!          Routines for computing surface drag coefficients
!                 from data at the lowest model level
!              and for computing the profile of fields
!           between the lowest model level and the ground
!                  using Monin-Obukhov scaling
!
!=======================================================================


use gcm_params_mod, only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit
use constants_mod, only : grav, vonkarm
!use       fms_mod, only:  error_mesg, FATAL, file_exist,   &
!                          check_nml_error, open_namelist_file,      &
!                          mpp_pe, mpp_root_pe, close_file, stdlog, &
!                          write_version_number

implicit none
private

!=======================================================================
 public mo_drag
 public mo_profile
 public mo_diff
 public stable_mix
!=======================================================================

interface mo_drag
    module procedure  mo_drag_0d, mo_drag_1d, mo_drag_2d
end interface


interface mo_profile
    module procedure  mo_profile_0d,   mo_profile_1d,   mo_profile_2d, &
                      mo_profile_0d_n, mo_profile_1d_n, mo_profile_2d_n
end interface

interface mo_diff
    module procedure  mo_diff_0d_n, mo_diff_0d_1, &
                      mo_diff_1d_n, mo_diff_1d_1, &
                      mo_diff_2d_n, mo_diff_2d_1
end interface

interface stable_mix
    module procedure  stable_mix_0d, stable_mix_1d, &
                      stable_mix_2d, stable_mix_3d
end interface


!--------------------- version number ---------------------------------

character(len=128) :: version = '$Id: monin_obukhov_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
character(len=128) :: tagname = '$Name:  $'

!=======================================================================

!  DEFAULT VALUES OF NAMELIST PARAMETERS:

real    :: rich_crit      = 2.0
real    :: drag_min       = 1.e-05
logical :: neutral        = .false.
integer :: stable_option  = 1
real    :: zeta_trans     = 0.5


namelist /monin_obukhov_nml/ rich_crit, neutral, drag_min, &
                             stable_option, zeta_trans

!=======================================================================

!  MODULE VARIABLES

real, parameter    :: small  = 1.e-04
real               :: b_stab, r_crit, sqrt_drag_min, lambda, rich_trans
logical            :: init = .false.


contains

!=======================================================================

subroutine monin_obukhov_init(myThid)

integer, intent(in) :: myThid
integer :: unit, ierr, io

integer         :: iUnit
CHARACTER*(gcm_LEN_MBUF) :: msgBuf
!------------------- read namelist input -------------------------------

! read namelist and copy to logfile

!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

     WRITE(msgBuf,'(A)') 'MONIN_OBUKHOV_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'MONIN_OBUKHOV_INIT',       &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=monin_obukhov_nml)
     WRITE(msgBuf,'(A)')                                            &
          'MONIN_OBUKHOV_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
     CLOSE(iUnit)

!      if (file_exist('input.nml')) then
!         unit = open_namelist_file ()
!         ierr=1; do while (ierr /= 0)
!            read  (unit, nml=monin_obukhov_nml, iostat=io, end=10)
!            ierr = check_nml_error(io,'monin_obukhov_nml')
!         enddo
!  10     call close_file (unit)
!      endif

!---------- output namelist to log-------------------------------------

!      call write_version_number(version, tagname)
!      if ( mpp_pe() == mpp_root_pe() ) write (stdlog(), nml=monin_obukhov_nml)

!----------------------------------------------------------------------

if(rich_crit.le.0.25)  call PRINT_ERROR( &
        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD'// &
         'rich_crit in monin_obukhov_mod must be > 0.25', myThid )
!if(rich_crit.le.0.25)  call error_mesg( &
!        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD', &
!        'rich_crit in monin_obukhov_mod must be > 0.25', FATAL)

if(drag_min.lt.0.0)  call PRINT_ERROR( &
        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD'// &
        'drag_min in monin_obukhov_mod must be >= 0.0', myThid )
!if(drag_min.lt.0.0)  call error_mesg( &
!        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD', &
!        'drag_min in monin_obukhov_mod must be >= 0.0', FATAL)

if(stable_option < 1 .or. stable_option > 2) call PRINT_ERROR( &
        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD'// &
        'the only allowable values of stable_option are 1 and 2', myThid )
!if(stable_option < 1 .or. stable_option > 2) call error_mesg( &
!        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD', &
!        'the only allowable values of stable_option are 1 and 2', FATAL)

if(stable_option == 2 .and. zeta_trans < 0) call PRINT_ERROR( &
        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD'// &
        'zeta_trans must be positive', myThid )
!if(stable_option == 2 .and. zeta_trans < 0) call error_mesg( &
!        'MONIN_OBUKHOV_INIT in MONIN_OBUKHOV_MOD', &
!        'zeta_trans must be positive', FATAL)

b_stab = 1.0/rich_crit
r_crit = 0.95*rich_crit  ! convergence can get slow if one is
                         ! close to rich_crit

sqrt_drag_min = 0.0
if(drag_min.ne.0.0) sqrt_drag_min = sqrt(drag_min)

lambda     = 1.0 + (5.0 - b_stab)*zeta_trans   ! used only if stable_option = 2
rich_trans = zeta_trans/(1.0 + 5.0*zeta_trans) ! used only if stable_option = 2

init = .true.

!    _END_MASTER(myThid)
!    _BARRIER
     ENDIF
     CALL BARRIER(myThid)

return
end subroutine monin_obukhov_init

!=======================================================================

subroutine mo_drag_1d &
         (pt, pt0, z, z0, zt, zq, speed, drag_m, drag_t, drag_q, &
          u_star, b_star, myThid, avail)

real, intent(in)   , dimension(:) :: pt, pt0, z, z0, zt, zq, speed
real, intent(inout), dimension(:) :: drag_m, drag_t, drag_q, u_star, b_star
integer, intent(in)               :: myThid
logical, intent(in), optional, dimension(:) :: avail

real   , dimension(size(pt)) :: rich, fm, ft, fq, zz
logical, dimension(size(pt)) :: mask, mask_1, mask_2

real   , dimension(size(pt)) :: delta_b, us, bs, qs

if(.not.init) call monin_obukhov_init(myThid)


mask = .true.
if(present(avail)) mask = avail

where(mask)
   delta_b = grav*(pt0 - pt)/pt0
   rich    = - z*delta_b/(speed*speed + small)
   zz      = max(z,z0,zt,zq)
else where
   rich = 0.0
end where

if(neutral) then

  where(mask)
    fm   = log(zz/z0)
    ft   = log(zz/zt)
    fq   = log(zz/zq)
    us   = vonkarm/fm
    bs   = vonkarm/ft
    qs   = vonkarm/fq
    drag_m    = us*us
    drag_t    = us*bs
    drag_q    = us*qs
    u_star = us*speed
    b_star = bs*delta_b
  end where

else

  mask_1 = mask .and. rich <  r_crit
  mask_2 = mask .and. rich >= r_crit

  where(mask_2)
    drag_m   = drag_min
    drag_t   = drag_min
    drag_q   = drag_min
    us       = sqrt_drag_min
    bs       = sqrt_drag_min
    qs       = sqrt_drag_min
    u_star   = us*speed
    b_star   = bs*delta_b
  end where
  call solve_zeta (rich, zz, z0, zt, zq, fm, ft, fq, mask_1, myThid)

  where (mask_1)
    us   = max(vonkarm/fm, sqrt_drag_min)
    bs   = max(vonkarm/ft, sqrt_drag_min)
    qs   = max(vonkarm/fq, sqrt_drag_min)
    drag_m   = us*us
    drag_t   = us*bs
    drag_q   = us*qs
    u_star   = us*speed
    b_star   = bs*delta_b
  end where

end if

return
end subroutine mo_drag_1d


!=======================================================================

subroutine mo_profile_1d(zref, zref_t, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_t, del_q, myThid, avail)

real,    intent(in)                :: zref, zref_t
real,    intent(in) , dimension(:) :: z, z0, zt, zq, u_star, b_star, q_star
real,    intent(out), dimension(:) :: del_m, del_t, del_q
integer, intent(in)                :: myThid
logical, intent(in) , optional, dimension(:) :: avail

real, dimension(size(z)) :: zeta, zeta_0, zeta_t, zeta_q, zeta_ref, zeta_ref_t, &
                            ln_z_z0, ln_z_zt, ln_z_zq, ln_z_zref, ln_z_zref_t,  &
                            f_m_ref, f_m, f_t_ref, f_t, f_q_ref, f_q,           &
                            mo_length_inv

logical, dimension(size(z)) :: mask

if(.not.init) call monin_obukhov_init(myThid)

mask = .true.
if(present(avail)) mask = avail

del_m = 0.0  ! zero output arrays
del_t = 0.0
del_q = 0.0

where(mask)
  ln_z_z0     = log(z/z0)
  ln_z_zt     = log(z/zt)
  ln_z_zq     = log(z/zq)
  ln_z_zref   = log(z/zref)
  ln_z_zref_t = log(z/zref_t)
endwhere

if(neutral) then

  where(mask)
    del_m = 1.0 - ln_z_zref  /ln_z_z0
    del_t = 1.0 - ln_z_zref_t/ln_z_zt
    del_q = 1.0 - ln_z_zref_t/ln_z_zq
  endwhere

else

  where(mask .and. u_star > 0.0)
    mo_length_inv = - vonkarm * b_star/(u_star*u_star)
    zeta       = z     *mo_length_inv
    zeta_0     = z0    *mo_length_inv
    zeta_t     = zt    *mo_length_inv
    zeta_q     = zq    *mo_length_inv
    zeta_ref   = zref  *mo_length_inv
    zeta_ref_t = zref_t*mo_length_inv
  endwhere

  call mo_integral_m(f_m,     zeta, zeta_0,   ln_z_z0,   mask, myThid)
  call mo_integral_m(f_m_ref, zeta, zeta_ref, ln_z_zref, mask, myThid)

  call mo_integral_tq(f_t, f_q, zeta, zeta_t, zeta_q, ln_z_zt, ln_z_zq, mask, myThid)
  call mo_integral_tq(f_t_ref, f_q_ref, zeta, zeta_ref_t, zeta_ref_t, &
                      ln_z_zref_t, ln_z_zref_t,  mask, myThid)

  where(mask)
    del_m = 1.0 - f_m_ref/f_m
    del_t = 1.0 - f_t_ref/f_t
    del_q = 1.0 - f_q_ref/f_q
  endwhere

end if

return
end subroutine mo_profile_1d

!=======================================================================

subroutine stable_mix_3d(rich, mix, myThid)

real, intent(in) , dimension(:,:,:)  :: rich
real, intent(out), dimension(:,:,:)  :: mix
integer, intent(in)                  :: myThid

real, dimension(size(rich,1),size(rich,2),size(rich,3)) :: &
      r, a, b, c, zeta, phi

mix = 0.0

if(stable_option == 1) then

  where(rich > 0.0 .and. rich < rich_crit)
    r = 1.0/rich
    a = r - b_stab
    b = r - (1.0 + 5.0)
    c = - 1.0

    zeta = (-b + sqrt(b*b - 4.0*a*c))/(2.0*a)
    phi = 1.0 + b_stab*zeta + (5.0 - b_stab)*zeta/(1.0 + zeta)
    mix = 1./(phi*phi)
  end where

else if(stable_option == 2) then

  where(rich > 0.0 .and. rich <= rich_trans)
    mix = (1.0 - 5.0*rich)**2
  end where
  where(rich > rich_trans .and. rich < rich_crit)
    mix = ((1.0 - b_stab*rich)/lambda)**2
  end where

end if

return
end subroutine stable_mix_3d

!=======================================================================

subroutine mo_diff_2d_n(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in),  dimension(:,:,:) :: z
real, intent(in),  dimension(:,:)   :: u_star, b_star
real, intent(out), dimension(:,:,:) :: k_m, k_h
integer, intent(in)                 :: myThid

real , dimension(size(z,1),size(z,2)) :: phi_m, phi_h, zeta, uss
integer :: j, k
INTEGER :: I
logical, dimension(size(z,1)) :: mask

if(.not.init) call monin_obukhov_init(myThid)

mask = .true.
uss = max(u_star, 1.e-10)

if(neutral) then
  do k = 1, size(z,3)
    k_m(:,:,k) = vonkarm *uss*z(:,:,k)
    k_h(:,:,k) = k_m(:,:,k)
  end do
else
  do k = 1, size(z,3)
    zeta = - vonkarm * b_star*z(:,:,k)/(uss*uss)

    do j = 1, size(z,2)
      call mo_derivative_m(phi_m(:,j), zeta(:,j), mask, myThid)
      call mo_derivative_t(phi_h(:,j), zeta(:,j), mask, myThid)
    enddo
    k_m(:,:,k) = vonkarm * uss*z(:,:,k)/phi_m
    k_h(:,:,k) = vonkarm * uss*z(:,:,k)/phi_h
  end do
endif

return
end subroutine mo_diff_2d_n

!=======================================================================
! The following routines are used by the public interfaces above
!=======================================================================

subroutine solve_zeta(rich, z, z0, zt, zq, f_m, f_t, f_q, mask, myThid)

real   , intent(in) , dimension(:) :: rich, z, z0, zt, zq
logical, intent(in) , dimension(:) :: mask
real   , intent(out), dimension(:) :: f_m, f_t, f_q
integer, intent(in)                :: myThid


real, parameter    :: error    = 1.e-04
real, parameter    :: zeta_min = 1.e-06
integer, parameter :: max_iter = 20

real    :: max_cor
integer :: iter

real, dimension(size(rich)) ::   &
          d_rich, rich_1, correction, corr, z_z0, z_zt, z_zq, &
          ln_z_z0, ln_z_zt, ln_z_zq, zeta,                    &
          phi_m, phi_m_0, phi_t, phi_t_0, rzeta,              &
          zeta_0, zeta_t, zeta_q, df_m, df_t

logical, dimension(size(rich)) :: mask_1


z_z0 = z/z0
z_zt = z/zt
z_zq = z/zq
ln_z_z0 = log(z_z0)
ln_z_zt = log(z_zt)
ln_z_zq = log(z_zq)

corr = 0.0
mask_1 = mask

! initial guess

where(mask_1)
  zeta = rich*ln_z_z0*ln_z_z0/ln_z_zt
else where
  zeta = 0.0
end where

where (mask_1 .and. rich >= 0.0)
  zeta = zeta/(1.0 - rich/rich_crit)
end where

iter_loop: do iter = 1, max_iter

  where (mask_1 .and. abs(zeta).lt.zeta_min)
    zeta = 0.0
    f_m = ln_z_z0
    f_t = ln_z_zt
    f_q = ln_z_zq
    mask_1 = .false.  ! don't do any more calculations at these pts
  end where

  where (mask_1)
    rzeta  = 1.0/zeta
    zeta_0 = zeta/z_z0
    zeta_t = zeta/z_zt
    zeta_q = zeta/z_zq
  else where
    zeta_0 = 0.0
    zeta_t = 0.0
    zeta_q = 0.0
  end where

  call mo_derivative_m(phi_m  , zeta  , mask_1, myThid)
  call mo_derivative_m(phi_m_0, zeta_0, mask_1, myThid)
  call mo_derivative_t(phi_t  , zeta  , mask_1, myThid)
  call mo_derivative_t(phi_t_0, zeta_t, mask_1, myThid)

  call mo_integral_m(f_m, zeta, zeta_0, ln_z_z0, mask_1, myThid)
  call mo_integral_tq(f_t, f_q, zeta, zeta_t, zeta_q, ln_z_zt, ln_z_zq, mask_1, myThid)

  where (mask_1)
    df_m  = (phi_m - phi_m_0)*rzeta
    df_t  = (phi_t - phi_t_0)*rzeta
    rich_1 = zeta*f_t/(f_m*f_m)
    d_rich = rich_1*( rzeta +  df_t/f_t - 2.0 *df_m/f_m)
    correction = (rich - rich_1)/d_rich
    corr = min(abs(correction),abs(correction/zeta))
      ! the criterion corr < error seems to work ok, but is a bit arbitrary
      !  when zeta is small the tolerance is reduced
  end where

  max_cor= maxval(corr)

  if(max_cor > error) then
    mask_1 = mask_1 .and. (corr > error)
       ! change the mask so computation proceeds only on non-converged points
    where(mask_1)
      zeta = zeta + correction
    end where
    cycle iter_loop
  else
    return
  end if

end do iter_loop

!call error_mesg ('solve_zeta in monin_obukhov_mod',  &
!                 'surface drag iteration did not converge', FATAL)

end subroutine solve_zeta

!=======================================================================

subroutine mo_derivative_m(phi_m, zeta, mask, myThid)

! the differential similarity function for momentum

real    , intent(out),  dimension(:) :: phi_m
real    , intent(in),   dimension(:) :: zeta
logical , intent(in),   dimension(:) :: mask
integer, intent(in)                  :: myThid

logical, dimension(size(zeta)) :: stable, unstable
real   , dimension(size(zeta)) :: x

stable   = mask .and. zeta >= 0.0
unstable = mask .and. zeta <  0.0

where (unstable)
  x     = (1 - 16.0*zeta  )**(-0.5)
  phi_m = sqrt(x)  ! phi_m = (1 - 16.0*zeta)**(-0.25)
end where

if(stable_option == 1) then

  where (stable)
    phi_m = 1.0 + zeta  *(5.0 + b_stab*zeta)/(1.0 + zeta)
  end where

else if(stable_option == 2) then

  where (stable .and. zeta < zeta_trans)
    phi_m = 1 + 5.0*zeta
  end where
  where (stable .and. zeta >= zeta_trans)
    phi_m = lambda + b_stab*zeta
  end where

endif

return
end subroutine mo_derivative_m

!=======================================================================

subroutine mo_derivative_t(phi_t, zeta, mask, myThid)

! the differential similarity function for buoyancy and tracers

real    , intent(out),  dimension(:) :: phi_t
real    , intent(in),   dimension(:) :: zeta
logical , intent(in),   dimension(:) :: mask
integer, intent(in)                  :: myThid

logical, dimension(size(zeta)) :: stable, unstable

stable   = mask .and. zeta >= 0.0
unstable = mask .and. zeta <  0.0

where (unstable)
  phi_t = (1 - 16.0*zeta)**(-0.5)
end where

if(stable_option == 1) then

  where (stable)
    phi_t = 1.0 + zeta*(5.0 + b_stab*zeta)/(1.0 + zeta)
  end where

else if(stable_option == 2) then

  where (stable .and. zeta < zeta_trans)
    phi_t = 1 + 5.0*zeta
  end where
  where (stable .and. zeta >= zeta_trans)
    phi_t = lambda + b_stab*zeta
  end where

endif

return
end subroutine mo_derivative_t

!=======================================================================

subroutine mo_integral_tq (psi_t, psi_q, zeta, zeta_t, zeta_q, &
                           ln_z_zt, ln_z_zq, mask, myThid)

! the integral similarity function for moisture and tracers

real    , intent(out), dimension(:) :: psi_t, psi_q
real    , intent(in),  dimension(:) :: zeta, zeta_t, zeta_q, ln_z_zt, ln_z_zq
logical , intent(in),  dimension(:) :: mask
integer, intent(in)                 :: myThid

real, dimension(size(zeta)) :: x, x_t, x_q

logical, dimension(size(zeta)) :: stable, unstable, &
                                  weakly_stable, strongly_stable

stable   = mask .and. zeta >= 0.0
unstable = mask .and. zeta <  0.0

where(unstable)

  x     = sqrt(1 - 16.0*zeta)
  x_t   = sqrt(1 - 16.0*zeta_t)
  x_q   = sqrt(1 - 16.0*zeta_q)

  psi_t = ln_z_zt - 2.0*log( (1.0 + x)/(1.0 + x_t) )
  psi_q = ln_z_zq - 2.0*log( (1.0 + x)/(1.0 + x_q) )

end where

if( stable_option == 1) then

  where (stable)

    psi_t = ln_z_zt + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_t)) &
       + b_stab*(zeta - zeta_t)
    psi_q = ln_z_zq + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_q)) &
       + b_stab*(zeta - zeta_q)

  end where

else if (stable_option == 2) then

  weakly_stable   = stable .and. zeta <= zeta_trans
  strongly_stable = stable .and. zeta >  zeta_trans

  where (weakly_stable)
    psi_t = ln_z_zt + 5.0*(zeta - zeta_t)
    psi_q = ln_z_zq + 5.0*(zeta - zeta_q)
  end where

  where(strongly_stable)
    x = (lambda - 1.0)*log(zeta/zeta_trans) + b_stab*(zeta - zeta_trans)
  endwhere

  where (strongly_stable .and. zeta_t <= zeta_trans)
    psi_t = ln_z_zt + x + 5.0*(zeta_trans - zeta_t)
  end where
  where (strongly_stable .and. zeta_t > zeta_trans)
    psi_t = lambda*ln_z_zt + b_stab*(zeta  - zeta_t)
  endwhere

  where (strongly_stable .and. zeta_q <= zeta_trans)
    psi_q = ln_z_zq + x + 5.0*(zeta_trans - zeta_q)
  end where
  where (strongly_stable .and. zeta_q > zeta_trans)
    psi_q = lambda*ln_z_zq + b_stab*(zeta  - zeta_q)
  endwhere

end if

return
end subroutine mo_integral_tq

!=======================================================================

subroutine mo_integral_m (psi_m, zeta, zeta_0, ln_z_z0, mask, myThid)

!  the integral similarity function for momentum

real    , intent(out), dimension(:) :: psi_m
real    , intent(in),  dimension(:) :: zeta, zeta_0, ln_z_z0
logical , intent(in),  dimension(:) :: mask
integer, intent(in)                 :: myThid

real, dimension(size(zeta)) :: x, x_0, x1, x1_0, num, denom, y

logical, dimension(size(zeta)) :: stable, unstable, &
                                  weakly_stable, strongly_stable

stable   = mask .and. zeta >= 0.0
unstable = mask .and. zeta <  0.0

where(unstable)

  x     = sqrt(1 - 16.0*zeta)
  x_0   = sqrt(1 - 16.0*zeta_0)

  x      = sqrt(x)
  x_0    = sqrt(x_0)

  x1     = 1.0 + x
  x1_0   = 1.0 + x_0

  num    = x1*x1*(1.0 + x*x)
  denom  = x1_0*x1_0*(1.0 + x_0*x_0)
  y      = atan(x) - atan(x_0)
  psi_m  = ln_z_z0 - log(num/denom) + 2*y

end where

if( stable_option == 1) then

  where (stable)
    psi_m = ln_z_z0 + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_0)) &
       + b_stab*(zeta - zeta_0)
  end where

else if (stable_option == 2) then

  weakly_stable   = stable .and. zeta <= zeta_trans
  strongly_stable = stable .and. zeta >  zeta_trans

  where (weakly_stable)
    psi_m = ln_z_z0 + 5.0*(zeta - zeta_0)
  end where

  where(strongly_stable)
    x = (lambda - 1.0)*log(zeta/zeta_trans) + b_stab*(zeta - zeta_trans)
  endwhere

  where (strongly_stable .and. zeta_0 <= zeta_trans)
    psi_m = ln_z_z0 + x + 5.0*(zeta_trans - zeta_0)
  end where
  where (strongly_stable .and. zeta_0 > zeta_trans)
    psi_m = lambda*ln_z_z0 + b_stab*(zeta  - zeta_0)
  endwhere

end if

return
end subroutine mo_integral_m


!=======================================================================
! The following routines allow the public interfaces to be used
! with different dimensions of the input and output
!
!=======================================================================


subroutine mo_drag_2d &
    (pt, pt0, z, z0, zt, zq, speed, drag_m, drag_t, drag_q, u_star, b_star, myThid)

real, intent(in)   , dimension(:,:) :: z, speed, pt, pt0, z0, zt, zq
real, intent(out)  , dimension(:,:) :: drag_m, drag_t, drag_q
real, intent(inout), dimension(:,:) :: u_star, b_star
integer, intent(in)                 :: myThid

integer :: j

do j = 1, size(pt,2)
  call mo_drag_1d (pt(:,j), pt0(:,j), z(:,j), z0(:,j), zt(:,j), zq(:,j), &
                   speed(:,j), drag_m(:,j), drag_t(:,j), drag_q(:,j), &
                   u_star(:,j), b_star(:,j), myThid )
end do


return
end subroutine mo_drag_2d

!=======================================================================
subroutine mo_drag_0d &
    (pt, pt0, z, z0, zt, zq, speed, drag_m, drag_t, drag_q, u_star, b_star, myThid)

real, intent(in)    :: z, speed, pt, pt0, z0, zt, zq
real, intent(out)   :: drag_m, drag_t, drag_q, u_star, b_star

real, dimension(1) :: pt_1, pt0_1, z_1, z0_1, zt_1, zq_1, speed_1, &
                      drag_m_1, drag_t_1, drag_q_1, u_star_1, b_star_1
integer, intent(in):: myThid

pt_1   (1) = pt
pt0_1  (1) = pt0
z_1    (1) = z
z0_1   (1) = z0
zt_1   (1) = zt
zq_1   (1) = zq
speed_1(1) = speed

call mo_drag_1d (pt_1, pt0_1, z_1, z0_1, zt_1, zq_1, speed_1, &
                 drag_m_1, drag_t_1, drag_q_1, u_star_1, b_star_1, myThid )

drag_m = drag_m_1(1)
drag_t = drag_t_1(1)
drag_q = drag_q_1(1)
u_star = u_star_1(1)
b_star = b_star_1(1)

return
end subroutine mo_drag_0d
!=======================================================================

subroutine mo_profile_2d(zref, zref_t, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_h, del_q, myThid)

real, intent(in)                  :: zref, zref_t
real, intent(in) , dimension(:,:) :: z, z0, zt, zq, u_star, b_star, q_star
real, intent(out), dimension(:,:) :: del_m, del_h, del_q
integer, intent(in)               :: myThid

integer :: j

do j = 1, size(z,2)
  call mo_profile_1d (zref, zref_t, z(:,j), z0(:,j), zt(:,j),         &
                      zq(:,j), u_star(:,j), b_star(:,j), q_star(:,j), &
                      del_m(:,j), del_h (:,j), del_q (:,j), myThid )
enddo

return
end subroutine mo_profile_2d

!=======================================================================

subroutine mo_profile_0d(zref, zref_t, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_h, del_q, myThid)

real, intent(in)  :: zref, zref_t
real, intent(in)  :: z, z0, zt, zq, u_star, b_star, q_star
real, intent(out) :: del_m, del_h, del_q

real, dimension(1) :: z_1, z0_1, zt_1, zq_1, u_star_1, b_star_1, q_star_1, &
                      del_m_1, del_h_1, del_q_1
integer, intent(in):: myThid

z_1     (1) = z
z0_1    (1) = z0
zt_1    (1) = zt
zq_1    (1) = zq
u_star_1(1) = u_star
b_star_1(1) = b_star
q_star_1(1) = q_star

call mo_profile_1d (zref, zref_t, z_1, z0_1, zt_1, zq_1, &
                    u_star_1, b_star_1, q_star_1,        &
                    del_m_1, del_h_1, del_q_1, myThid )

del_m = del_m_1(1)
del_h = del_h_1(1)
del_q = del_q_1(1)


return
end subroutine mo_profile_0d

!=======================================================================

subroutine mo_profile_1d_n(zref, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_t, del_q, myThid, avail)

real,    intent(in),  dimension(:)   :: zref
real,    intent(in) , dimension(:)   :: z, z0, zt, zq, u_star, b_star, q_star
real,    intent(out), dimension(:,:) :: del_m, del_t, del_q
integer, intent(in)                  :: myThid
logical, intent(in) , optional, dimension(:) :: avail

integer :: k

do k = 1, size(zref)
  if(present(avail)) then
    call mo_profile_1d (zref(k), zref(k), z, z0, zt, zq, &
       u_star, b_star, q_star, del_m(:,k), del_t(:,k), del_q(:,k), myThid, avail)
  else
      call mo_profile_1d (zref(k), zref(k), z, z0, zt, zq, &
       u_star, b_star, q_star, del_m(:,k), del_t(:,k), del_q(:,k), myThid )
  endif
enddo

return
end subroutine mo_profile_1d_n

!=======================================================================

subroutine mo_profile_0d_n(zref, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_t, del_q, myThid)

real,    intent(in),  dimension(:) :: zref
real,    intent(in)                :: z, z0, zt, zq, u_star, b_star, q_star
real,    intent(out), dimension(:) :: del_m, del_t, del_q
integer, intent(in)                :: myThid

integer :: k

do k = 1, size(zref)
  call mo_profile_0d (zref(k), zref(k), z, z0, zt, zq, &
       u_star, b_star, q_star, del_m(k), del_t(k), del_q(k), myThid)
enddo

return
end subroutine mo_profile_0d_n

!=======================================================================

subroutine mo_profile_2d_n(zref, z, z0, zt, zq, u_star, b_star, q_star, &
                         del_m, del_t, del_q, myThid)

real,    intent(in),  dimension(:)     :: zref
real,    intent(in),  dimension(:,:)   :: z, z0, zt, zq, u_star, b_star, q_star
real,    intent(out), dimension(:,:,:) :: del_m, del_t, del_q
integer, intent(in)                    :: myThid

integer :: k

do k = 1, size(zref)
  call mo_profile_2d (zref(k), zref(k), z, z0, zt, zq, &
       u_star, b_star, q_star, del_m(:,:,k), del_t(:,:,k), del_q(:,:,k), myThid)
enddo

return
end subroutine mo_profile_2d_n

!=======================================================================

subroutine mo_diff_2d_1(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in),  dimension(:,:) :: z, u_star, b_star
real, intent(out), dimension(:,:) :: k_m, k_h

real   , dimension(size(z,1),size(z,2),1) :: z_n, k_m_n, k_h_n
integer, intent(in)                       :: myThid

z_n(:,:,1) = z

call mo_diff_2d_n(z_n, u_star, b_star, k_m_n, k_h_n, myThid)

k_m = k_m_n(:,:,1)
k_h = k_h_n(:,:,1)

return
end subroutine mo_diff_2d_1


!=======================================================================

subroutine mo_diff_1d_1(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in),  dimension(:) :: z, u_star, b_star
real, intent(out), dimension(:) :: k_m, k_h

real, dimension(size(z),1,1) :: z_n, k_m_n, k_h_n
real, dimension(size(z),1)   :: u_star_n, b_star_n
integer, intent(in)          :: myThid

z_n   (:,1,1) = z
u_star_n(:,1) = u_star
b_star_n(:,1) = b_star

call mo_diff_2d_n(z_n, u_star_n, b_star_n, k_m_n, k_h_n, myThid)

k_m = k_m_n(:,1,1)
k_h = k_h_n(:,1,1)

return
end subroutine mo_diff_1d_1

!=======================================================================

subroutine mo_diff_1d_n(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in),  dimension(:,:) :: z
real, intent(in),  dimension(:)   :: u_star, b_star
real, intent(out), dimension(:,:) :: k_m, k_h
integer, intent(in)               :: myThid

real, dimension(size(z,1),1)            :: u_star2, b_star2
real, dimension(size(z,1),size(z,2), 1) :: z2, k_m2, k_h2


z2   (:,:,1) = z
u_star2(:,1) = u_star
b_star2(:,1) = b_star

call mo_diff_2d_n(z2, u_star2, b_star2, k_m2, k_h2, myThid)

k_m = k_m2(:,:,1)
k_h = k_h2(:,:,1)

return
end subroutine mo_diff_1d_n

!=======================================================================

subroutine mo_diff_0d_1(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in)   :: z, u_star, b_star
real, intent(out)  :: k_m, k_h
integer, intent(in):: myThid

real, dimension(1,1,1) :: z_n, k_m_n, k_h_n
real, dimension(1,1)   :: u_star_n, b_star_n

z_n   (1,1,1) = z
u_star_n(1,1) = u_star
b_star_n(1,1) = b_star

call mo_diff_2d_n(z_n, u_star_n, b_star_n, k_m_n, k_h_n, myThid)

k_m = k_m_n(1,1,1)
k_h = k_h_n(1,1,1)

return
end subroutine mo_diff_0d_1

!=======================================================================

subroutine mo_diff_0d_n(z, u_star, b_star, k_m, k_h, myThid)

real, intent(in),  dimension(:) :: z
real, intent(in)                :: u_star, b_star
real, intent(out), dimension(:) :: k_m, k_h
integer, intent(in)             :: myThid

real, dimension(1,1)            :: u_star2, b_star2
real, dimension(size(z,1),1, 1) :: z2, k_m2, k_h2

z2   (:,1,1) = z
u_star2(1,1) = u_star
b_star2(1,1) = b_star

call mo_diff_2d_n(z2, u_star2, b_star2, k_m2, k_h2, myThid)

k_m = k_m2(:,1,1)
k_h = k_h2(:,1,1)

return
end subroutine mo_diff_0d_n

!=======================================================================

subroutine stable_mix_2d(rich, mix, myThid)

real, intent(in) , dimension(:,:)  :: rich
real, intent(out), dimension(:,:)  :: mix
integer, intent(in)                :: myThid

real, dimension(size(rich,1),size(rich,2),1) :: rich_3d, mix_3d

rich_3d(:,:,1) = rich

call stable_mix_3d(rich_3d, mix_3d, myThid)

mix = mix_3d(:,:,1)

return
end subroutine stable_mix_2d


!=======================================================================

subroutine stable_mix_1d(rich, mix, myThid)

real, intent(in) , dimension(:)  :: rich
real, intent(out), dimension(:)  :: mix
integer, intent(in)              :: myThid

real, dimension(size(rich),1,1) :: rich_3d, mix_3d

rich_3d(:,1,1) = rich

call stable_mix_3d(rich_3d, mix_3d, myThid)

mix = mix_3d(:,1,1)

return
end subroutine stable_mix_1d

!=======================================================================

subroutine stable_mix_0d(rich, mix, myThid)

real, intent(in)    :: rich
real, intent(out)   :: mix
integer, intent(in) :: myThid

real, dimension(1,1,1) :: rich_3d, mix_3d

rich_3d(1,1,1) = rich

call stable_mix_3d(rich_3d, mix_3d, myThid)

mix = mix_3d(1,1,1)

return
end subroutine stable_mix_0d
!=======================================================================

end module monin_obukhov_mod

