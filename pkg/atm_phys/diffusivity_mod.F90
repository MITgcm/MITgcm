! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/diffusivity_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

module diffusivity_mod

!=======================================================================
!
!                          DIFFUSIVITY MODULE
!
!     Routines for computing atmospheric diffusivities in the
!       planetary boundary layer and in the free atmosphere
!
!=======================================================================


use gcm_params_mod, only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit
use constants_mod, only : grav, vonkarm, cp_air, rdgas, rvgas

!use       fms_mod, only:  error_mesg, FATAL, file_exist,   &
!                          check_nml_error, open_namelist_file,      &
!                          mpp_pe, mpp_root_pe, close_file, stdlog,  &
!                          write_version_number

use monin_obukhov_mod, only : mo_diff

implicit none
private

! public interfaces
!=======================================================================

 public diffusivity, pbl_depth, molecular_diff

!=======================================================================

! form of iterfaces

!=======================================================================
! subroutine diffusivity (t, q, u, v, p_full, p_half, z_full, z_half,
!                         u_star, b_star, h, k_m, k_t)

! input:

!        t     : real, dimension(:,:,:) -- (:,:,pressure), third index running
!                          from top of atmosphere to bottom
!                 temperature (K)
!
!        q     : real, dimension(:,:,:)
!                 water vapor specific humidity (nondimensional)
!
!        u     : real, dimension(:,:)
!                 zonal wind (m/s)
!
!        v     : real, dimension(:,:,:)
!                 meridional wind (m/s)
!
!        z_full  : real, dimension(:,:,:
!                 height of full levels (m)
!                 1 = top of atmosphere; size(p_half,3) = surface
!                 size(z_full,3) = size(t,3)
!
!        z_half  : real, dimension(:,:,:)
!                 height of  half levels (m)
!                 size(z_half,3) = size(t,3) +1
!              z_half(:,:,size(z_half,3)) must be height of surface!
!                                  (if you are not using eta-model)
!
!        u_star: real, dimension(:,:)
!                friction velocity (m/s)
!
!        b_star: real, dimension(:,:)
!                buoyancy scale (m/s**2)

!   (u_star and b_star can be obtained by calling
!     mo_drag in monin_obukhov_mod)

! output:

!        h     : real, dimension(:,:,)
!                 depth of planetary boundary layer (m)
!
!        k_m   : real, dimension(:,:,:)
!                diffusivity for momentum (m**2/s)
!
!                defined at half-levels
!                size(k_m,3) should be at least as large as size(t,3)
!                only the returned values at
!                      levels 2 to size(t,3) are meaningful
!                other values will be returned as zero
!
!        k_t   : real, dimension(:,:,:)
!                diffusivity for temperature and scalars (m**2/s)
!
!
!=======================================================================


!--------------------- version number ----------------------------------

character(len=128) :: version = '$Id: diffusivity_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
character(len=128) :: tag = '$Name:  $'

!=======================================================================

!  DEFAULT VALUES OF NAMELIST PARAMETERS:

logical :: fixed_depth         = .false.
logical :: do_virtual_non_mcm  = .false.  ! applies only to non-'mcm' pbl scheme
real    :: depth_0             =  5000.0
real    :: frac_inner          =  0.1
real    :: rich_crit_pbl       =  1.0
real    :: entr_ratio          =  0.2
real    :: parcel_buoy         =  2.0
real    :: znom                =  1000.0
logical :: free_atm_diff       = .false.
logical :: free_atm_skyhi_diff = .false.
logical :: pbl_mcm             = .false.
real    :: rich_crit_diff      =  0.25
real    :: mix_len             = 30.
real    :: rich_prandtl        =  1.00
real    :: background_m        =  0.0
real    :: background_t        =  0.0
logical :: ampns               = .false. ! include delta z factor in
                                         ! defining ri ?
real    :: ampns_max           = 1.0E20  ! limit to reduction factor
                                         ! applied to ri due to delta z
                                         ! factor

namelist /diffusivity_nml/ fixed_depth, depth_0, frac_inner,&
                           rich_crit_pbl, entr_ratio, parcel_buoy,&
                           znom, free_atm_diff, free_atm_skyhi_diff,&
                           pbl_mcm, rich_crit_diff, mix_len, rich_prandtl,&
                           background_m, background_t, ampns, ampns_max, &
                           do_virtual_non_mcm

!=======================================================================

!  OTHER MODULE VARIABLES

real    :: small  = 1.e-04
real    :: gcp    = grav/cp_air
logical :: init   = .false.
real    :: beta   = 1.458e-06
real    :: rbop1  = 110.4
real    :: rbop2  = 1.405

real, parameter :: d608 = (rvgas-rdgas)/rdgas


contains

!=======================================================================

subroutine diffusivity_init(myThid)

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

     WRITE(msgBuf,'(A)') 'DIFFUSIVITY_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'DIFFUSIVITY_INIT',       &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=diffusivity_nml)
     WRITE(msgBuf,'(A)')                                            &
          'DIFFUSIVITY_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
     CLOSE(iUnit)

!     if (file_exist('input.nml')) then
!        unit = open_namelist_file ()
!        ierr=1; do while (ierr /= 0)
!           read  (unit, nml=diffusivity_nml, iostat=io, end=10)
!           ierr = check_nml_error(io,'diffusivity_nml')
!        enddo
! 10     call close_file (unit)

!------------------- dummy checks --------------------------------------
         if (frac_inner .le. 0. .or. frac_inner .ge. 1.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'frac_inner must be between 0 and 1', myThid)
!           call error_mesg ('diffusivity_init',  &
!           'frac_inner must be between 0 and 1', FATAL)
         if (rich_crit_pbl .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
           'rich_crit_pbl must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!          'rich_crit_pbl must be greater than or equal to zero', FATAL)
         if (entr_ratio .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'entr_ratio must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'entr_ratio must be greater than or equal to zero', FATAL)
         if (znom .le. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'znom must be greater than zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'znom must be greater than zero', FATAL)
         if (.not.free_atm_diff .and. free_atm_skyhi_diff)&
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'free_atm_diff must be set to true if '//&
            'free_atm_skyhi_diff = .true.', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'free_atm_diff must be set to true if '//&
!           'free_atm_skyhi_diff = .true.', FATAL)
         if (rich_crit_diff .le. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'rich_crit_diff must be greater than zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'rich_crit_diff must be greater than zero', FATAL)
         if (mix_len .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'mix_len must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'mix_len must be greater than or equal to zero', FATAL)
         if (rich_prandtl .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'rich_prandtl must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'rich_prandtl must be greater than or equal to zero', FATAL)
         if (background_m .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'background_m must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'background_m must be greater than or equal to zero', FATAL)
         if (background_t .lt. 0.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'background_t must be greater than or equal to zero', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'background_t must be greater than or equal to zero', FATAL)
         if (ampns_max .lt. 1.) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'ampns_max must be greater than or equal to one', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'ampns_max must be greater than or equal to one', FATAL)
         if (ampns .and. .not. free_atm_skyhi_diff) &
            CALL PRINT_ERROR( 'diffusivity_init'//  &
            'ampns is only valid when free_atm_skyhi_diff is & also true', myThid )
!           call error_mesg ('diffusivity_init',  &
!           'ampns is only valid when free_atm_skyhi_diff is &
!                  & also true', FATAL)

!     endif  !end of reading input.nml

!---------- output namelist to log-------------------------------------

!     call write_version_number(version, tag)
!     if ( mpp_pe() == mpp_root_pe() ) write (stdlog(), nml=diffusivity_nml)

      init = .true.

     ENDIF
     CALL BARRIER(myThid)

return
end subroutine diffusivity_init

!=======================================================================

subroutine diffusivity(t, q, u, v, p_full, p_half, z_full, z_half,  &
                       u_star, b_star, h, k_m, k_t, myThid, kbot)

real,    intent(in),           dimension(:,:,:) :: t, q, u, v
real,    intent(in),           dimension(:,:,:) :: p_full, p_half
real,    intent(in),           dimension(:,:,:) :: z_full, z_half
real,    intent(in),           dimension(:,:)   :: u_star, b_star
real,    intent(inout),        dimension(:,:,:) :: k_m, k_t
real,    intent(out),          dimension(:,:)   :: h
integer, intent (in)                            :: myThid
integer, intent(in), optional, dimension(:,:)   :: kbot

real, dimension(size(t,1),size(t,2),size(t,3))  :: svcp,z_full_ag, &
                                                   k_m_save, k_t_save
real, dimension(size(t,1),size(t,2),size(t,3)+1):: z_half_ag
real, dimension(size(t,1),size(t,2))            :: z_surf
integer                                         :: i,j,k,nlev,nlat,nlon

if(.not.init) call diffusivity_init(myThid)

nlev = size(t,3)

k_m_save = k_m
k_t_save = k_t

!compute height of surface
if (present(kbot)) then
   nlat = size(t,2)
   nlon = size(t,1)
   do j=1,nlat
   do i=1,nlon
          z_surf(i,j) = z_half(i,j,kbot(i,j)+1)
   enddo
   enddo
else
   z_surf(:,:) = z_half(:,:,nlev+1)
end if


!compute density profile, and heights relative to surface
do k = 1, nlev

  z_full_ag(:,:,k) = z_full(:,:,k) - z_surf(:,:)
  z_half_ag(:,:,k) = z_half(:,:,k) - z_surf(:,:)

  if (do_virtual_non_mcm) then
   svcp(:,:,k)  =   t(:,:,k)*(1. + d608*q(:,:,k)) + gcp*(z_full_ag(:,:,k))
  else
   svcp(:,:,k)  =   t(:,:,k) + gcp*(z_full_ag(:,:,k))
  endif

end do
z_half_ag(:,:,nlev+1) = z_half(:,:,nlev+1) - z_surf(:,:)


if(fixed_depth)  then
   h = depth_0
else
   call pbl_depth(svcp,u,v,z_full_ag,u_star,b_star,h, myThid, kbot=kbot)
end if

if(pbl_mcm) then
   call diffusivity_pbl_mcm (u,v, t, p_full, p_half, &
                             z_full_ag, z_half_ag, h, k_m, k_t, myThid)
else
   call diffusivity_pbl  (svcp, u, v, z_half_ag, h, u_star, b_star,&
                       k_m, k_t, myThid, kbot=kbot)
end if
if(free_atm_diff) &
   call diffusivity_free (svcp, u, v, z_full_ag, z_half_ag, h, k_m, k_t, myThid)

k_m = k_m + k_m_save
k_t = k_t + k_t_save

!NOTE THAT THIS LINE MUST FOLLOW DIFFUSIVITY_FREE SO THAT ENTRAINMENT
!K's DO NOT GET OVERWRITTEN IN DIFFUSIVITY_FREE SUBROUTINE
if(entr_ratio .gt. 0. .and. .not. fixed_depth) &
    call diffusivity_entr(svcp,z_full_ag,h,u_star,b_star,k_m,k_t, myThid)

!set background diffusivities
if(background_m.gt.0.0) k_m = max(k_m,background_m)
if(background_t.gt.0.0) k_t = max(k_t,background_t)


return
end subroutine diffusivity

!=======================================================================

subroutine pbl_depth(t, u, v, z, u_star, b_star, h, myThid, kbot)


real,   intent(in) ,           dimension(:,:,:) :: t, u, v, z
real,   intent(in) ,           dimension(:,:)   :: u_star,b_star
real,   intent(out),           dimension(:,:)   :: h
integer, intent(in)                             :: myThid
integer,intent(in) , optional, dimension(:,:)   :: kbot

real,    dimension(size(t,1),size(t,2),size(t,3))  :: rich
real,    dimension(size(t,1),size(t,2))            :: ws,k_t_ref,&
                                                      h_inner,tbot
real                                               :: rich1, rich2,&
                                                      h1,h2,svp,t1,t2
integer, dimension(size(t,1),size(t,2))            :: ibot
integer                                            :: i,j,k,nlon,&
                                                      nlat, nlev

nlev = size(t,3)
nlat = size(t,2)
nlon = size(t,1)

!assign ibot, compute tbot (virtual temperature at lowest level)
if (present(kbot)) then
    ibot(:,:) = kbot
    do j = 1,nlat
    do i = 1,nlon
          tbot(i,j) = t(i,j,ibot(i,j))
    enddo
    enddo
else
    ibot(:,:) = nlev
    tbot(:,:) = t(:,:,nlev)
end if


!compute richardson number for use in pbl depth of neutral/stable side
do k = 1,nlev
  rich(:,:,k) =  z(:,:,k)*grav*(t(:,:,k)-tbot(:,:))/tbot(:,:)&
                /(u(:,:,k)*u(:,:,k) + v(:,:,k)*v(:,:,k) + small )
end do

!compute ws to be used in evaluating parcel buoyancy
!ws = u_star / phi(h_inner,u_star,b_star)  .  To find phi
!a call to mo_diff is made.

h_inner(:,:)=frac_inner*znom
call mo_diff(h_inner, u_star, b_star, ws, k_t_ref, myThid )
ws = max(small,ws/vonkarm/h_inner)


do j = 1, nlat
 do i = 1, nlon

        !do neutral or stable case
        if (b_star(i,j).le.0.) then

              h1     = z(i,j,ibot(i,j))
              h(i,j) = h1
              rich1  = rich(i,j,ibot(i,j))
              do k = ibot(i,j)-1, 1, -1
                       rich2 = rich(i,j,k)
                       h2    = z(i,j,k)
                       if(rich2.gt.rich_crit_pbl) then
                             h(i,j) = h2 + (h1 - h2)*(rich2 - rich_crit_pbl)&
                                                    /(rich2 - rich1        )
                             go to 10
                       endif
                       rich1 = rich2
                       h1    = h2
              enddo

        !do unstable case
        else

              svp    = tbot(i,j)*(1.+ &
                       (parcel_buoy*u_star(i,j)*b_star(i,j)/grav/ws(i,j)) )
              h1     = z(i,j,ibot(i,j))
              h(i,j) = h1
              t1     = tbot(i,j)
              do k = ibot(i,j)-1 , 1, -1
                       h2 = z(i,j,k)
                       t2 = t(i,j,k)
                       if (t2.gt.svp) then
                             h(i,j) = h2 + (h1 - h2)*(t2 - svp)/(t2 - t1 )
                             go to 10
                       end if
                       h1 = h2
                       t1 = t2
              enddo

        end if
10 continue
  enddo
enddo

return
end subroutine pbl_depth

!=======================================================================

subroutine diffusivity_pbl(t, u, v, z_half, h, u_star, b_star, &
                           k_m, k_t, myThid, kbot)

real,    intent(in)  ,           dimension(:,:,:) :: t, u, v, z_half
real,    intent(in)  ,           dimension(:,:)   :: h, u_star, b_star
real,    intent(inout) ,           dimension(:,:,:) :: k_m, k_t
integer, intent (in)                                :: myThid
integer, intent(in)  , optional, dimension(:,:)   :: kbot

real, dimension(size(t,1),size(t,2))              :: h_inner, k_m_ref,&
                                                     k_t_ref, factor
real, dimension(size(t,1),size(t,2),size(t,3)+1)  :: zm
real                                              :: h_inner_max
integer                                           :: i,j, k, kk, nlev


nlev = size(t,3)

!assign z_half to zm, and set to zero any values of zm < 0.
!the setting to zero is necessary so that when using eta model
!below ground half levels will have zero k_m and k_t
zm = z_half
if (present(kbot)) then
   where(zm < 0.)
        zm = 0.
   end where
end if

h_inner    = frac_inner*h
h_inner_max = maxval(h_inner)

kk = nlev
do k = 2, nlev
  if( minval(zm(:,:,k)) < h_inner_max) then
      kk = k
      exit
  end if
end do

k_m = 0.0
k_t = 0.0

call mo_diff(h_inner        , u_star, b_star, k_m_ref         , k_t_ref, myThid )
call mo_diff(zm(:,:,kk:nlev), u_star, b_star, k_m(:,:,kk:nlev), k_t(:,:,kk:nlev), &
              myThid )

do k = 2, nlev
  where(zm(:,:,k) >= h_inner .and. zm(:,:,k) < h)
    factor = (zm(:,:,k)/h_inner)* &
             (1.0 - (zm(:,:,k) - h_inner)/(h - h_inner))**2
    k_m(:,:,k) = k_m_ref*factor
    k_t(:,:,k) = k_t_ref*factor
  end where
end do

return
end subroutine diffusivity_pbl

!=======================================================================

subroutine diffusivity_pbl_mcm(u, v, t, p_full, p_half, z_full, z_half, &
                               h, k_m, k_t, myThid)

real, intent(in)  , dimension(:,:,:) :: u, v, t, z_full, z_half
real, intent(in)  , dimension(:,:,:) :: p_full, p_half
real, intent(in)  , dimension(:,:)   :: h
real, intent(inout) , dimension(:,:,:) :: k_m, k_t
integer, intent(in)                    :: myThid

integer                                        :: k, nlev
real, dimension(size(z_full,1),size(z_full,2)) :: elmix, htcrit
real, dimension(size(z_full,1),size(z_full,2)) :: delta_u, delta_v, delta_z

real :: htcrit_ss
real :: h_ss
real, dimension(size(z_full,1),size(z_full,2)) :: sig_half, z_half_ss, elmix_ss

!  htcrit_ss = height at which mixing length is a maximum (75m)


!  h_ss   = height at which mixing length vanishes (4900m)
!  elmix_ss   = mixing length

! Define some constants:
!  salaps = standard atmospheric lapse rate (K/m)
!  tsfc   = idealized global mean surface temperature (15C)
real :: tsfc = 288.16
real :: salaps = -6.5e-3

nlev = size(z_full,3)

k_m = 0.

h_ss = depth_0
htcrit_ss = frac_inner*h_ss

do k = 2, nlev

! TK mods 8/13/01:  (code derived from SS)
! Compute the height of each half level assuming a constant
! standard lapse rate using the above procedure.
! WARNING: These should be used with caution.  They will
!  have large errors above the tropopause.

! In order to determine the height, the layer mean temperature
! from the surface to that level is required.  A surface
! temperature of 15 deg Celsius and a standard lapse rate of
! -6.5 deg/km will be used to estimate an average temperature
! profile.

   sig_half = p_half(:,:,k)/p_half(:,:,nlev+1)
   z_half_ss = -rdgas * .5*(tsfc+tsfc*(sig_half**(-rdgas*salaps/grav))) * alog(sig_half)/grav

   !compute mixing length as in SS (no geographical variation)
    elmix_ss = 0.

    where (z_half_ss < htcrit_ss .and. z_half_ss > 0.)
         elmix_ss = vonkarm*z_half_ss
    endwhere
    where (z_half_ss >= htcrit_ss .and. z_half_ss < h_ss)
         elmix_ss = vonkarm*htcrit_ss*(h_ss-z_half_ss)/(h_ss-htcrit_ss)
    endwhere

   delta_z = rdgas*0.5*(t(:,:,k)+t(:,:,k-1))*(p_full(:,:,k)-p_full(:,:,k-1))/&
             (grav*p_half(:,:,k))
   delta_u =      u(:,:,k-1) -      u(:,:,k)
   delta_v =      v(:,:,k-1) -      v(:,:,k)

   k_m(:,:,k) =   elmix_ss * elmix_ss *&
                  sqrt(delta_u*delta_u + delta_v*delta_v)/delta_z

end do

k_t = k_m

return
end subroutine diffusivity_pbl_mcm

!=======================================================================

subroutine diffusivity_free(t, u, v, z, zz, h, k_m, k_t, myThid)

real, intent(in)    , dimension(:,:,:) :: t, u, v, z, zz
real, intent(in)    , dimension(:,:)   :: h
real, intent(inout) , dimension(:,:,:) :: k_m, k_t
integer, intent(in)                    :: myThid

real, dimension(size(t,1),size(t,2))   :: dz, b, speed2, rich, fri, &
                                          alpz, fri2
integer                                :: k

do k = 2, size(t,3)

!----------------------------------------------------------------------
!  define the richardson number. set it to zero if it is negative. save
!  a copy of it for later use (rich2).
!----------------------------------------------------------------------
  dz     = z(:,:,k-1) - z(:,:,k)
  b      = grav*(t(:,:,k-1)-t(:,:,k))/t(:,:,k)
  speed2 = (u(:,:,k-1) - u(:,:,k))**2 + (v(:,:,k-1) - v(:,:,k))**2
  rich= b*dz/(speed2+small)
  rich = max(rich, 0.0)

  if (free_atm_skyhi_diff) then
!---------------------------------------------------------------------
!   limit the standard richardson number to between 0 and the critical
!   value (rich2). compute the richardson number factor needed in the
!   eddy mixing coefficient using this standard richardson number.
!---------------------------------------------------------------------
    where (rich(:,:) >= rich_crit_diff)
      fri2(:,:) = 0.0
    elsewhere
      fri2(:,:)  = (1.0 - rich/rich_crit_diff)**2
    endwhere
  endif

!---------------------------------------------------------------------
!  if ampns is activated, compute the delta z factor. define rich
!  including this factor.
!---------------------------------------------------------------------
  if (ampns) then
    alpz(:,:) = MIN ( (1.  + 1.e-04*(dz(:,:)**1.5)), ampns_max)
    rich(:,:) = rich(:,:) / alpz(:,:)
  endif

!---------------------------------------------------------------------
!   compute the richardson number factor to be used in the eddy
!   mixing coefficient. if ampns is on, this value includes it; other-
!   wise it does not.
!---------------------------------------------------------------------
  fri(:,:)   = (1.0 - rich/rich_crit_diff)**2

!---------------------------------------------------------------------
!   compute the eddy mixing coefficients in the free atmosphere ( zz
!   > h). in the non-ampns case, values are obtained only when the
!   standard richardson number is sub-critical; in the ampns case values
!   are obtained only when the richardson number computed with the
!   ampns factor is sub critical. when the ampns factor is activated,
!   it is also included in the mixing coefficient. the value of mixing
!   for temperature, etc. is reduced dependent on the ri stability
!   factor calculated without the ampns factor.
!---------------------------------------------------------------------
  if (free_atm_skyhi_diff) then

!---------------------------------------------------------------------
!   this is the skyhi-like formulation -- possible ampns factor, ratio
!   of k_m to k_t defined based on computed stability factor.
!---------------------------------------------------------------------
    if (ampns) then
      where (rich < rich_crit_diff .and. zz(:,:,k) > h)
           k_m(:,:,k) = mix_len*mix_len*sqrt(speed2)*fri(:,:)* &
                        ( 1.  + 1.e-04*(dz(:,:)**1.5))/dz
           k_t(:,:,k) = k_m(:,:,k)* (0.1 + 0.9*fri2(:,:))
      end where
    else
      where (rich < rich_crit_diff .and. zz(:,:,k) > h)
        k_m(:,:,k) = mix_len*mix_len*sqrt(speed2)*fri(:,:)/dz
        k_t(:,:,k) = k_m(:,:,k)* (0.1 + 0.9*fri2(:,:))
      end where
    endif
  else

!---------------------------------------------------------------------
!   this is the non-skyhi-like formulation -- no ampns factor, ratio
!   of k_m to k_t defined by rich_prandtl.
!---------------------------------------------------------------------
    where (rich < rich_crit_diff .and. zz(:,:,k) > h)
         k_t(:,:,k) = mix_len*mix_len*sqrt(speed2)*fri(:,:)/dz
         k_m(:,:,k) = k_t(:,:,k)*rich_prandtl
    end where
  end if
end do


end subroutine diffusivity_free

!=======================================================================

subroutine molecular_diff ( temp, press, k_m, k_t, myThid)

real, intent(in),    dimension (:,:,:)  ::  temp, press
real, intent(inout), dimension (:,:,:)  ::  k_m, k_t
integer, intent(in)                     :: myThid

      real, dimension (size(temp,1), size(temp,2)) :: temp_half, &
                                                      rho_half, rbop2d
      integer      :: k

!---------------------------------------------------------------------

      do k=2,size(temp,3)
        temp_half(:,:) = 0.5*(temp(:,:,k) + temp(:,:,k-1))
        rho_half(:,:) = press(:,:,k)/(rdgas*temp_half(:,:) )
        rbop2d(:,:)  = beta*temp_half(:,:)*sqrt(temp_half(:,:))/  &
                       (rho_half(:,:)*(temp_half(:,:)+rbop1))
        k_m(:,:,k) = rbop2d(:,:)
        k_t(:,:,k) = rbop2d(:,:)*rbop2
      end do

      k_m(:,:,1) = 0.0
      k_t(:,:,1) = 0.0



end subroutine molecular_diff



!=======================================================================

subroutine diffusivity_entr(t, z,  h, u_star, b_star, k_m, k_t, myThid)

real, intent(in)    , dimension(:,:,:) :: t, z
real, intent(in)    , dimension(:,:)   :: h, u_star, b_star
real, intent(inout) , dimension(:,:,:) :: k_m, k_t
integer, intent(in)                    :: myThid

integer                                :: k, nlev

nlev=size(t,3)

do k = 2,nlev
    where (b_star .gt. 0. .and. z(:,:,k-1) .gt. h .and. &
                                z(:,:,k)   .le. h)
        k_t(:,:,k) = (z(:,:,k-1)-z(:,:,k))*entr_ratio*t(:,:,k)* &
                      u_star*b_star/grav/max(small,t(:,:,k-1)-t(:,:,k))
        k_m(:,:,k) = k_t(:,:,k)
    end where
enddo
end subroutine diffusivity_entr

!=======================================================================

end module diffusivity_mod

