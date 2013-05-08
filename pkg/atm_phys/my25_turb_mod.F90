! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/my25_turb_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

  MODULE MY25_TURB_MOD

!=======================================================================
!   MELLOR-YAMADA LEVEL 2.5 TURBULENCE CLOSURE SCHEME - GFDL VERSION   !
!=======================================================================

! use Fms_Mod,         ONLY: FILE_EXIST, OPEN_NAMELIST_FILE, ERROR_MESG, FATAL,&
!                            read_data, write_data, CLOSE_FILE,&
!                            check_nml_error,mpp_pe, mpp_root_pe, &
!                            write_version_number, stdlog, open_file
! use Tridiagonal_Mod, ONLY: TRI_INVERT, CLOSE_TRIDIAGONAL
 use constants_mod,   only: grav, vonkarm
 use monin_obukhov_mod, only : mo_diff

!---------------------------------------------------------------------
 implicit none
 private
!---------------------------------------------------------------------

 public :: MY25_TURB, MY25_TURB_INIT, MY25_TURB_END, TKE_SURF

!---------------------------------------------------------------------
! --- GLOBAL STORAGE
!---------------------------------------------------------------------

  real, public, allocatable, dimension(:,:,:) :: TKE

!---------------------------------------------------------------------

 character(len=128) :: version = '$Id: my25_turb_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
 character(len=128) :: tag = '$Name:  $'

 logical :: do_init = .true.
 logical :: init_tke
 integer :: num_total_pts, pts_done

!---------------------------------------------------------------------
! --- CONSTANTS
!---------------------------------------------------------------------

 real :: aa1,   aa2,   bb1,  bb2,  ccc
 real :: ckm1,  ckm2,  ckm3, ckm4, ckm5, ckm6, ckm7, ckm8
 real :: ckh1,  ckh2,  ckh3, ckh4
 real :: cvfq1, cvfq2, bcq

 real, parameter :: aa1_old =  0.78
 real, parameter :: aa2_old =  0.79
 real, parameter :: bb1_old = 15.0
 real, parameter :: bb2_old =  8.0
 real, parameter :: ccc_old =  0.056
 real, parameter :: aa1_new =  0.92
 real, parameter :: aa2_new =  0.74
 real, parameter :: bb1_new = 16.0
 real, parameter :: bb2_new = 10.0
 real, parameter :: ccc_new =  0.08
 real, parameter :: cc1     =  0.27
 real, parameter :: t00     =  2.7248e2
 real, parameter :: small   =  1.0e-10

!---------------------------------------------------------------------
! --- NAMELIST
!---------------------------------------------------------------------

 real    :: TKEmax       =  5.0
 real    :: TKEmin       =  0.0
 real    :: el0max       =  1.0e6
 real    :: el0min       =  0.0
 real    :: alpha_land   =  0.10
 real    :: alpha_sea    =  0.10
 real    :: akmax        =  1.0e4
 real    :: akmin_land   =  5.0
 real    :: akmin_sea    =  0.0
 integer :: nk_lim       =  2
 integer :: init_iters   =  20
 logical :: do_thv_stab  = .true.
 logical :: use_old_cons = .false.
 real    :: kcrit        =  0.01

  NAMELIST / my25_turb_nml /                           &
         TKEmax,   TKEmin,   init_iters,               &
         akmax,    akmin_land, akmin_sea, nk_lim,      &
         el0max,   el0min,  alpha_land,  alpha_sea,    &
         do_thv_stab, use_old_cons,                    &
         kcrit

!---------------------------------------------------------------------

 contains

!#######################################################################

 SUBROUTINE MY25_TURB( delt, fracland, phalf, pfull, theta, &
                       um,   vm,       zhalf, zfull, z0,    &
                       TKE,  el0,      el,    akm,   akh,   &
                       mask, kbot,     ustar, bstar, h    )

!=======================================================================
!---------------------------------------------------------------------
! Arguments (Intent in)
!       delt     -  Time step in seconds
!       fracland -  Fractional amount of land beneath a grid box
!       phalf    -  Pressure at half levels
!       pfull    -  Pressure at full levels
!       theta    -  Potential temperature
!       um, vm   -  Wind components
!       zhalf    -  Height at half levels
!       zfull    -  Height at full levels
!       z0       -  Roughness length
!       mask     -  OPTIONAL; floating point mask (0. or 1.) designating
!                   where data is present
!       kbot     -  OPTIONAL;lowest model level index (integer);
!                    at levels > kbot, mask = 0.
!       ustar    -  OPTIONAL:friction velocity (m/sec)
!       bstar    -  OPTIONAL:buoyancy scale (m/sec**2)
!---------------------------------------------------------------------
  real,    intent(in)                   :: delt
  real,    intent(in), dimension(:,:)   :: fracland, z0
  real,    intent(in), dimension(:,:,:) :: phalf, pfull, zhalf, zfull
  real,    intent(in), dimension(:,:,:) :: um, vm, theta

  integer, intent(in), OPTIONAL, dimension(:,:)   :: kbot
  real,    intent(in), OPTIONAL, dimension(:,:,:) :: mask
  real,    intent(in), OPTIONAL, dimension(:,:)   :: ustar, bstar

!---------------------------------------------------------------------
! Arguments (Intent in/out)
!       TKE  -  turbulent kinetic energy
!---------------------------------------------------------------------
  real, intent(inout), dimension(:,:,:) :: TKE

!---------------------------------------------------------------------
! Arguments (Intent out)
!       el0  -  characteristic length scale
!       el   -  master length scale
!       akm  -  mixing coefficient for momentum
!       akh  -  mixing coefficient for heat and moisture
!         h  -  OPTIONAL, diagnosed depth of planetary boundary
!                         layer (m)
!---------------------------------------------------------------------
  real, intent(out), dimension(:,:)   :: el0
  real, intent(out), dimension(:,:,:) :: akm, akh, el
  real, intent(out), OPTIONAL, dimension(:,:) :: h

!---------------------------------------------------------------------
!  (Intent local)
!---------------------------------------------------------------------
  integer :: ix, jx, kx, i, j, k
  integer :: kxp, kxm, klim, it, itermax
  real    :: cvfqdt, dvfqdt

  real, dimension(SIZE(um,1),SIZE(um,2)) :: zsfc, x1, x2, akmin

  real, dimension(SIZE(um,1),SIZE(um,2),SIZE(um,3)-1) ::     &
        dsdzh, shear, buoync, qm2,  qm3, qm4, el2,           &
        aaa,   bbb,   ccc,    ddd,                           &
        xxm1,  xxm2,  xxm3,   xxm4, xxm5

  real, dimension(SIZE(um,1),SIZE(um,2),SIZE(um,3)) ::       &
        dsdz, qm,  xx1, xx2

!====================================================================

! --- Check to see if MY25_TURB has been initialized
!del  if( do_init ) CALL ERROR_MESG( ' MY25_TURB',     &
!del                                 ' MY25_TURB_INIT has not been called',&
!del                                   FATAL )

! --- Set dimensions etc
  ix  = SIZE( um, 1 )
  jx  = SIZE( um, 2 )
  kx  = SIZE( um, 3 )
  kxp = kx + 1
  kxm = kx - 1

!====================================================================
! --- SURFACE HEIGHT
!====================================================================

  if( PRESENT( kbot ) ) then
     do j = 1,jx
     do i = 1,ix
        k = kbot(i,j) + 1
       zsfc(i,j) = zhalf(i,j,k)
     end do
     end do
  else
       zsfc(:,:) = zhalf(:,:,kxp)
  endif

!====================================================================
! --- D( )/DZ OPERATORS: AT FULL LEVELS & AT HALF LEVELS
!====================================================================

   dsdz(:,:,1:kx)  = 1.0 / ( zhalf(:,:,2:kxp) - zhalf(:,:,1:kx) )
  dsdzh(:,:,1:kxm) = 1.0 / ( zfull(:,:,2:kx)  - zfull(:,:,1:kxm) )

!====================================================================
! --- WIND SHEAR
!====================================================================

  xxm1(:,:,1:kxm) = dsdzh(:,:,1:kxm)*( um(:,:,2:kx) - um(:,:,1:kxm) )
  xxm2(:,:,1:kxm) = dsdzh(:,:,1:kxm)*( vm(:,:,2:kx) - vm(:,:,1:kxm) )

  shear = xxm1 * xxm1 + xxm2 * xxm2

!====================================================================
! --- BUOYANCY
!====================================================================

  xxm1(:,:,1:kxm) = theta(:,:,2:kx) - theta(:,:,1:kxm)

  if( do_thv_stab ) then
     xxm2(:,:,1:kxm) = 0.5*( theta(:,:,2:kx) + theta(:,:,1:kxm) )
  else
     xxm2(:,:,1:kxm) = t00
  end if

  buoync = grav * dsdzh * xxm1 / xxm2

!====================================================================
! --- MASK OUT UNDERGROUND VALUES FOR ETA COORDINATE
!====================================================================

  if( PRESENT( mask ) ) then
    where (mask(:,:,2:kx) < 0.1)   ! assume mask(k=1) always eq 1 ?
         TKE(:,:,3:kxp) = 0.0
        dsdz(:,:,2:kx ) = 0.0
       dsdzh(:,:,1:kxm) = 0.0
       shear(:,:,1:kxm) = 0.0
      buoync(:,:,1:kxm) = 0.0
    endwhere
  endif

!====================================================================
! --- SET ITERATION LOOP IF INITALIZING TKE
!====================================================================

            itermax = 1
     if (init_tke) then
            itermax = init_iters
            pts_done = pts_done + ix*jx
            if (pts_done >= num_total_pts) init_tke = .false.
     endif

! $$$$$$$$$$$$$$$$$
  do it = 1,itermax
! $$$$$$$$$$$$$$$$$

!====================================================================
! --- SOME TKE STUFF
!====================================================================

  xx1(:,:,1:kx)  = 2.0 * TKE(:,:,2:kxp)
  where (xx1(:,:,1:kx) > 0.0)
    qm(:,:,1:kx)  = SQRT( xx1(:,:,1:kx) )
  elsewhere
    qm(:,:,1:kx)  = 0.0
  endwhere

  qm2(:,:,1:kxm)  = xx1(:,:,1:kxm)
  qm3(:,:,1:kxm)  =  qm(:,:,1:kxm) * qm2(:,:,1:kxm)
  qm4(:,:,1:kxm)  = qm2(:,:,1:kxm) * qm2(:,:,1:kxm)

!====================================================================
! --- CHARACTERISTIC LENGTH SCALE
!====================================================================

  xx1(:,:,1:kxm) = qm(:,:,1:kxm)*( pfull(:,:,2:kx) - pfull(:,:,1:kxm) )
  do k = 1, kxm
     xx2(:,:,k) = xx1(:,:,k)  * ( zhalf(:,:,k+1) - zsfc(:,:) )
  end do

  if( PRESENT( kbot ) ) then
       xx1(:,:,kx) = 0.0
       xx2(:,:,kx) = 0.0
     do j = 1,jx
     do i = 1,ix
        k = kbot(i,j)
       xx1(i,j,k)  =  qm(i,j,k) * ( phalf(i,j,k+1) - pfull(i,j,k) )
       xx2(i,j,k)  = xx1(i,j,k) * z0(i,j)
     end do
     end do
  else
       xx1(:,:,kx) =  qm(:,:,kx) * ( phalf(:,:,kxp) - pfull(:,:,kx) )
       xx2(:,:,kx) = xx1(:,:,kx) * z0(:,:)
  endif

  if (PRESENT(mask)) then
    x1 = SUM( xx1, 3, mask=mask.gt.0.1 )
    x2 = SUM( xx2, 3, mask=mask.gt.0.1 )
  else
    x1 = SUM( xx1, 3 )
    x2 = SUM( xx2, 3 )
  endif

!---- should never be equal to zero ----
!del  if (count(x1 <= 0.0) > 0) CALL ERROR_MESG( ' MY25_TURB',  &
!del                             'divid by zero, x1 <= 0.0', FATAL)
  el0 = x2 / x1
  el0 = el0 * (alpha_land*fracland + alpha_sea*(1.-fracland))

  el0 = MIN( el0, el0max )
  el0 = MAX( el0, el0min )

!====================================================================
! --- MASTER LENGTH SCALE
!====================================================================

  do k = 1, kxm
     xx1(:,:,k)  = vonkarm * ( zhalf(:,:,k+1) - zsfc(:,:) )
  end do

  x1(:,:) = vonkarm * z0(:,:)

  if( PRESENT( kbot ) ) then
     do j = 1,jx
     do i = 1,ix
        do k = kbot(i,j), kx
          xx1(i,j,k) = x1(i,j)
        end do
     end do
     end do
  else
        xx1(:,:,kx) = x1(:,:)
  endif

  do k = 1,kx
    el(:,:,k+1) = xx1(:,:,k) / ( 1.0 + xx1(:,:,k) / el0(:,:) )
  end do
    el(:,:,1)   = el0(:,:)

  el2(:,:,1:kxm) = el(:,:,2:kx) * el(:,:,2:kx)

!====================================================================
! --- MIXING COEFFICIENTS
!====================================================================

  xxm3(:,:,1:kxm) = el2(:,:,1:kxm)*buoync(:,:,1:kxm)
  xxm4(:,:,1:kxm) = el2(:,:,1:kxm)* shear(:,:,1:kxm)
  xxm5(:,:,1:kxm) =  el(:,:,2:kx )*   qm3(:,:,1:kxm)

!-------------------------------------------------------------------
! --- MOMENTUM
!-------------------------------------------------------------------

  xxm1 = xxm5*( ckm1*qm2 + ckm2*xxm3 )
  xxm2 = qm4 + ckm5*qm2*xxm4 + xxm3*( ckm6*xxm4 + ckm7*qm2 + ckm8*xxm3 )

  xxm2 = MAX( xxm2, 0.2*qm4 )
  xxm2 = MAX( xxm2, small  )

  akm(:,:,1)    = 0.0
  akm(:,:,2:kx) = xxm1(:,:,1:kxm) / xxm2(:,:,1:kxm)

  akm = MAX( akm, 0.0 )

!-------------------------------------------------------------------
! --- HEAT AND MOISTURE
!-------------------------------------------------------------------

  xxm1(:,:,1:kxm) = ckh1*xxm5(:,:,1:kxm) - ckh2*xxm4(:,:,1:kxm)*akm(:,:,2:kx)
  xxm2(:,:,1:kxm) = qm2(:,:,1:kxm) + ckh3*xxm3(:,:,1:kxm)

  xxm1 = MAX( xxm1, ckh4*xxm5 )
  xxm2 = MAX( xxm2, 0.4*qm2   )
  xxm2 = MAX( xxm2, small     )

  akh(:,:,1)    = 0.0
  akh(:,:,2:kx) = xxm1(:,:,1:kxm) / xxm2(:,:,1:kxm)

!-------------------------------------------------------------------
! --- BOUNDS
!-------------------------------------------------------------------

! --- UPPER BOUND
  akm = MIN( akm, akmax )
  akh = MIN( akh, akmax )

! --- LOWER BOUND
!  where( akm(:,:,1:klim) < small )  akm(:,:,1:klim) = 0.0
!  where( akh(:,:,1:klim) < small )  akh(:,:,1:klim) = 0.0

! --- LOWER BOUND NEAR SURFACE

  akmin = akmin_land*fracland + akmin_sea*(1.-fracland)

  if( PRESENT( kbot ) ) then
     do j = 1,jx
     do i = 1,ix
             klim = kbot(i,j) - nk_lim + 1
     do  k = klim,kbot(i,j)
        akm(i,j,k) = MAX( akm(i,j,k), akmin(i,j) )
        akh(i,j,k) = MAX( akh(i,j,k), akmin(i,j) )
     end do
     end do
     end do
  else
             klim = kx - nk_lim + 1
     do  k = klim,kx
        akm(:,:,k) = MAX( akm(:,:,k), akmin(:,:) )
        akh(:,:,k) = MAX( akh(:,:,k), akmin(:,:) )
     end do
  endif

!-------------------------------------------------------------------
! --- MASK OUT UNDERGROUND VALUES FOR ETA COORDINATE
!-------------------------------------------------------------------

 if( PRESENT( mask ) ) then
     akm(:,:,1:kx) = akm(:,:,1:kx) * mask(:,:,1:kx)
     akh(:,:,1:kx) = akh(:,:,1:kx) * mask(:,:,1:kx)
 endif

!====================================================================
! --- PROGNOSTICATE TURBULENT KE
!====================================================================

  cvfqdt = cvfq1 * delt
  dvfqdt = cvfq2 * delt * 2.0

!-------------------------------------------------------------------
! --- PART OF LINEARIZED ENERGY DISIIPATION TERM
!-------------------------------------------------------------------

  xxm1(:,:,1:kxm) = dvfqdt * qm(:,:,1:kxm) / el(:,:,2:kx)

!-------------------------------------------------------------------
! --- PART OF LINEARIZED VERTICAL DIFFUSION TERM
!-------------------------------------------------------------------

  xx1(:,:,1:kx) = el(:,:,2:kxp) * qm(:,:,1:kx)

  xx2(:,:,1)    = 0.5*  xx1(:,:,1)
  xx2(:,:,2:kx) = 0.5*( xx1(:,:,2:kx) + xx1(:,:,1:kxm) )

  xx1 = xx2 * dsdz

!-------------------------------------------------------------------
! --- IMPLICIT TIME DIFFERENCING FOR VERTICAL DIFFUSION
! --- AND ENERGY DISSIPATION TERM
!-------------------------------------------------------------------

  aaa(:,:,1:kxm) = -cvfqdt * xx1(:,:,2:kx ) * dsdzh(:,:,1:kxm)
  ccc(:,:,1:kxm) = -cvfqdt * xx1(:,:,1:kxm) * dsdzh(:,:,1:kxm)
  bbb(:,:,1:kxm) =     1.0 - aaa(:,:,1:kxm) -   ccc(:,:,1:kxm)
  bbb(:,:,1:kxm) =           bbb(:,:,1:kxm) +  xxm1(:,:,1:kxm)
  ddd(:,:,1:kxm) =           TKE(:,:,2:kx )

! correction for vertical diffusion of TKE surface boundary condition

  if (present(kbot)) then
     do j = 1,jx
     do i = 1,ix
          k = kbot(i,j)
          ddd(:,:,k-1) = ddd(:,:,k-1) - aaa(:,:,k-1) * TKE(:,:,k+1)
     enddo
     enddo
  else
          ddd(:,:,kxm) = ddd(:,:,kxm) - aaa(:,:,kxm) * TKE(:,:,kxp)
  endif

! mask out terms below ground

  if (present(mask)) then
     where (mask(:,:,2:kx) < 0.1) ddd(:,:,1:kxm) = 0.0
  endif


!del  CALL TRI_INVERT( xxm1, ddd, aaa, bbb, ccc )
!del  CALL CLOSE_TRIDIAGONAL

!-------------------------------------------------------------------
! --- MASK OUT UNDERGROUND VALUES FOR ETA COORDINATE
!-------------------------------------------------------------------

 if( PRESENT( mask ) ) then
    where (mask(:,:,2:kx) < 0.1) xxm1(:,:,1:kxm) = TKE(:,:,2:kx)
 endif

!-------------------------------------------------------------------
! --- SHEAR AND BUOYANCY TERMS
!-------------------------------------------------------------------

  xxm2(:,:,1:kxm) =  delt*( akm(:,:,2:kx)* shear(:,:,1:kxm)    &
                          - akh(:,:,2:kx)*buoync(:,:,1:kxm) )

!-------------------------------------------------------------------
! --- UPDATE TURBULENT KINETIC ENERGY
!-------------------------------------------------------------------

  TKE(:,:,1)    = 0.0
  TKE(:,:,2:kx) = xxm1(:,:,1:kxm) +  xxm2(:,:,1:kxm)

!====================================================================
! --- BOUND TURBULENT KINETIC ENERGY
!====================================================================

  TKE = MIN( TKE, TKEmax )
  TKE = MAX( TKE, TKEmin )

  if( PRESENT( mask ) ) then
     where (mask(:,:,1:kx) < 0.1) TKE(:,:,2:kxp) = 0.0
  endif

!====================================================================
! --- COMPUTE PBL DEPTH IF DESIRED
!====================================================================

  if (present(h)) then

      if (.not.present(ustar).or..not.present(bstar)) then
!del          CALL ERROR_MESG( ' MY25_TURB',     &
!del              'cannot request pbl depth diagnostic if ustar'// &
!del              ' and bstar are not also supplied', FATAL )
      end if
      if (present(kbot)) then
          call k_pbl_depth(ustar,bstar,akm,akh,zsfc,zfull,zhalf,&
                           h,kbot=kbot)
      else
          call k_pbl_depth(ustar,bstar,akm,akh,zsfc,zfull,zhalf,h)
      end if

  end if
!====================================================================

! $$$$$$$$$$$$$$$$$
  end do
! $$$$$$$$$$$$$$$$$

!====================================================================
  end SUBROUTINE MY25_TURB

!#######################################################################

  SUBROUTINE MY25_TURB_INIT( ix, jx, kx )

!=======================================================================
! ***** INITIALIZE MELLOR-YAMADA
!=======================================================================
!---------------------------------------------------------------------
! Arguments (Intent in)
!     ix, jx  - Horizontal dimensions for global storage arrays
!     kx      - Number of vertical levels in model
!---------------------------------------------------------------------
 integer, intent(in) :: ix, jx, kx
!---------------------------------------------------------------------
!  (Intent local)
!---------------------------------------------------------------------
 integer             :: unit, io, ierr
 real                :: actp, facm
 real, dimension(15) :: au,   tem

!=====================================================================

!---------------------------------------------------------------------
! --- Read namelist
!---------------------------------------------------------------------

!del  if( FILE_EXIST( 'input.nml' ) ) then
! -------------------------------------
!del   unit = OPEN_NAMELIST_FILE ()
!del   ierr = 1
!del   do while( ierr .ne. 0 )
!del   READ ( unit,  nml = my25_turb_nml, iostat = io, end = 10 )
!del   ierr = check_nml_error (io, 'my25_turb_nml')
!del   end do
10 continue
!del   CALL CLOSE_FILE( unit )
! -------------------------------------
!del  end if

!---------------------------------------------------------------------
! --- Output version
!---------------------------------------------------------------------


!del      if ( mpp_pe() == mpp_root_pe() )write (stdlog(), nml=my25_turb_nml)
!del      call write_version_number(version, tag)

!---------------------------------------------------------------------
! --- Initialize constants
!---------------------------------------------------------------------

  if( use_old_cons ) then
      aa1 = aa1_old
      aa2 = aa2_old
      bb1 = bb1_old
      bb2 = bb2_old
      ccc = ccc_old
  else
      aa1 = aa1_new
      aa2 = aa2_new
      bb1 = bb1_new
      bb2 = bb2_new
      ccc = ccc_new
  end if

     ckm1 = ( 1.0 - 3.0*ccc )*aa1
     ckm3 =  3.0 * aa1*aa2*    ( bb2 - 3.0*aa2 )
     ckm4 =  9.0 * aa1*aa2*ccc*( bb2 + 4.0*aa1 )
     ckm5 =  6.0 * aa1*aa1
     ckm6 = 18.0 * aa1*aa1*aa2*( bb2 - 3.0*aa2 )
     ckm7 =  3.0 * aa2*        ( bb2 + 7.0*aa1 )
     ckm8 = 27.0 * aa1*aa2*aa2*( bb2 + 4.0*aa1 )
     ckm2 =  ckm3 - ckm4
     ckh1 =  aa2
     ckh2 =  6.0 * aa1*aa2
     ckh3 =  3.0 * aa2*( bb2 + 4.0*aa1 )
     ckh4 =  2.0e-6 * aa2
    cvfq1 = 5.0 * cc1 / 3.0
    cvfq2 = 1.0 / bb1
      bcq = 0.5 * ( bb1**(2.0/3.0) )

!---------------------------------------------------------------------
! --- Allocate storage for TKE
!---------------------------------------------------------------------

  if( ALLOCATED( TKE ) ) DEALLOCATE( TKE )
                           ALLOCATE( TKE(ix,jx,kx+1) )

!-------------------------------------------------------------------
  do_init = .false.
!---------------------------------------------------------------------
! --- Input TKE
!---------------------------------------------------------------------

!del  if( FILE_EXIST( 'INPUT/my25_turb.res' ) ) then

!del      unit = OPEN_FILE ( file = 'INPUT/my25_turb.res', &
!del                         form = 'native', action = 'read' )
!del      call read_data ( unit, TKE )
!del      CALL CLOSE_FILE( unit )

      init_tke = .false.

!del  else

      TKE  = TKEmin

      init_tke      = .true.
      num_total_pts = ix*jx
      pts_done      = 0

!del  endif


!=====================================================================
  end SUBROUTINE MY25_TURB_INIT

!#######################################################################

  SUBROUTINE MY25_TURB_END
!=======================================================================
 integer :: unit
!=======================================================================

!del      unit = OPEN_FILE ( file = 'RESTART/my25_turb.res', &
!del                         form = 'native', action = 'write' )
!del      call write_data ( unit, TKE )
!del      CALL CLOSE_FILE ( unit )

!=====================================================================

  end SUBROUTINE MY25_TURB_END

!#######################################################################

  SUBROUTINE K_PBL_DEPTH(ustar,bstar,akm,akh,zsfc,zfull,zhalf,h,kbot)
!=======================================================================

  real,    intent(in), dimension(:,:)   :: ustar, bstar, zsfc
  real,    intent(in), dimension(:,:,:) :: zhalf, zfull
  real,    intent(in), dimension(:,:,:) :: akm, akh
  real,    intent(out),dimension(:,:)   :: h
  integer, intent(in), optional, dimension(:,:)   :: kbot

!=======================================================================
  real,    dimension(size(zfull,1),size(zfull,2)) :: km_surf, kh_surf
  real,    dimension(size(zfull,1),size(zfull,2)) :: zhalfhalf
  real, dimension(size(zfull,1),size(zfull,2),size(zfull,3))  :: zfull_ag
  real, dimension(size(zfull,1),size(zfull,2),size(zfull,3)+1):: zhalf_ag
  real, dimension(size(zfull,1),size(zfull,2),size(zfull,3)+1):: diff_tm
  integer, dimension(size(zfull,1),size(zfull,2))            :: ibot
  integer                                         :: i,j,k,nlev,nlat,nlon


nlev = size(zfull,3)
nlat = size(zfull,2)
nlon = size(zfull,1)

!compute height of surface
if (present(kbot)) then
   ibot=kbot
else
   ibot(:,:) = nlev
end if

!compute density profile, and heights relative to surface
do k = 1, nlev
  zfull_ag(:,:,k) = zfull(:,:,k) - zsfc(:,:)
  zhalf_ag(:,:,k) = zhalf(:,:,k) - zsfc(:,:)
  end do
zhalf_ag(:,:,nlev+1) = zhalf(:,:,nlev+1) - zsfc(:,:)


!compute height half way between surface and lowest model level
zhalfhalf=0.5*MINVAL(MAX(zfull_ag,0.0))

!compute k's there by a call to mo_diff
call mo_diff(zhalfhalf,ustar,bstar,km_surf,kh_surf,1)

!create combined surface k's and diffusivity matrix
diff_tm(:,:,nlev+1) = 0.
diff_tm(:,:,1:nlev) = 0.5*(akm+akh)
if (present(kbot)) then
do j=1,nlat
do i=1,nlon
   diff_tm(i,j,ibot(i,j)+1) = 0.5*(km_surf(i,j)+kh_surf(i,j))
   zhalf_ag(i,j,ibot(i,j)+1) = zhalfhalf(i,j)
enddo
enddo
else
   diff_tm(:,:,nlev+1) = 0.5*(km_surf(:,:)+kh_surf(:,:))
   zhalf_ag(:,:,nlev+1) = zhalfhalf(:,:)
end if


!determine pbl depth as the height above ground where diff_tm
!first falls beneath a critical value kcrit.  If the value between
!ground and level 1 does not exceed kcrit set pbl depth equal to zero.
!kcrit is a namelist parameter.

do j = 1,nlat
do i = 1,nlon
             if (diff_tm(i,j,ibot(i,j)+1) .gt. kcrit) then
                       k=ibot(i,j)+1
                       do while (k.gt. 2 .and. &
                                 diff_tm(i,j,k-1).gt.kcrit)
                             k=k-1
                       enddo
                       h(i,j) = zhalf_ag(i,j,k) + &
                         (zhalf_ag(i,j,k-1)-zhalf_ag(i,j,k))* &
                         (diff_tm(i,j,k)-kcrit) / &
                         (diff_tm(i,j,k)-diff_tm(i,j,k-1))
             else
                       h(i,j) = 0.
             end if
enddo
enddo

!-----------------------------------------------------------------------


!=====================================================================
  end SUBROUTINE K_PBL_DEPTH

!#######################################################################

 SUBROUTINE TKE_SURF ( u_star, TKE, kbot )

!=======================================================================
!---------------------------------------------------------------------
! Arguments (Intent in)
!       u_star -  surface friction velocity (m/s)
!       kbot   -  OPTIONAL;lowest model level index (integer);
!                 at levels > Kbot, Mask = 0.
!---------------------------------------------------------------------
  real, intent(in), dimension(:,:)   :: u_star

  integer, intent(in), OPTIONAL, dimension(:,:) :: kbot

!---------------------------------------------------------------------
! Arguments (Intent inout)
!       TKE  -  turbulent kinetic energy
!---------------------------------------------------------------------
  real, intent(inout), dimension(:,:,:) :: TKE

!---------------------------------------------------------------------
!  (Intent local)
!---------------------------------------------------------------------
  real, dimension(SIZE(u_star,1),SIZE(u_star,2)) :: x1
  integer  :: ix, jx, kxp, i, j, k

!=======================================================================

  ix  = SIZE( u_star, 1 )
  jx  = SIZE( u_star, 2 )
  kxp = SIZE( TKE,    3 )

!---------------------------------------------------------------------

  x1 = bcq * u_star * u_star

  if( PRESENT( kbot ) ) then
    do j = 1,jx
    do i = 1,ix
       k = kbot(i,j) + 1
      TKE(i,j,k) = x1(i,j)
    end do
    end do
  else
      TKE(:,:,kxp) = x1(:,:)
  endif

!=======================================================================
 end SUBROUTINE TKE_SURF

!#######################################################################
  end MODULE MY25_TURB_MOD
