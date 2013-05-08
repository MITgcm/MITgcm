! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/shallow_conv_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

  MODULE SHALLOW_CONV_MOD

!=======================================================================
! --- SHALLOW CONVECTION MODULE - GFDL SPECTRAL MODEL VERSION
!=======================================================================

 use  simple_Sat_Vapor_Pres_Mod, ONLY: ESCOMP, DESCOMP
! use       fms_Mod,       ONLY: FILE_EXIST, ERROR_MESG, FATAL,   &
!                                CHECK_NML_ERROR, OPEN_namelist_FILE,      &
!                                CLOSE_FILE, mpp_pe, mpp_root_pe, stdlog,  &
!                                write_version_number

 use constants_mod, only: Hlv, Cp_air, RDgas, RVgas, Kappa, grav

!---------------------------------------------------------------------
 implicit none
 private
!---------------------------------------------------------------------

 public  :: SHALLOW_CONV, SHALLOW_CONV_INIT
 public  :: MYLCL

!---------------------------------------------------------------------

 character(len=128) :: version = '$Id: shallow_conv_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
 character(len=128) :: tag = '$Name:  $'

 logical :: do_init = .true.

!---------------------------------------------------------------------
! --- CONSTANTS
!---------------------------------------------------------------------

  real :: Hlv_by_Cp, Cp_by_RDgas, omkappa, dovcns, d622, d378
  real :: crtkons
  real, parameter :: p00 = 1000.0E2
  integer  :: kctopm1, kctopm2

  real, allocatable, dimension(:) :: rhcrit, rhmax, rhmin, delrhc

!---------------------------------------------------------------------
! --- NAMELIST
!---------------------------------------------------------------------

  logical ::  lipps    = .false.
  logical ::  ldetran  = .true.
  real    ::  theqvcr  =    0.0
  real    ::  pshalow  =  750.0E2
  real    ::  akhsc0   =    5.0
  real    ::  crthum   =    0.85
  real    ::  hc       =    1.0
  integer ::  kctop    =    3


 NAMELIST / shallow_conv_nml /    &
         lipps, ldetran,  theqvcr, pshalow, akhsc0, kctop, crthum, hc

!---------------------------------------------------------------------

 contains

!#######################################################################
!#######################################################################

 SUBROUTINE SHALLOW_CONV_INIT( kx )

!=======================================================================
! ***** INITIALIZE SHALLOW CONVECTION
!=======================================================================
!---------------------------------------------------------------------
! Arguments (Intent in)
!     kx     - Number of levels in vertical
!---------------------------------------------------------------------
 integer, intent(in) :: kx

!---------------------------------------------------------------------
!  (Intent local)
!---------------------------------------------------------------------
 integer :: unit, io, ierr

!=====================================================================

  if (.not.do_init) return

!---------------------------------------------------------------------
! --- Read namelist
!---------------------------------------------------------------------

!del  if( FILE_EXIST( 'input.nml' ) ) then
! -------------------------------------
!del   unit = OPEN_namelist_FILE ()
!del   ierr = 1
!del   do while( ierr .ne. 0 )
!del   READ ( unit,  nml = shallow_conv_nml, iostat = io, end = 10 )
!del   ierr = CHECK_NML_ERROR(io,'shallow_conv_nml')
!del   end do
10 continue
!del   CALL CLOSE_FILE ( unit )
! -------------------------------------
!del  end if

!---------------------------------------------------------------------
! --- Output version
!---------------------------------------------------------------------

!del  call write_version_number(version, tag)
!del  if ( mpp_pe() == mpp_root_pe() ) write (stdlog(), nml=shallow_conv_nml)

!---------------------------------------------------------------------
! --- Initialize constants
!---------------------------------------------------------------------

  d622        = RDgas / RVgas
  d378        = 1.0 - d622
  Hlv_by_Cp   = Hlv / Cp_air
  Cp_by_RDgas = Cp_air / RDgas
  omkappa     = 1.0 - Kappa
  dovcns      = -0.5 * grav / RDgas

!----------------------------------------------------

              crtkons = -1.0 * theqvcr * RDgas / grav
  if( lipps ) crtkons = 0.0

!----------------------------------------------------

  kctopm1 = kctop - 1
  kctopm2 = kctop - 2

!----------------------------------------------------

  allocate( rhcrit(kx) )
  allocate(  rhmax(kx) )
  allocate(  rhmin(kx) )
  allocate( delrhc(kx) )

  rhcrit = crthum
  rhmax  = hc
  rhmin  = 2.0 * rhcrit - hc - 1.0e-15
  delrhc = 1.0 / ( rhmax - rhmin )

!-------------------------------------------------------------------
  do_init = .false.
!---------------------------------------------------------------------

!=====================================================================
  end SUBROUTINE SHALLOW_CONV_INIT

!#######################################################################

  SUBROUTINE SHALLOW_CONV( Temp, qmix0, pfull, phalf, akhsc, kbot )

!=======================================================================
! --- SHALLOW CONVECTION
!=======================================================================
!----------------------------------------------------------------------
! Arguments (Intent in)
!       Temp    -  Temperature
!       qmix0   -  Specific humidity
!       pfull   -  Pressure at full levels
!       phalf   -  Pressure at half levels
!       kbot    -  OPTIONAL; lowest model level index (integer)
!----------------------------------------------------------------------
  real, intent(in), dimension(:,:,:) :: Temp, qmix0, pfull, phalf

  integer, intent(in), OPTIONAL, dimension(:,:) :: kbot

!----------------------------------------------------------------------
! Arguments (Intent out)
!       akhsc  -  mixing coefficient for heat and moisture
!                 due to shallow convection
!----------------------------------------------------------------------
  real, intent(out), dimension(:,:,:) :: akhsc

!----------------------------------------------------------------------
! --- local
!----------------------------------------------------------------------
  real,    dimension(SIZE(Temp,1),SIZE(Temp,2)) ::                &
           plcl, rhumjmp, rhumscl,  xy1, xy2, xy3

  integer, dimension(SIZE(Temp,1),SIZE(Temp,2)) ::                &
           ksiglcl

  real,    dimension(SIZE(Temp,1),SIZE(Temp,2),SIZE(Temp,3)) ::   &
           qmix,  dphalf, qsat, rhum, theta, thetav, buoy, xyz1

  integer, dimension(SIZE(Temp,1),SIZE(Temp,2),SIZE(Temp,3)) ::   &
           kbuoy

 integer :: k, kx, kxm, kxp, i, ix, j, jx

!=======================================================================
!=======================================================================

  ix  = SIZE(Temp,1)
  jx  = SIZE(Temp,2)
  kx  = SIZE(Temp,3)

  kxm = kx - 1
  kxp = kx + 1

!=======================================================================
! --- MOISTURE VARIABLES
!=======================================================================

  qmix = qmix0
  qmix = MAX( qmix, 1.0E-6 )
  qmix = MIN( qmix, 0.2    )

! --- saturation mixing ratio
  CALL ESCOMP( Temp, qsat )
  xyz1 = pfull - d378 * qsat
  qsat = d622 * qsat / xyz1

! --- relative humidity
  rhum = qmix / qsat

!=======================================================================
! --- POTENTIAL TEMPERATURE
!=======================================================================

  theta  = Temp  * ( ( p00 / pfull )**Kappa )

!=======================================================================
! --- CALCULATE THE LIFTING CONDENSATION LEVEL, IE CLOUB BASE
!=======================================================================

  if( PRESENT( kbot ) ) then
     do j = 1,jx
     do i = 1,ix
        k = kbot(i,j)
             xy1(i,j) =      Temp(i,j,k)
             xy2(i,j) = MIN( qmix(i,j,k), qsat(i,j,k) )
             xy3(i,j) =     pfull(i,j,k)
     end do
     end do
  else
             xy1(:,:) =      Temp(:,:,kx)
             xy2(:,:) = MIN( qmix(:,:,kx), qsat(:,:,kx) )
             xy3(:,:) =     pfull(:,:,kx)
  end if

  CALL MYLCL( xy1, xy2, xy3, phalf, plcl, ksiglcl )

!=======================================================================
! --- INITALIZE
!=======================================================================

  kbuoy   = kxp
  akhsc   = 0.0

!=======================================================================
! --- BUOYANCY
!=======================================================================

!---------------------------------------------------------------------
! --- DEFAULT:
! --- BASED ON EQUIVALENT POTENTIAL TEMPERATURE GRADIENT
!---------------------------------------------------------------------

  if( .not. lipps ) then
! %%%%%%%%%%%%%%%%%%%%%%%

! --- Vertical differential of pressure
  dphalf(:,:,1:kxm) = pfull(:,:,2:kx) - pfull(:,:,1:kxm)

  if( PRESENT( kbot ) ) then
  dphalf(:,:,1:kxm) = MAX( dphalf(:,:,1:kxm), 1.0e-5 )
  end if

! --- Equivalent potential temperature
  xyz1    = ( Hlv_by_Cp * qmix ) / Temp
! thetav = theta * (1.0 + xyz1 )
  thetav = theta * EXP( xyz1 )

! --- Equivalent potential temperature gradient
  xyz1(:,:,2:kx)  =     thetav(:,:,2:kx)  - thetav(:,:,1:kxm)
  xyz1(:,:,2:kx)  =       xyz1(:,:,2:kx)  / dphalf(:,:,1:kxm)
  buoy(:,:,2:kxm) = 0.5*( xyz1(:,:,2:kxm) +   xyz1(:,:,3:kx) )

! %%%%%%%%%%%%%%%%%%%%%%%
  endif

!---------------------------------------------------------------------
! --- OPTION:
! --- BUOYANCY ALA FRANK LIPPS
!---------------------------------------------------------------------

  if( lipps ) then
! %%%%%%%%%%%%%%%%%%%%%%%

! --- Virtual potential temperature gardient
  thetav = theta * ( 1.0 + 0.608 * qmix )

! --- Buoyancy
  do k = kctopm2,kxm
! -------------------
  xy1(:,:) = Hlv / ( Cp_air*RVgas*Temp(:,:,k)*Temp(:,:,k) ) + 1.0 / qsat(:,:,k)

  xy2(:,:) = ( grav / ( Cp_air*Temp(:,:,k) ) ) *                              &
             ( Hlv / ( RVgas*Temp(:,:,k) ) - Cp_by_RDgas ) / xy1(:,:)

  xy3(:,:) = ( thetav(:,:,k+1) - thetav(:,:,k )  ) / phalf(:,:,k+1) +     &
             ( thetav(:,:,k  ) - thetav(:,:,k-1) ) / phalf(:,:,k  )

  buoy(:,:,k) = ( hlv / ( cp_air*Temp(:,:,k) ) - 1.608 ) * xy2(:,:)
  buoy(:,:,k) =    buoy(:,:,k) - dovcns*pfull(:,:,k) * xy3(:,:) / Temp(:,:,k)
! -------------------
  end do

! %%%%%%%%%%%%%%%%%%%%%%%
  endif

!=======================================================================
! --- COMPUTE THE LEVEL OF NO BUOYANCY
! --- RETAIN ONLY THE LOWEST CONTIGUOUS BUOYANT SHALLOW CONVECTIVE LAYER.
!=======================================================================

  do k = kctopm1,kxm
! -------------------
  where ( ( pfull(:,:,k) >= pshalow   ) .and.    &
          ( pfull(:,:,k) <= plcl(:,:) ) .and.    &
          (  buoy(:,:,k) >= crtkons   ) )
            kbuoy(:,:,k) =  k
  endwhere
! -------------------
  end do

  do k = kctopm1,kxm
! -------------------
  where( ( pfull(:,:,k)   <  plcl(:,:) ) .and.   &
         ( kbuoy(:,:,k)   == kxp       ) .and.   &
         ( kbuoy(:,:,k-1) == k-1       ) )
           kbuoy(:,:,k-1) =  kxp
  endwhere
! -------------------
  end do

!=======================================================================
! --- SHALLOW CONVECTION WILL OCCUR AT LEVELS WHERE KBUOY <= KSIGLCL
!=======================================================================

  do k = kctopm1,kxm
! -------------------
  where( kbuoy(:,:,k) <= ksiglcl(:,:) )
         akhsc(:,:,k+1) =  akhsc0
  endwhere
! -------------------
  end do

!=======================================================================
! --- DETRAINMENT THRU INVERSION LAYER
!=======================================================================

!---------------------------------------------------------------------
! --- DEFAULT:
! --- ENHANCED DETRAINMENT THRU INVERSION LAYER.
!---------------------------------------------------------------------

  if( ldetran) then
! %%%%%%%%%%%%%%%%%%%%%%%%

  do k = kctopm1,kxm
! -------------------
  where( ( kbuoy(:,:,k)   == k       ) .and.   &
         ( kbuoy(:,:,k-1) == kxp     ) .and.   &
         ( pfull(:,:,k)   >= pshalow ) )
           akhsc(:,:,k)   =  0.2 * akhsc0
           akhsc(:,:,k+1) =  0.6 * akhsc0
  endwhere
! -------------------
  end do

  do k = kctopm1,kxm
! -------------------
  where( ( pfull(:,:,k)   <= plcl(:,:) ) .and.  &
         ( pfull(:,:,k+1) >  plcl(:,:) ) .and.  &
         ( kbuoy(:,:,k)   == k         )  )
           akhsc(:,:,k+1) =  0.2 * akhsc0
  endwhere
! -------------------
  end do

! %%%%%%%%%%%%%%%%%%%%%%%%
  endif

!---------------------------------------------------------------------
! --- OPTION:
! --- NORMAL DETRAINMENT THRU INVERSION LAYER
!---------------------------------------------------------------------

  if( .not. ldetran ) then
! %%%%%%%%%%%%%%%%%%%%%%%%

  rhumscl = 0.0
  rhumjmp = 0.0

  do k = kctopm1,kxm
! -------------------
  where( ( kbuoy(:,:,k)   == k       ) .and.    &
         (  buoy(:,:,k-1) <  crtkons ) .and.    &
         (  buoy(:,:,k)   >= crtkons ) )
         rhumjmp(:,:) =   rhum(:,:,k) -   rhum(:,:,k-1)
         rhumscl(:,:) =     delrhc(k) * ( rhum(:,:,k) - rhmin(k) )
  endwhere
! -------------------
  end do

  rhumscl = MIN( 1.0, rhumscl )
  rhumscl = MAX( 0.0, rhumscl )
  rhumjmp = MIN( 1.0, rhumjmp )
  rhumjmp = MAX( 0.0, rhumjmp )

  do k = kctopm1,kxm
! -------------------
  where( ( kbuoy(:,:,k)   == k       ) .and.   &
         ( kbuoy(:,:,k-1) == kxp     ) .and.   &
         ( pfull(:,:,k)   >= pshalow ) )
           akhsc(:,:,k)   =  akhsc0 * rhumscl(:,:) * rhumjmp(:,:)
  endwhere
! -------------------
  end do

! %%%%%%%%%%%%%%%%%%%%%%%%
  endif

!=======================================================================
! --- CONFINE SHALLOW CONVECTION TO ( pshalow <= p <= plcl )
!=======================================================================

  do k = kctopm1,kxm
! -------------------
  where ( ( pfull(:,:,k) <= pshalow   ) .or.    &
          ( pfull(:,:,k) >= plcl(:,:) ) )
            akhsc(:,:,k+1) =  0.0
  endwhere
! -------------------
  end do

!=======================================================================
  end SUBROUTINE SHALLOW_CONV

!#######################################################################

  SUBROUTINE MYLCL ( tlparc, qlparc, plparc, phalf, plcl, kbase )

!=======================================================================
! ***** COMPUTE LCL ( CLOUD BASE )
!=======================================================================
!---------------------------------------------------------------------
! Arguments (Intent in)
!       tlparc   Initial parcel temperature
!       qlparc   Initial parcel mixing ratio
!       plparc   Initial parcel pressure
!       phalf    Pressure at half levels
! Arguments (Intent out)
!       plcl     Pressure at LCL
!       kbase    Index of LCL in column
!---------------------------------------------------------------------
  real,    intent(in),  dimension(:,:)   :: tlparc, qlparc, plparc
  real,    intent(in),  dimension(:,:,:) :: phalf
  real,    intent(out), dimension(:,:)   :: plcl
  integer, intent(out), dimension(:,:)   :: kbase

!---------------------------------------------------------------------
!  (Intent local)
!---------------------------------------------------------------------
  integer, parameter :: iter_max = 10
  real,    parameter :: small    = 1.0E-2

  real,    dimension(size(tlparc,1),size(tlparc,2)) ::    &
           tlcl, tlclo, clclo, esat, esato, desato, xy1, xy2

 logical, dimension(size(tlparc,1),size(tlparc,2)) ::  &
           non_cnvg

  integer :: k, kx, n, iter

!=======================================================================

! --- Index of lowest model level, etc
  kx  = size( phalf, 3 ) - 1

! --- Initial guess for temperature at LCL
  tlclo = tlparc

! --- Compute constant factor
  clclo = ( 1.0 + d622/qlparc ) / plparc
  clclo = kappa*LOG( clclo )
  clclo =       EXP( clclo )
  clclo =    tlclo * clclo

! --- Start with all points non-convergent
  non_cnvg = .true.

! $$$$$$$$$$$$$$$$$$$$
  do iter = 1,iter_max
! $$$$$$$$$$$$$$$$$$$$

! --- Compute saturation vapor pressure and derivative
  CALL  ESCOMP ( tlclo,  esato )
  CALL DESCOMP ( tlclo, desato )

! --- Compute new guess for temperature at LCL
  where (non_cnvg)
     xy1  = kappa * clclo * desato
     xy2  = omkappa*LOG( esato )
     xy2  = EXP(  xy2 )
     tlcl = ( xy1 * tlclo - clclo * esato ) / ( xy1 - xy2 )
     xy2  = abs( tlcl - tlclo )
  end where

! --- Test for convergence
  where (non_cnvg .and. xy2 <= small)
     esat = esato
     non_cnvg = .false.
  endwhere
  n = COUNT( non_cnvg )

  if( n .eq. 0 ) go to 1000

! --- Shift for next iteration
  tlclo = tlcl

! $$$$$$$$$$$$$$$$$$$$
  end do
! $$$$$$$$$$$$$$$$$$$$
!del       CALL ERROR_MESG ('MYLCL in SHALLOW_CONV_MOD',  &
!del                        'ITERATION LOOP FOR LCL FAILED', FATAL)
 1000 continue

! --- Compute pressure at LCL
  plcl = ( 1.0 + d622 / qlparc ) * esat

! --- Bound plcl
  plcl(:,:) = MAX( plcl(:,:), pshalow      )
  plcl(:,:) = MIN( plcl(:,:),  plparc(:,:) )

! --- Find index of LCL
  do k = 2,kx
  where ( ( plcl(:,:) >= phalf(:,:,k  ) ) .and.   &
          ( plcl(:,:) <= phalf(:,:,k+1) ) )
           kbase(:,:) = k
  end where
  end do

!=======================================================================
  end SUBROUTINE MYLCL

!#######################################################################
!#######################################################################
  end MODULE SHALLOW_CONV_MOD

