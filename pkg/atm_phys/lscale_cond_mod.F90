! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/lscale_cond_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

module lscale_cond_mod

!-----------------------------------------------------------------------
!use            fms_mod, only:  file_exist, error_mesg, open_file,  &
!                               check_nml_error, mpp_pe, FATAL,  &
!                               close_file
use simple_sat_vapor_pres_mod, only:  escomp, descomp

use      constants_mod, only:  HLv,HLs,Cp_air,Grav,rdgas,rvgas
use     gcm_params_mod, only: gcm_LEN_MBUF, gcm_SQZ_R, gcm_stdMsgUnit

implicit none
private
!-----------------------------------------------------------------------
!  ---- public interfaces ----

   public  lscale_cond, lscale_cond_init

!-----------------------------------------------------------------------
!   ---- version number ----

 character(len=128) :: version = '$Id: lscale_cond_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
 character(len=128) :: tag = '$Name:  $'

!-----------------------------------------------------------------------
!   ---- local/private data ----

    real, parameter :: d622 = rdgas/rvgas
    real, parameter :: d378 = 1.-d622

    logical :: do_init=.true.

!-----------------------------------------------------------------------
!   --- namelist ----

real    :: hc=1.00
logical :: do_evap=.false.

namelist /lscale_cond_nml/  hc, do_evap

!-----------------------------------------------------------------------
!           description of namelist variables
!
!  hc        =  relative humidity at which large scale condensation
!               occurs, where 0 <= hc <= 1 (default: hc=1.)
!
!  do_evap   =  flag for the re-evaporation of moisture in
!               sub-saturated layers below, if do_evap=.true. then
!               re-evaporation is performed (default: do_evap=.false.)
!
!-----------------------------------------------------------------------

contains

!#######################################################################

!   subroutine lscale_cond (tin, qin, pfull, phalf, coldT, &
!                           rain, snow, tdel, qdel, mask, conv)
subroutine lscale_cond (tin, qin, pfull, phalf, coldT, &
                        rain, snow, tdel, qdel, qsat,  &
                        myThid, mask, conv )


!-----------------------------------------------------------------------
!
!                      large scale condensation
!
!-----------------------------------------------------------------------
!
!   input:  tin      temperature at full model levels
!           qin      specific humidity of water vapor at full
!                      model levels
!           pfull    pressure at full model levels
!           phalf    pressure at half (interface) model levels
!           coldT    should precipitation be snow at this point?
!   optional:
!           mask     optional mask (0 or 1.)
!           conv     logical flag; if true then no large-scale
!                       adjustment is performed at that grid-point or
!                       model level
!
!  output:  rain     liquid precipitation (kg/m2)
!           snow     frozen precipitation (kg/m2)
!           tdel     temperature tendency at full model levels
!           qdel     specific humidity tendency (of water vapor) at
!                      full model levels
!           qsat     saturated specific humidity
!
!-----------------------------------------------------------------------
!--------------------- interface arguments -----------------------------

   real   , intent(in) , dimension(:,:,:) :: tin, qin, pfull, phalf
   logical   , intent(in) , dimension(:,:):: coldT
   real   , intent(out), dimension(:,:)   :: rain,snow
   real   , intent(out), dimension(:,:,:) :: tdel, qdel, qsat
   integer, intent(in)                    :: myThid
   real   , intent(in) , dimension(:,:,:), optional :: mask
   logical, intent(in) , dimension(:,:,:), optional :: conv
!-----------------------------------------------------------------------
!---------------------- local data -------------------------------------

logical,dimension(size(tin,1),size(tin,2),size(tin,3)) :: do_adjust
   real,dimension(size(tin,1),size(tin,2),size(tin,3)) ::  &
                             esat, desat, dqsat, pmes, pmass
   real,dimension(size(tin,1),size(tin,2))             :: hlcp, precip
integer  k, kx
!-----------------------------------------------------------------------
!     computation of precipitation by condensation processes
!-----------------------------------------------------------------------

!      if (do_init) call error_mesg ('lscale_cond',  &
!                         'lscale_cond_init has not been called.', FATAL)

      kx=size(tin,3)

!----- compute proper latent heat --------------------------------------
      WHERE (coldT)
           hlcp = HLs/Cp_air
      ELSEWHERE
           hlcp = HLv/Cp_air
      END WHERE

!----- saturation vapor pressure (esat) & specific humidity (qsat) -----

      call  escomp (tin,esat)
      call descomp (tin,desat)


      esat(:,:,:)=esat(:,:,:)*hc

   where (pfull(:,:,:) > d378*esat(:,:,:))
      pmes(:,:,:)=1.0/(pfull(:,:,:)-d378*esat(:,:,:))
      qsat(:,:,:)=d622*esat(:,:,:)*pmes(:,:,:)
      qsat(:,:,:)=max(0.0,qsat(:,:,:))
     dqsat(:,:,:)=d622*pfull(:,:,:)*desat(:,:,:)*pmes(:,:,:)*pmes(:,:,:)
   elsewhere
      pmes(:,:,:)=0.0
      qsat(:,:,:)=0.0
     dqsat(:,:,:)=0.0
   endwhere

!--------- do adjustment where greater than saturated value ------------

   if (present(conv)) then
!!!!  do_adjust(:,:,:)=(.not.conv(:,:,:) .and. qin(:,:,:) > qsat(:,:,:))
      do_adjust(:,:,:)=(.not.conv(:,:,:) .and.   &
                         (qin(:,:,:) - qsat(:,:,:))*qsat(:,:,:) > 0.0)
   else
!!!!  do_adjust(:,:,:)=(qin(:,:,:) > qsat(:,:,:))
      do_adjust(:,:,:)=( (qin(:,:,:) - qsat(:,:,:))*qsat(:,:,:) > 0.0)
   endif

   if (present(mask)) then
      do_adjust(:,:,:)=do_adjust(:,:,:) .and. (mask(:,:,:) > 0.5)
   end if

!----------- compute adjustments to temp and spec humidity -------------
   do k = 1,kx
   where (do_adjust(:,:,k))
      qdel(:,:,k)=(qsat(:,:,k)-qin(:,:,k))/(1.0+hlcp(:,:)*dqsat(:,:,k))
      tdel(:,:,k)=-hlcp(:,:)*qdel(:,:,k)
   elsewhere
      qdel(:,:,k)=0.0
      tdel(:,:,k)=0.0
   endwhere
   end do


!------------ pressure mass of each layer ------------------------------

   do k=1,kx
      pmass(:,:,k)=(phalf(:,:,k+1)-phalf(:,:,k))/Grav
   enddo

!------------ re-evaporation of precipitation in dry layer below -------

   if (do_evap) then
      if (present(mask)) then
!        call precip_evap (pmass,tin,qin,qsat,dqsat,hlcp,tdel,qdel,mask)
         call precip_evap (pmass,tin,qin,qsat,dqsat,hlcp,tdel,qdel,      &
                           myThid, mask )
      else
         call precip_evap (pmass,tin,qin,qsat,dqsat,hlcp,tdel,qdel,      &
                           myThid )
      endif
   endif

!------------ integrate precip -----------------------------------------

      precip(:,:)=0.0
   do k=1,kx
      precip(:,:)=precip(:,:)-pmass(:,:,k)*qdel(:,:,k)
   enddo
      precip(:,:)=max(precip(:,:),0.0)

   !assign precip to snow or rain
   WHERE (coldT)
      snow = precip
      rain = 0.
   ELSEWHERE
      rain = precip
      snow = 0.
   END WHERE

!-----------------------------------------------------------------------

   end subroutine lscale_cond

!#######################################################################

! subroutine precip_evap (pmass, tin, qin, qsat, dqsat, hlcp, &
!                         tdel, qdel, mask)
subroutine precip_evap (pmass, tin, qin, qsat, dqsat, hlcp, &
                        tdel, qdel, myThid, mask )

!-----------------------------------------------------------------------
!        performs re-evaporation of falling precipitation
!-----------------------------------------------------------------------
   real, intent(in),    dimension(:,:,:) :: pmass, tin, qin, qsat, dqsat
   real, intent(in),    dimension(:,:)   :: hlcp
   real, intent(inout), dimension(:,:,:) :: tdel, qdel
   integer, intent(in)                   :: myThid
   real, intent(in), dimension(:,:,:), optional :: mask
!-----------------------------------------------------------------------
   real, dimension(size(tin,1),size(tin,2)) :: exq, def

   integer  k
!-----------------------------------------------------------------------
    exq(:,:)=0.0

    do k=1,size(tin,3)

        where (qdel(:,:,k) < 0.0)  exq(:,:) = exq(:,:) -  &
                                               qdel(:,:,k)*pmass(:,:,k)

        if (present(mask)) exq(:,:) = exq(:,:)*mask(:,:,k)

!  ---- evaporate precip where needed ------

        where ( (qdel(:,:,k) >= 0.0) .and. (exq(:,:) > 0.0) )
            exq(:,:) = exq(:,:) / pmass(:,:,k)
            def(:,:) = (qsat(:,:,k)-qin(:,:,k))/(1.+hlcp(:,:)*dqsat(:,:,k))
            def(:,:) = min(max(def(:,:),0.0),exq(:,:))
            qdel(:,:,k) = qdel(:,:,k) + def(:,:)
            tdel(:,:,k) = tdel(:,:,k) - def(:,:)*hlcp(:,:)
            exq(:,:) = (exq(:,:)-def(:,:))*pmass(:,:,k)
        endwhere

    enddo

!-----------------------------------------------------------------------

   end subroutine precip_evap

!#######################################################################

   subroutine lscale_cond_init (myThid)

!-----------------------------------------------------------------------
!
!        initialization for large scale condensation
!
!-----------------------------------------------------------------------

  integer, intent(in) :: myThid

!-----------------------------------------------------------------------

! integer  unit,io,ierr
  integer         :: iUnit
  CHARACTER*(gcm_LEN_MBUF) :: msgBuf


!----------- read namelist ---------------------------------------------

!    _BARRIER
!    _BEGIN_MASTER(myThid)
     CALL BARRIER(myThid)
     IF ( myThid.EQ.1 ) THEN

     WRITE(msgBuf,'(A)') 'LSCALE_COND_INIT: opening data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
     CALL OPEN_COPY_DATA_FILE(                                      &
                           'data.atm_gray', 'LSCALE_COND_INIT',     &
                           iUnit,                                   &
                           myThid )
!    Read parameters from open data file
     READ(UNIT=iUnit,NML=lscale_cond_nml)
     WRITE(msgBuf,'(A)')                                            &
          'LSCALE_COND_INIT: finished reading data.atm_gray'
     CALL PRINT_MESSAGE( msgBuf, gcm_stdMsgUnit, gcm_SQZ_R, myThid )
!    Close the open data file
     CLOSE(iUnit)

!      if (file_exist('input.nml')) then
!         unit = open_file (file='input.nml', action='read')
!         ierr=1; do while (ierr /= 0)
!            read  (unit, nml=lscale_cond_nml, iostat=io, end=10)
!            ierr = check_nml_error (io,'lscale_cond_nml')
!         enddo
!  10     call close_file (unit)
!      endif

!---------- output namelist --------------------------------------------

!      unit = open_file (file='logfile.out', action='append')
!      if ( mpp_pe() == 0 ) then
!           write (unit,'(/,80("="),/(a))') trim(version), trim(tag)
!           write (unit,nml=lscale_cond_nml)
!      endif
!      call close_file (unit)

      do_init=.false.

     ENDIF
     CALL BARRIER(myThid)

   end subroutine lscale_cond_init

!#######################################################################

end module lscale_cond_mod

