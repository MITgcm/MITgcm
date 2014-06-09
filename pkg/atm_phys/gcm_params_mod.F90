! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/gcm_params_mod.F90,v 1.2 2014/06/09 12:51:00 jmc Exp $
! $Name:  $

module gcm_params_mod

!use fms_mod, only: write_version_number

implicit none
private

!  ---- public interfaces ----

real,    public :: gcm_unset_RS = 0.
real,    public :: gcm_unset_RL = 0.
integer, public :: gcm_unset_I  = 0
integer, public, parameter :: gcm_LEN_MBUF = 512
integer, public, parameter :: gcm_LEN_FNAM = 512
integer, public, parameter :: gcm_LEN_PREC = 200
character*1, public :: gcm_SQZ_R = ' '
character*1, public :: gcm_SQZ_L = ' '
character*1, public :: gcm_SQZ_B = ' '
integer, public :: gcm_precFloat32 = 0
integer, public :: gcm_precFloat64 = 0
integer, public :: gcm_debLev0  = 0
integer, public :: gcm_debLevA  = 0
integer, public :: gcm_debLevB  = 0
integer, public :: gcm_debLevC  = 0
integer, public :: gcm_debLevD  = 0
integer, public :: gcm_debLevE  = 0
integer, public :: gcm_stdMsgUnit = 0
integer, public :: gcm_errMsgUnit = 0
integer, public :: gcm_debugLevel = 0
logical, public :: gcm_debugMode = .FALSE.

public gcm_params_init

contains

! <SUBROUTINE NAME="gcm_params_init">

!   <OVERVIEW>
!     Initialization routine. The purpose of this routine
!     is initialize the GCM PARAMS module from EEPARAMS.h & PARAMS.h
!     commom bloc values which are passed as argument to this routine.
!   </OVERVIEW>
!   <DESCRIPTION>
!   </DESCRIPTION>
!   <TEMPLATE>
!     call gcm_params_init( ... )
!   </TEMPLATE>

subroutine gcm_params_init(                                          &
               inp_unset_RS, inp_unset_RL, inp_unset_I,              &
!              inp_LEN_MBUF, inp_LEN_FNAM, inp_LEN_PREC,             &
               inp_SQZ_R, inp_SQZ_L, inp_SQZ_B,                      &
               inp_precFloat32, inp_precFloat64,                     &
               inp_debLev0, inp_debLevA, inp_debLevB,                &
               inp_debLevC, inp_debLevD, inp_debLevE,                &
               inp_stdMsgUnit, inp_errMsgUnit,                       &
               inp_debugLevel, inp_debugMode,                        &
               myThid )

#ifdef LET_RS_BE_REAL4
  real*4,  intent(in) :: inp_unset_RS
#else
  real*8,  intent(in) :: inp_unset_RS
#endif
  real,    intent(in) :: inp_unset_RL
  integer, intent(in) :: inp_unset_I
! integer, intent(in) :: inp_LEN_MBUF
! integer, intent(in) :: inp_LEN_FNAM
! integer, intent(in) :: inp_LEN_PREC
  character*1, intent(in) :: inp_SQZ_R
  character*1, intent(in) :: inp_SQZ_L
  character*1, intent(in) :: inp_SQZ_B
  integer, intent(in) :: inp_precFloat32
  integer, intent(in) :: inp_precFloat64
  integer, intent(in) :: inp_debLev0
  integer, intent(in) :: inp_debLevA
  integer, intent(in) :: inp_debLevB
  integer, intent(in) :: inp_debLevC
  integer, intent(in) :: inp_debLevD
  integer, intent(in) :: inp_debLevE
  integer, intent(in) :: inp_stdMsgUnit
  integer, intent(in) :: inp_errMsgUnit
  integer, intent(in) :: inp_debugLevel
  logical, intent(in) :: inp_debugMode

  integer, intent(in) :: myThid

   gcm_unset_RS    = inp_unset_RS
   gcm_unset_RL    = inp_unset_RL
   gcm_unset_I     = inp_unset_I
!  gcm_LEN_MBUF    = inp_LEN_MBUF
!  gcm_LEN_FNAM    = inp_LEN_FNAM
!  gcm_LEN_PREC    = inp_LEN_PREC
   gcm_SQZ_R       = inp_SQZ_R
   gcm_SQZ_L       = inp_SQZ_L
   gcm_SQZ_B       = inp_SQZ_B
   gcm_precFloat32 = inp_precFloat32
   gcm_precFloat64 = inp_precFloat64
   gcm_debLev0     = inp_debLev0
   gcm_debLevA     = inp_debLevA
   gcm_debLevB     = inp_debLevB
   gcm_debLevB     = inp_debLevC
   gcm_debLevD     = inp_debLevD
   gcm_debLevE     = inp_debLevE
   gcm_stdMsgUnit  = inp_stdMsgUnit
   gcm_errMsgUnit  = inp_errMsgUnit
   gcm_debugLevel  = inp_debugLevel
   gcm_debugMode   = inp_debugMode

end subroutine gcm_params_init
! </SUBROUTINE>

!-----------------------------------------------------------------------

end module gcm_params_mod
