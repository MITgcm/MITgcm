#ifdef ALLOW_PTRACERS

!BOP
!    !ROUTINE: PTRACERS_SIZE.h
!    !INTERFACE:
! #include PTRACERS_SIZE.h

!    !DESCRIPTION:
! Contains passive tracer array size (number of tracers).

! PTRACERS_num defines how many passive tracers are allocated/exist.
!  and is set here (default 1)
!
! Number of tracers
      INTEGER :: PTRACERS_num
      PARAMETER(PTRACERS_num = 1 )

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER :: maxpass
      PARAMETER( maxpass     = PTRACERS_num + 2 )
#endif

!EOP
#endif /* ALLOW_PTRACERS */

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
