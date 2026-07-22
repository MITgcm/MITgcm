#ifdef ALLOW_PTRACERS

!BOP
!    !ROUTINE: PTRACERS_FIELDS.h
!    !INTERFACE:
! #include PTRACERS_FIELDS.h

!    !DESCRIPTION:
! Contains passive tracer fields

!EOP

! COMMON /PTRACERS_FIELDS/
! pTracer  :: passive tracer concentration (tr per unit volume).
! gpTrNm1  :: work-space for time-stepping
! surfaceForcingPTr :: passive tracer surface forcing
      _RL  pTracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,                       &
     &      PTRACERS_num)
      _RL  gpTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,                       &
     &      PTRACERS_num)
      _RL  surfaceForcingPTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,                &
     &      PTRACERS_num)
      COMMON /PTRACERS_FIELDS/                                                    &
     &      pTracer, gpTrNm1, surfaceForcingPTr

      _RL totSurfCorPTr(PTRACERS_num)
      _RL meanSurfCorPTr(PTRACERS_num)
      COMMON /PTRACERS_SURFCOR_FIELDS/ totSurfCorPTr, meanSurfCorPTr

#endif /* ALLOW_PTRACERS */

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
