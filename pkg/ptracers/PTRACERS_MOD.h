#ifdef ALLOW_PTRACERS
#ifdef PTRACERS_ALLOW_DYN_STATE

!BOP
!    !ROUTINE: PTRACERS_MOD.h
!    !INTERFACE:
! #include PTRACERS_MOD.h

!    !DESCRIPTION:
! Contains passive tracer modules and associated macros

!EOP

      use ptracers_dyn_state_mod
      use ptracers_dyn_state_data_mod

! This macro allows the second-order-moment member of the ptracers
! internal state data structure to be used like an simple array.
! Names are chosen such that macro expansion does not increase
! line length
#define _Ptracers_som(a,b,c,d,e,f,g) \
        PtrISt(g)%som_P(a,b,c,d,e,f)

#endif /* PTRACERS_ALLOW_DYN_STATE */
#endif /* ALLOW_PTRACERS */

