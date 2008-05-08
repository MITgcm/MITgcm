C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_MOD.h,v 1.1 2008/05/08 19:50:08 jahn Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
#ifdef PTRACERS_ALLOW_DYN_STATE

CBOP
C    !ROUTINE: PTRACERS_MOD.h
C    !INTERFACE:
C #include PTRACERS_MOD.h

C    !DESCRIPTION:
C Contains passive tracer modules and associated macros

CEOP

      use ptracers_dyn_state_mod
      use ptracers_dyn_state_data_mod

#ifdef GAD_ALLOW_SOM_ADVECT
C     This macro allows the second-order-moment member of the ptracers
C     internal state data structure to be used like an simple array.
C     Names are chosen such that macro expansion does not increase 
C     line length
#define _Ptracers_som(a,b,c,d,e,f,g) \
	PtrISt(g)%som_P(a,b,c,d,e,f)
#endif /* GAD_ALLOW_SOM_ADVECT */

#endif /* PTRACERS_ALLOW_DYN_STATE */
#endif /* ALLOW_PTRACERS */

