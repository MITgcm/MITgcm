#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: GCHEM.h
C    !INTERFACE:

C    !DESCRIPTION:
c coptions for biogeochemistry package
#define PTRACERS_SEPARATE_FORCING
#define DIC_BIOTIC
#undef  ALLOW_CFC
#undef  ALLOW_FE

#endif
