#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: GCHEM.h
C    !INTERFACE:

C    !DESCRIPTION:
c coptions for biogeochemistry package
#define PTRACERS_SEPERATE_FORCING
#define DIC_BIOTIC
#define ALLOW_FE
#ifdef DIC_BIOTIC
#define DIC_ABIOTIC
#endif

#define ALLOW_DIC_COST

#endif
