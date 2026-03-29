#ifdef ALLOW_GCHEM

CBOP
C    !ROUTINE: GCHEM_SIZE.h
C    !INTERFACE:
C #include GCHEM_SIZE.h

C    !DESCRIPTION:
C Contains GCHEM tracer array size (number of tracers).
C  Need to be included anywhere GCHEM_FIELDS.h is included

C GCHEM_tendTr_num :: defines how many GCHEM tendency tracers are allocated.
      INTEGER GCHEM_tendTr_num
C  Note: 1) tracers that use GCHEM_SEPARATE_FORCING are not counted here
C           since they do not use gchemTendency array.
C        2) if several tendency-tracer pkgs are compiled with no intention to
C           be used all together, a smaller number could be specified like here:
c     PARAMETER( GCHEM_tendTr_num = 3 )
#ifdef ALLOW_CFC
# include "CFC_SIZE.h"	
#else /* ALLOW_CFC */
      INTEGER CFC_Tr_num
      PARAMETER( CFC_Tr_num = 0 )
#endif /* ALLOW_CFC */
#ifdef ALLOW_SPOIL
# include "SPOIL_SIZE.h"	
#else /* ALLOW_SPOIL */
      INTEGER SPOIL_Tr_num
      PARAMETER( SPOIL_Tr_num = 0 )
#endif /* ALLOW_SPOIL */
      PARAMETER( GCHEM_tendTr_num = CFC_Tr_num + SPOIL_Tr_num )
CEOP
#endif /* ALLOW_GCHEM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
