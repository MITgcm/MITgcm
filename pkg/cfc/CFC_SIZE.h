#ifndef CFC_SIZE_H
#define CFC_SIZE_H

CBOP
C    !ROUTINE: CFC_SIZE.h
C    !INTERFACE:
C #include CFC_SIZE.h

C    !DESCRIPTION:
C Contains CFC tracer array size (number of tracers).

C CFC_Tr_num   :: defines how many CFC tracer are allocated.
      INTEGER CFC_Tr_num
      PARAMETER( CFC_Tr_num = 2 )

C CFC_pTr_i1   :: pTracer index of first CFC tracer
      COMMON /CFC_INDEX/ CFC_pTr_i1
      INTEGER CFC_pTr_i1
CEOP
#endif /* CFC_SIZE_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
