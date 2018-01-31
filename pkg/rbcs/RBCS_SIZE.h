#ifdef ALLOW_RBCS

CBOP
C    !ROUTINE: RBCS_SIZE.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains RBCS array size (number of tracer mask)
CEOP

C---  RBCS Parameters:
C     maskLEN :: number of mask to read
      INTEGER maskLEN
      PARAMETER( maskLEN = 3 )

#endif /* ALLOW_RBCS */
