C $Header: /u/gcmpack/MITgcm/verification/cfc_offline/code/Attic/PTRACERS_SIZE.h,v 1.1 2005/05/05 17:47:12 stephd Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: PTRACERS_SIZE.h
C    !INTERFACE:
C #include PTRACERS_SIZE.h
 
C    !DESCRIPTION:
C Contains passive tracer array size (number of tracers).

C PTRACERS_num defines how many passive tracers are allocated/exist.
C  and is set here (default 1)
C
C     Number of tracers
      INTEGER PTRACERS_num
      PARAMETER(PTRACERS_num = 2 )

CEOP
#endif /* ALLOW_PTRACERS */
