C $Header: /u/gcmpack/MITgcm/pkg/gchem/GCHEM_FIELDS.h,v 1.1 2004/11/28 23:48:31 mlosch Exp $
C $Name:  $

#ifdef ALLOW_GCHEM
CBOP
C    !ROUTINE: GCHEM_FIELDS.h
C    !INTERFACE:
 
C    !DESCRIPTION:
C Contains tracer fields specifically for chemical tracers.
C
C  gchemTendency :: 3DxPTRACER_num field that store the tendencies due
C                   to the bio-geochemical model

#ifndef GCHEM_SEPARATE_FORCING
      _RL gchemTendency(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                  PTRACERS_num)
      COMMON /GCHEM_FIELDS/ 
     &     gchemTendency
#endif /* GCHEM_SEPARATE_FORCING */
CEOP
#endif /* ALLOW_GCHEM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
