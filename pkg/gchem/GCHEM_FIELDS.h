#ifdef ALLOW_GCHEM
CBOP
C    !ROUTINE: GCHEM_FIELDS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains tracer fields specifically for chemical tracers.
C
C  gchemTendency :: 3DxPTRACER_num field that store the tendencies due
C                   to the bio-geochemical model

CEOP
#ifdef GCHEM_ADD2TR_TENDENCY
      _RL gchemTendency(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                  GCHEM_tendTr_num)
      COMMON /GCHEM_TENDENCY_R/
     &     gchemTendency
#endif /* GCHEM_ADD2TR_TENDENCY */

      COMMON /GCHEM_FORCING_R/
     &    gchemSi,
     &    gchemPAR,
     &    gchemFe,
     &    gchemapCO2,
     &    gchemIce,
     &    gchemWind,
     &    gchemAtmosP

      _RL gchemSi    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemPAR   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemFe    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemapCO2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemWind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemAtmosP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     From here on, these fields are only needed in gchem_fields_load.F
C     and we could put them into a separate header file (or, more
C     radically, directly into gchem_fields_load.F), thereby hiding
C     these fields from everyone else.

C--   COMMON /GCHEM_LOAD_I/
      COMMON /GCHEM_LOAD_I/ GCHEM_ldRec
      INTEGER GCHEM_ldRec(nSx,nSy)

C--   COMMON /GCHEM_AUX_FIELDS_R/
      COMMON /GCHEM_AUX_FIELDS_R/
     &    gchemSi0, gchemSi1,
     &    gchemPAR0, gchemPAR1,
     &    gchemIron0, gchemIron1,
     &    gchemApCO20, gchemApCO21,
     &    gchemIce0, gchemIce1,
     &    gchemWind0, gchemWind1,
     &    gchemAP0, gchemAP1

      _RL gchemSi0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemSi1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemPAR0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemPAR1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemIron0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemIron1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemApCO20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemApCO21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemIce0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemIce1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemWind0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemWind1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemAP0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemAP1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_GCHEM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
