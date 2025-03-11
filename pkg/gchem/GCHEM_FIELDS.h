CBOP
C    !ROUTINE: GCHEM_FIELDS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains tracer fields specifically for chemical tracers.
C
C  gchemTendency :: 3DxPTRACER_num field that store the tendencies due
C                   to the bio-geochemical model
C  gchemSi/PAR etc :: surface forcing fields provided by pkg/gchem

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
