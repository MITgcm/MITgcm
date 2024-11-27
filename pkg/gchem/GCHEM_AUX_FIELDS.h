CBOP
C     !ROUTINE: GCHEM_AUX_FIELDS.h
C     !INTERFACE:

C     !DESCRIPTION:

C     Contains helper tracer fields specifically for time dependent
C     chemical tracers
C

CEOP

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
