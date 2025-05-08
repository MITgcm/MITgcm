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
     &    gchemFe0, gchemFe1,
     &    gchemApco20, gchemApco21,
     &    gchemIce0, gchemIce1,
     &    gchemWind0, gchemWind1,
     &    gchemApres0, gchemApres1

      _RL gchemSi0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemSi1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemPAR0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemPAR1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemFe0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemFe1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemApco20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemApco21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemIce0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemIce1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemWind0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemWind1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL gchemApres0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gchemApres1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
