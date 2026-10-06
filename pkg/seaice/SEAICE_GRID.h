CBOP
C !ROUTINE: SEAICE_GRID.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SEAICE_GRID.h
C     | o header that contains grid parameters specific to
C     |   pkg/seaice
C     *==========================================================*

C--   Grid variables for seaice
C     static masks (depend only on geometry)
C     HEFFM     :: land-sea mask at C-points (copy of maskC(k=kSrf))
C     SIMaskU/V :: land-sea mask at U/V-points (copies of maskW/S(k=kSrf))
      COMMON/ARRAY/HEFFM, SIMaskU, SIMaskV
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskU    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskV    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef SEAICE_CGRID
      COMMON/ARRAYC/ seaiceMaskU, seaiceMaskV
C     dynamic masks (depend on area)
      _RL seaiceMaskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMaskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_CGRID */

#ifdef SEAICE_BGRID_DYNAMICS
C     UVM         :: B-grid velocity-point mask
      COMMON/ARRAYB/ UVM
      _RS UVM        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_BGRID_DYNAMICS */

#if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
C     k1/2AtC/U/V :: coefficients at C, U, and V points
C                    for metric terms in U/V ice equations.
      COMMON /ARRAYMETRIC/ k1AtC, k2AtC, k1AtU, k1AtV, k2AtU, k2AtV
      _RS k1AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef SEAICE_CGRID
C     k1/2AtZ :: coefficients at C and Z points
      COMMON /ARRAYCMETRIC/ k1AtZ, k2AtZ
      _RS k1AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_CGRID */
