CBOP
C !ROUTINE: SEAICE.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SEAICE.h
C     | o Basic header for sea ice model.
C     |   Contains most sea ice field declarations.
C     *==========================================================*
C
C     UICE  :: zonal ice velocity in m/s at South-West B-grid
C              (or C-grid #ifdef SEAICE_CGRID) U point
C              >0 from West to East
C     VICE  :: meridional ice velocity in m/s at South-West B-grid
C              (or C-grid #ifdef SEAICE_CGRID) V point
C              >0 from South to North
C              note: the South-West B-grid U and V points are on
C                the lower, left-hand corner of each grid cell
C     AREA  :: fractional ice-covered area in m^2/m^2
C              at center of grid, i.e., tracer point
C              0 is no cover, 1 is 100% cover
C     HEFF  :: effective ice thickness in m
C              at center of grid, i.e., tracer point
C              note: for non-zero AREA, actual ice thickness is HEFF / AREA
C     HSNOW :: effective snow thickness in m
C              at center of grid, i.e., tracer point
C              note: for non-zero AREA, actual snow thickness is HSNOW / AREA
C \ev
CEOP

C--   Grid variables for seaice
C     static masks (depend only on geometry)
C     HEFFM     :: land-sea mask at C-points (copy of maskC(k=kSrf))
C     SIMaskU/V :: land-sea mask at U/V-points (copies of maskW/S(k=kSrf))
      COMMON/ARRAY/HEFFM, SIMaskU, SIMaskV
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskU    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIMaskV    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
      COMMON/ARRAYMETRIC/  k1AtC, k2AtC
      _RS k1AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef SEAICE_CGRID
      COMMON/ARRAYC/ seaiceMaskU, seaiceMaskV
C     dynamic masks (depend on area)
      _RL seaiceMaskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMaskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     k1/2AtZ :: coefficients at C and Z points
C     k1/2AtC    for metric terms in U/V ice equations.
      COMMON/ARRAYCMETRIC/  k1AtZ, k2AtZ
      _RS k1AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_CGRID */

#ifdef SEAICE_BGRID_DYNAMICS
C     UVM         :: B-grid velocity-point mask
      COMMON/ARRAYB/ UVM
      _RS UVM        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     k1/2AtC/U/V :: coefficients at C, U, and V points
C                    for metric terms in U/V ice equations.
      COMMON/ARRAYBMETRIC/ k1AtU, k1AtV, k2AtU, k2AtV
      _RS k1AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_BGRID_DYNAMICS */

C--   Dynamical variables
      COMMON/SEAICE_DYNVARS_1/
     &     AREA, HEFF, HSNOW, UICE, VICE
      _RL AREA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HSNOW      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     DWATN         :: (linear) ice-ocean drag coefficient
C                      ( units of [rho|u|] = kg/m^2/s )
C     u/vIceNm1     :: sea ice drift velocities of previous timestep (m/s)
      COMMON/SEAICE_DYNVARS_2/
     &     DWATN,
     &     uIceNm1, vIceNm1
      _RL DWATN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef SEAICE_ITD
      COMMON/SEAICE_DYNVARS_ITD/
     &     AREAITD, HEFFITD, HSNOWITD,
     &     opnWtrFrac, fw2ObyRidge
C     Fields for dynamic ice thickness distribution (ITD)
C     AREAITD     :: area classes
C     HEFFITD     :: ice thickenss classes (in meters)
C     HSNOWITD    :: snow thickness classes (in meters)
C     openWtrFrac :: fraction of open water (= 1-AREA) for ridging param.
C     fw2ObyRidge :: fresh water flux (kg/m^2/s) due to snow pushed into
C                    ocean during ridging
      _RL AREAITD    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL HEFFITD    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL HSNOWITD   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL opnWtrFrac (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fw2ObyRidge(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_ITD */

#ifdef SEAICE_CGRID
C     stressDivergenceX/Y :: div of (vert. integr.) stress tensor (N/m^2)
      COMMON /SEAICE_STRESSDIV/
     &     stressDivergenceX, stressDivergenceY
      _RL stressDivergenceX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL stressDivergenceY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# ifdef SEAICE_ALLOW_EVP
C--   Additional fields needed by the EVP solver:
C     (vertically integrated) stress tensor, with diagonal terms sigma11/22
C     seaice_sigma1  :: sigma11+sigma22, defined at C-points   (N/m)
C     seaice_sigma2  :: sigma11-sigma22, defined at C-points   (N/m)
C     seaice_sigma12 :: off-diagonal term, defined at Z-points (N/m)
      COMMON /SEAICE_EVP_FIELDS/
     &     seaice_sigma1, seaice_sigma2, seaice_sigma12
      _RL seaice_sigma1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_sigma2    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_sigma12   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif /* SEAICE_ALLOW_EVP */
#endif

#if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
C     ETA,  etaZ    :: shear viscosity as C-points, at Z-points (N s/m = kg/s)
C     ZETA, zetaA   :: bulk viscosity at C-points, at Z-points
C     PRESS         :: maximum vertically integrated ice strength/pressure (N/m)
C     e11, e22, e12 :: components strain rate tensor (1/s)
C     deltaC        :: deformation rate tensor invariant, for VP sea ice
C                      = sqrt( (e11+e22)**2 + (1/e)*(e11-e22)**2 + 4*e12**2) )
C     FORCEX/Y      :: momentum forcing
C                      ( units of [rho * h * u / deltaT] = kg/m/s^2 )
C     tensileStrFac :: factor k to compute the maximal tensile stress k*PRESS0
      COMMON/SEAICE_DYNVARS_3/
     &     ETA, etaZ, ZETA, zetaZ, PRESS, tensileStrFac,
     &     e11, e22, e12, deltaC,
     &     FORCEX,FORCEY

      _RL ETA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZ       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ZETA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     ice strength/pressure term
      _RL PRESS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tensileStrFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     strain rate tensor
      _RL e11        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL FORCEX     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEY     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     PRESS0        :: maximal compressive stress/strength (N/m)
C     FORCEX/Y0     :: external momentum forcing fields (part of FORCEX/Y)
C     SEAICE_zMax/zMin :: maximum/minimum bulk viscosities
      COMMON/SEAICE_DYNVARS_4/
     &     PRESS0, FORCEX0, FORCEY0, SEAICE_zMax, SEAICE_zMin
      _RL PRESS0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEX0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEY0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SEAICE_zMax(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SEAICE_zMin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef SEAICE_CGRID
C     seaiceMassC/U/V :: mass (ice+snow) at C/U/V-points ( kg/m^2 )
      COMMON/SEAICE_DYNVARS_CGRID/
     &     seaiceMassC, seaiceMassU, seaiceMassV
      _RL seaiceMassC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMassU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMassV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# ifdef SEAICE_ALLOW_FREEDRIFT
C     u/vice_fd :: free drift velocities (m/s)
      COMMON /SEAICE_FD_FIELDS/
     &     uice_fd, vice_fd
      _RL uice_fd   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vice_fd   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif

# ifdef SEAICE_ALLOW_BOTTOMDRAG
C     CbobC :: (linear) bottom drag coefficient for basals stress param.
      COMMON/SEAICE_BOTTOMDRAG/ CbotC
      _RL CbotC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif /* SEAICE_ALLOW_BOTTOMDRAG */

# if ( defined SEAICE_ALLOW_JFNK ||  defined SEAICE_ALLOW_KRYLOV )
C     diagnostics for the JFNK and Krylov solver
      INTEGER totalNewtonIters
      INTEGER totalNewtonFails
      INTEGER totalKrylovIters
      INTEGER totalKrylovFails
      INTEGER totalJFNKtimeSteps
      COMMON /SEAICE_SOLVER_I/
     &     totalNewtonIters, totalNewtonFails,
     &     totalKrylovIters, totalKrylovFails,
     &     totalJFNKtimeSteps
C     Scalar product used in FGMRES needs a metric
      INTEGER nVec
      PARAMETER ( nVec=2*sNx*sNy )
      _RL scalarProductMetric( nVec, 1, nSx, nSy )
      COMMON /SEAICE_KRYLOV_RL/ scalarProductMetric
# endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */

#endif /* SEAICE_CGRID */

#ifdef SEAICE_BGRID_DYNAMICS
C     AMASS :: sea ice mass
C     DAIRN :: (linear) atmosphere-ice drag coefficient
C     u/vIceC has been renamed to u/vIceC to avoid conficts with C-grid code
C     uIceB :: average of UICE between last two time steps
C     vIceB :: average of VICE between last two time steps
      COMMON/SEAICE_DYNVARS_BGRID/ AMASS, DAIRN, uIceB, vIceB
      _RL AMASS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DAIRN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceB      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceB      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON/WIND_STRESS_OCE/WINDX,WINDY
C     WINDX  - zonal      wind stress over water at C points
C     WINDY  - meridional wind stress over water at C points
      _RL WINDX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL WINDY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     GWATX/Y :: geostrophic ocean velocities
      COMMON/GWATXY/GWATX,GWATY
      _RL GWATX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL GWATY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   KGEO    Level used as a proxy for geostrophic velocity.
      COMMON/SEAICE_KGEO/KGEO
      INTEGER KGEO   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_BGRID_DYNAMICS */

      COMMON/SEAICE_REG_NEG/d_HEFFbyNEG,d_HSNWbyNEG
C     The change of mean ice thickness due to out-of-bounds values following
C     sea ice dynamics and advection
      _RL d_HEFFbyNEG (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL d_HSNWbyNEG (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
#ifdef EXF_SEAICE_FRACTION
      COMMON/SEAICE_RELAX/d_AREAbyRLX,d_HEFFbyRLX
C     ICE/SNOW stocks tendency associated with relaxation towards observation
      _RL d_AREAbyRLX (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     The change of mean ice thickness due to relaxation
      _RL d_HEFFbyRLX (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* EXF_SEAICE_FRACTION */

#ifdef SEAICE_VARIABLE_SALINITY
C     HSALT          :: effective sea ice salinity in g/m^2
C                       at center of grid, i.e., tracer point
C     saltFluxAdjust :: adjust salt flux, if HSALT < 0 (e.g. due to advection)
      COMMON/SEAICE_SALINITY_R/HSALT, saltFluxAdjust
      _RL HSALT         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL saltFluxAdjust(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_VARIABLE_SALINITY */

C     saltWtrIce :: contains m of salty ice melted (<0) or created (>0)
C     frWtrIce   :: contains m of freshwater ice melted (<0) or created (>0)
C                   that is, ice due to precipitation or snow
      COMMON/ICEFLUX/ saltWtrIce, frWtrIce
      _RL saltWtrIce (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL frWtrIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     TICES :: Seaice/snow surface temperature for each category
      COMMON/MULTICATEGORY/TICES
      _RL TICES      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
