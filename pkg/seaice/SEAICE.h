C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE.h,v 1.80 2014/12/01 12:29:06 mlosch Exp $
C $Name:  $

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
C     HSALT :: effective sea ice salinity in g/m^2
C              at center of grid, i.e., tracer point
C \ev
CEOP

C--   Grid variables for seaice
      COMMON/ARRAY/HEFFM
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_CGRID
      COMMON/ARRAYC/ seaiceMaskU, seaiceMaskV
      _RL seaiceMaskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMaskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     k1/2AtZ :: coefficients at C and Z points
C     k1/2AtC    for metric terms in U/V ice equations.
      COMMON/ARRAYCMETRIC/  k1AtC, k1AtZ, k2AtC, k2AtZ
      _RS k1AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      COMMON/ARRAYB/ UVM
      _RS UVM        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     k1/2AtC/U/V :: coefficients at C, U, and V points
C                    for metric terms in U/V ice equations.
      COMMON/ARRAYBMETRIC/
     &     k1AtC, k1AtU, k1AtV, k2AtC, k2AtU, k2AtV
      _RS k1AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k1AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS k2AtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_CGRID */

C--   Dynamical variables
      COMMON/SEAICE_DYNVARS_1/AREA,HEFF,HSNOW,UICE,VICE
#ifdef SEAICE_ITD
     &                       ,AREAITD,HEFFITD,HSNOWITD,
     &                        opnWtrFrac, fw2ObyRidge
      _RL AREAITD    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL HEFFITD    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL HSNOWITD   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
C     fraction of open water (= 1-AREA) needed for ridging parameterization
      _RL opnWtrFrac (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fw2ObyRidge(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_ITD */
      _RL AREA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HSNOW      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     uIceC :: average of UICE between last two time steps
C     vIceC :: average of VICE between last two time steps
      COMMON/SEAICE_DYNVARS_3/
     &     ETA,etaZ,ZETA,zetaZ,PRESS, e11, e22, e12, deltaC,
     &     FORCEX,FORCEY,
     &     uIceNm1, vIceNm1
      _RL ETA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZ       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ZETA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaZ      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     ice strength/pressure term
      _RL PRESS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     strain rate tensor
      _RL e11        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     deformation rate tensor invariant, for viscous plastic sea ice = 
C     sqrt[(e11**2+e22**2)*(1+1/e**2)+ 4./e**2*e12C**2 + 2*e11*e22*(1-1/e**2))
      _RL deltaC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C
      _RL FORCEX     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEY     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
C--   Dynamical variables
      COMMON/SEAICE_DYNVARS_COST/ AREAforAtmFW, frWtrAtm
      _RL AREAforAtmFW (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)   
      _RL frWtrAtm     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifndef SEAICE_CGRID
      COMMON/SEAICE_DYNVARS_BGRID/ AMASS, DAIRN, uIceC, vIceC
      _RL AMASS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DAIRN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      COMMON/SEAICE_DYNVARS_CGRID/
     &     seaiceMassC, seaiceMassU, seaiceMassV
      _RL seaiceMassC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMassU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaiceMassV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

      COMMON/SEAICE_DYNVARS_4/
     &     DWATN, PRESS0, FORCEX0, FORCEY0, ZMAX, ZMIN
      _RL DWATN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL PRESS0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEX0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL FORCEY0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ZMAX       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ZMIN       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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
      COMMON/SEAICE_SALINITY_R/HSALT, saltFluxAdjust
      _RL HSALT         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL saltFluxAdjust(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_VARIABLE_SALINITY */

C     saltWtrIce contains m of salty ice melted (<0) or created (>0)
C     frWtrIce contains m of freshwater ice melted (<0) or created (>0)
C              that is, ice due to precipitation or snow
C     frWtrAtm contains freshwater flux from the atmosphere
      COMMON/ICEFLUX/ saltWtrIce, frWtrIce
      _RL saltWtrIce (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL frWtrIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     TICES :: Seaice/snow surface temperature for each category
      COMMON/MULTICATEGORY/TICES
      _RL TICES      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)

#if (defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_FREEDRIFT))
      COMMON /SEAICE_FD_FIELDS/
     &     uice_fd, vice_fd
      _RL uice_fd   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vice_fd   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#if (defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_EVP))
C
C     additional fields needed by the EVP solver
C
C     seaice_sigma1  - sigma11+sigma22, defined at C-points
C     seaice_sigma2  - sigma11-sigma22, defined at C-points
C     seaice_sigma12 - off-diagonal term, defined at Z-points
      COMMON /SEAICE_EVP_FIELDS/
     &     seaice_sigma1, seaice_sigma2, seaice_sigma12
      _RL seaice_sigma1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_sigma2    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL seaice_sigma12   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_ALLOW_EVP and SEAICE_CGRID */

#ifdef SEAICE_CGRID
C     stressDivergenceX/Y - divergence of stress tensor
      COMMON /SEAICE_STRESSDIV/
     &     stressDivergenceX, stressDivergenceY
      _RL stressDivergenceX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL stressDivergenceY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* SEAICE_CGRID */

      COMMON/WIND_STRESS_ICE/TAUX,TAUY
C     TAUX   - zonal      wind stress over ice at U point
C     TAUY   - meridional wind stress over ice at V point
      _RL TAUX       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL TAUY       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifndef SEAICE_CGRID
      COMMON/WIND_STRESS_OCE/WINDX,WINDY
C     WINDX  - zonal      wind stress over water at C points
C     WINDY  - meridional wind stress over water at C points
      _RL WINDX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL WINDY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON/GWATXY/GWATX,GWATY
      _RL GWATX      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL GWATY      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   KGEO    Level used as a proxy for geostrophic velocity.
      COMMON/SEAICE_KGEO/KGEO
      INTEGER KGEO   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_SEAICE_COST_EXPORT
      _RL uHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icevolMeanCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /SEAICE_COST_EXPORT_R/
     &       uHeffExportCell, vHeffExportCell,
     &       icevolMeanCell
#endif

C     SWFracB :: fraction of surface Short-Wave radiation reaching
C                the bottom of ocean surface level
      _RL SWFracB
      COMMON /SEAICE_SW_R/
     &       SWFracB

#ifdef SEAICE_ALLOW_JFNK
C     diagnostics for the JFNK solver
      INTEGER totalNewtonIters
      INTEGER totalNewtonFails
      INTEGER totalKrylovIters
      INTEGER totalKrylovFails
      INTEGER totalJFNKtimeSteps
      COMMON /SEAICE_JFNK_I/
     &     totalNewtonIters, totalNewtonFails,
     &     totalKrylovIters, totalKrylovFails,
     &     totalJFNKtimeSteps
      INTEGER nVec
      PARAMETER ( nVec=2*sNx*sNy )
      _RL scalarProductMetric( nVec, 1, nSx, nSy )
      COMMON /SEAICE_JFNK_RL/ scalarProductMetric
#endif /* SEAICE_ALLOW_JFNK */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
