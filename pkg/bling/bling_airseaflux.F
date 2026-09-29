#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_AIRSEAFLUX(
     I           PTR_DIC, PTR_ALK, PTR_O2, PTR_PO4,
#ifdef USE_SIBLING
     I           PTR_SI,
#endif
     O           SGDIC, SGO2, FluxO2,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_airseaflux
C     | o Calculate the carbon and oxygen air-sea flux terms
C     |   Adapted from pkg/dic/dic_surfforcing.F
C     | - Get atmospheric pCO2 value
C     |   Option 1: constant value, default 268.d-6, can be changed
C     |             in data.bling
C     |   Option 2: read 2D field using EXF pkg
C     | - Update pCO2 and pH
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_EXF
# include "EXF_INTERP_SIZE.h"
#endif
#include "BLING_VARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime           :: current time
C     myIter           :: current timestep
C     myThid           :: thread Id. number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax, bi, bj
C     === Input ===
C     PTR_DIC          :: DIC tracer field
C     PTR_ALK          :: alkalinity tracer field
C     PTR_PO4          :: phosphate tracer field
C     PTR_O2           :: oxygen tracer field
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL  PTR_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
C     === Output ===
C     SGDIC            :: surface tendency of DIC due to air-sea exchange
C     SGO2             :: surface tendency of O2 due to air-sea exchange
C     FluxO2           :: air-sea flux of O2
C     (FluxCO2 is a global variable)
      _RL  SGDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SGO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  FluxO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_PTRACERS

C     === Local variables ===
C     i,j              :: Loop counters
      INTEGER i,j,klev
C Number of iterations for pCO2 solvers
      _RL co3dummy
      _RL Kwexch_Pre   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C Solubility relation coefficients
      _RL SchmidtNoDIC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pCO2sat      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pisvel       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C local variables for carbon chem
      _RL surfalk      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfphos     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsi       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surftemp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsalt     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfdic      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C o2 solubility relation coefficients
      _RL SchmidtNoO2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL O2sat        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL O2sat_percent(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch_o2    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL aTT
      _RL aTK
      _RL aTS
      _RL aTS2
      _RL aTS3
      _RL aTS4
      _RL aTS5
      _RL o2s
      _RL ttemp
      _RL stemp
      _RL oCnew
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
      INTEGER tkey
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif

C----------------------------------------------------------------------
C First, carbon
C----------------------------------------------------------------------
        klev=1
C determine inorganic carbon chem coefficients
        DO j=jmin,jmax
         DO i=imin,imax

             surfalk(i,j)  = PTR_ALK(i,j,1)
     &                          * maskC(i,j,1,bi,bj)
             surfphos(i,j) = PTR_PO4(i,j,1)
     &                          * maskC(i,j,1,bi,bj)

C FOR NON-INTERACTIVE Si
             surftemp(i,j) = theta(i,j,1,bi,bj)
             surfsalt(i,j) = salt(i,j,1,bi,bj)
             surfdic(i,j)  = PTR_DIC(i,j,1)
#ifdef USE_SIBLING
             surfsi(i,j)   = PTR_SI(i,j,1)
#else
             surfsi(i,j)   = silica(i,j,bi,bj) * maskC(i,j,1,bi,bj)
#endif

          ENDDO
         ENDDO

#ifdef CARBONCHEM_SOLVESAPHE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CARBON_COEFFS_SOLVESAPHE',myThid)
#endif
        CALL DIC_COEFFS_SURF(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
#else
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CARBON_COEFFS',myThid)
#endif
         CALL CARBON_COEFFS(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
#endif

       DO j=jmin,jmax
        DO i=imin,imax

C Pre-compute part of exchange coefficient: pisvel*(1-fice)
C which is re-used for flux of O2
C Schmidt number is accounted for later

              pisvel(i,j) = 0.337 _d 0 * wind(i,j,bi,bj)**2/3.6 _d 5

              Kwexch_Pre(i,j) = pisvel(i,j)* (1. _d 0 - FIce(i,j,bi,bj))

        ENDDO
       ENDDO

c pCO2 solver...

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ph(:,:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
#endif

C$TAF LOOP = parallel
       DO j=jmin,jmax
C$TAF LOOP = parallel
        DO i=imin,imax

          IF ( maskC(i,j,klev,bi,bj).NE.0. _d 0 ) THEN
#ifdef CARBONCHEM_SOLVESAPHE
             IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to calculate pH and pCO2
#ifdef ALLOW_DEBUG
                IF (debugMode) CALL DEBUG_CALL(
     &     'CALC_PCO2_SOLVESAPHE from DIC_SURFFORCING',myThid)
#endif
                CALL CALC_PCO2_SOLVESAPHE(
     I        surftemp(i,j),surfsalt(i,j),
     I        surfdic(i,j), surfphos(i,j),
     I        surfsi(i,j),surfalk(i,j),
     U        pH(i,j,klev,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I        i,j,klev,bi,bj,myIter, myThid )
             ELSE
C Use the original Follows et al. (2006) solver
#endif /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
                IF (debugMode) CALL DEBUG_CALL(
     &        'CALC_PCO2_APPROX',myThid)
#endif

            CALL CALC_PCO2_APPROX(
     I        surftemp(i,j),surfsalt(i,j),
     I        surfdic(i,j), surfphos(i,j),
     I        surfsi(i,j),surfalk(i,j),
     I        ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I        ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I        aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I        aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I        ak0(i,j,bi,bj), fugf(i,j,bi,bj),
     I        ff(i,j,bi,bj),
     I        bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U        pH(i,j,klev,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I        i,j,klev,bi,bj,myIter,myThid )

#ifdef CARBONCHEM_SOLVESAPHE
             ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */

          ELSE
            pCO2(i,j,bi,bj) = 0. _d 0
          ENDIF

        ENDDO
       ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE pCO2(:,:,bi,bj) = comlev1_bibj, key = tkey, kind = isbyte
#endif

       DO j=jmin,jmax
        DO i=imin,imax

          IF ( maskC(i,j,1,bi,bj).NE.0. _d 0 ) THEN
C calculate SCHMIDT NO. for CO2
              SchmidtNoDIC(i,j) =
     &            sca1
     &          + sca2 * theta(i,j,1,bi,bj)
     &          + sca3 * theta(i,j,1,bi,bj)*theta(i,j,1,bi,bj)
     &          + sca4 * theta(i,j,1,bi,bj)*theta(i,j,1,bi,bj)
     &                *theta(i,j,1,bi,bj)
c make sure Schmidt number is not negative (will happen if temp>39C)
             SchmidtNoDIC(i,j) = max(1.0 _d -2, SchmidtNoDIC(i,j))

C First determine local saturation pCO2
c Correct for atmospheric pressure
              pCO2sat(i,j) = apco2(i,j,bi,bj) * AtmosP(i,j,bi,bj)

C then account for Schmidt number
              Kwexch(i,j) = Kwexch_Pre(i,j)
     &                    / sqrt(SchmidtNoDIC(i,j)/660.0 _d 0)

C Calculate flux in terms of DIC units using K0, solubility
c Flux = kw*rho*(ff*pCO2atm-k0*FugFac*pCO2ocean)
               FluxCO2(i,j,bi,bj) =
     &          Kwexch(i,j)*(
     &            ff(i,j,bi,bj)*pCO2sat(i,j) -
     &            pCO2(i,j,bi,bj)*fugf(i,j,bi,bj)
     &            *ak0(i,j,bi,bj) )
     &
          ELSE
              FluxCO2(i,j,bi,bj) = 0. _d 0
          ENDIF

C convert flux (mol kg-1 m s-1) to (mol m-2 s-1)
          FluxCO2(i,j,bi,bj) = FluxCO2(i,j,bi,bj)/permil

          ENDDO
         ENDDO

C update tendency
         DO j=jmin,jmax
          DO i=imin,imax
           SGDIC(i,j)= recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
     &              *FluxCO2(i,j,bi,bj)
          ENDDO
         ENDDO

C----------------------------------------------------------------------
C Now oxygen
C----------------------------------------------------------------------

C calculate SCHMIDT NO. for O2
        DO j=jmin,jmax
          DO i=imin,imax
            IF (maskC(i,j,1,bi,bj).NE.0.) THEN
              ttemp = theta(i,j,1,bi,bj)
              stemp = salt(i,j,1,bi,bj)

              SchmidtNoO2(i,j) =
     &            sox1
     &          + sox2 * ttemp
     &          + sox3 * ttemp*ttemp
     &          + sox4 * ttemp*ttemp*ttemp

C Determine surface flux of O2
C exchange coeff accounting for ice cover and Schmidt no.
C Kwexch_Pre= pisvel*(1-fice): previously computed above

              Kwexch_o2(i,j) = Kwexch_Pre(i,j)
     &                    / sqrt(SchmidtNoO2(i,j)/660.0 _d 0)

C determine saturation O2
C using Garcia and Gordon (1992), L&O (mistake in original ?)
              aTT  = 298.15 _d 0 -ttemp
              aTK  = 273.15 _d 0 +ttemp
              aTS  = log(aTT/aTK)
              aTS2 = aTS*aTS
              aTS3 = aTS2*aTS
              aTS4 = aTS3*aTS
              aTS5 = aTS4*aTS

              oCnew  = oA0 + oA1*aTS + oA2*aTS2 + oA3*aTS3 +
     &            oA4*aTS4 + oA5*aTS5
     &          + stemp*(oB0 + oB1*aTS + oB2*aTS2 + oB3*aTS3)
     &          + oC0*(stemp*stemp)

              o2s = EXP(oCnew)

c Convert from ml/l to mol/m^3
              O2sat(i,j) = o2s/22391.6 _d 0 * 1. _d 3
c Calculate percent saturation for diagnostic
              O2sat_percent(i,j) = PTR_O2(i,j,1)/O2sat(i,j)*100

C Determine flux, incl. correction for local atmos surface pressure
              FluxO2(i,j) = Kwexch_o2(i,j)*
     &                     (AtmosP(i,j,bi,bj)*O2sat(i,j)
     &                      - PTR_O2(i,j,1))
            ELSE
              FluxO2(i,j) = 0. _d 0
            ENDIF

          ENDDO
        ENDDO

C update surface tendencies
        DO j=jmin,jmax
          DO i=imin,imax
           SGO2(i,j)= FluxO2(i,j)
     &         *recip_drF(1) * recip_hFacC(i,j,1,bi,bj)
          ENDDO
        ENDDO

        _EXCH_XY_RL( pCO2, myThid )
        _EXCH_XYZ_RL( pH, myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(O2sat_percent,'BLGO2SAT',0,1,2,bi,bj,
     &       myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_PTRACER */

        RETURN
        END
