#include "DIC_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: DIC_SURFFORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SURFFORCING( PTR_CO2 , PTR_ALK, PTR_PO4, GDC,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter,myTime,myThid)

C !DESCRIPTION:
C  Calculate the carbon air-sea flux terms
C  following external_forcing_dic.F (OCMIP run) from Mick

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_VARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
c  PTR_CO2              :: DIC tracer field
      INTEGER myIter, myThid
      _RL myTime
      _RL  PTR_CO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

C !OUTPUT PARAMETERS: ===================================================
c GDC                   :: tendency due to air-sea exchange
      _RL  GDC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_DIC

C !LOCAL VARIABLES: ====================================================
       INTEGER i,j, kLev
       LOGICAL debugPrt
       _RL co3dummy
C Number of iterations for pCO2 solvers...
C Solubility relation coefficients
      _RL SchmidtNoDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pCO2sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pisvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C local variables for carbon chem
      _RL surfalk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfphos(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsi(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surftemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfdic(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_OLD_VIRTUALFLUX
      _RL VirtualFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey_dic
#endif
CEOP

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      kLev=1

cc if coupled to atmsopheric model, use the
cc Co2 value passed from the coupler
c#ifndef USE_ATMOSCO2
cC PRE-INDUSTRIAL STEADY STATE pCO2 = 278.0 ppmv
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c           AtmospCO2(i,j,bi,bj)=278.0 _d -6
c        ENDDO
c       ENDDO
c#endif

C =================================================================
C determine inorganic carbon chem coefficients
        DO j=jMin,jMax
         DO i=iMin,iMax

#ifdef DIC_BIOTIC
cQQQQ check ptracer numbers
#ifdef DIC_BOUNDS
             surfalk(i,j) = max(0.4 _d 0,
     &                          min(10. _d 0,PTR_ALK(i,j,klev)))
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = max(1.0 _d -11,
     &                          min(1. _d -1,PTR_PO4(i,j,klev)))
     &                          * maskC(i,j,kLev,bi,bj)
#else
             surfalk(i,j) = PTR_ALK(i,j,klev)
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = PTR_PO4(i,j,klev)
     &                          * maskC(i,j,kLev,bi,bj)
#endif
#else /* DIC_BIOTIC */
             surfalk(i,j) = 2.366595 _d 0 * salt(i,j,kLev,bi,bj)/gsm_s
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = 5.1225 _d -4 * maskC(i,j,kLev,bi,bj)
#endif /* DIC_BIOTIC */
C FOR NON-INTERACTIVE Si
             surfsi(i,j)   = SILICA(i,j,bi,bj) * maskC(i,j,kLev,bi,bj)
#ifdef DIC_BOUNDS
            surftemp(i,j) = max(-4. _d 0,
     &                          min(50. _d 0, theta(i,j,kLev,bi,bj)))
            surfsalt(i,j) = max(4. _d 0,
     &                          min(50. _d 0, salt(i,j,kLev,bi,bj)))
            surfdic(i,j)  = max(0.4 _d 0,
     &                          min(10. _d 0, PTR_CO2(i,j,kLev)))
#else
            surftemp(i,j) = theta(i,j,kLev,bi,bj)
            surfsalt(i,j) = salt(i,j,kLev,bi,bj)
            surfdic(i,j)  = PTR_CO2(i,j,kLev)
#endif
          ENDDO
         ENDDO

#ifdef CARBONCHEM_SOLVESAPHE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DIC_COEFFS_SURF',myThid)
#endif
        CALL DIC_COEFFS_SURF(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
#else /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CARBON_COEFFS',myThid)
#endif
        CALL CARBON_COEFFS(
     I                       surftemp,surfsalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
#endif /* CARBONCHEM_SOLVESAPHE */
C====================================================================

       DO j=jMin,jMax
        DO i=iMin,iMax
C Compute AtmosP and Kwexch_Pre which are re-used for flux of O2

#ifdef USE_PLOAD
C Convert anomalous pressure pLoad (in Pa) from atmospheric model
C to total pressure (in Atm)
C Note: it is assumed the reference atmospheric pressure is 1Atm=1013mb
C       rather than the actual ref. pressure from Atm. model so that on
C       average AtmosP is about 1 Atm.
          AtmosP(i,j,bi,bj)= ( surf_pRef + pLoad(i,j,bi,bj) )/Pa2Atm
#endif

C Pre-compute part of exchange coefficient: pisvel*(1-fice)
C Schmidt number is accounted for later
              pisvel(i,j)=0.337 _d 0 *wind(i,j,bi,bj)**2/3.6 _d 5
              Kwexch_Pre(i,j,bi,bj) = pisvel(i,j)
     &                              * (1. _d 0 - FIce(i,j,bi,bj))

        ENDDO
       ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        ikey_dic = (act1 + 1) + act2*max1
     &                        + act3*max1*max2
     &                        + act4*max1*max2*max3
CADJ STORE pH(:,:,bi,bj) = comlev1_bibj, key = ikey_dic, kind = isbyte
#endif
       debugPrt = debugMode
c pCO2 solver...
C$TAF LOOP = parallel
       DO j=jMin,jMax
C$TAF LOOP = parallel
        DO i=iMin,iMax
          IF ( maskC(i,j,kLev,bi,bj).NE.0. _d 0 ) THEN
#ifdef CARBONCHEM_SOLVESAPHE
            IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to calculate pH and pCO2
#ifdef ALLOW_DEBUG
              IF (debugPrt) CALL DEBUG_CALL(
     &     'CALC_PCO2_SOLVESAPHE from DIC_SURFFORCING',myThid)
#endif
              CALL CALC_PCO2_SOLVESAPHE(
     I          surftemp(i,j),surfsalt(i,j),
     I          surfdic(i,j), surfphos(i,j),
     I          surfsi(i,j),surfalk(i,j),
     U          pH(i,j,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I          i,j,kLev,bi,bj, debugPrt, myIter, myThid )
              debugPrt = .FALSE.
            ELSE
C Use the original Follows et al. (2006) solver
#endif /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
              IF (debugPrt) CALL DEBUG_CALL(
     &        'CALC_PCO2_APPROX',myThid)
              debugPrt = .FALSE.
#endif
              CALL CALC_PCO2_APPROX(
     I          surftemp(i,j),surfsalt(i,j),
     I          surfdic(i,j), surfphos(i,j),
     I          surfsi(i,j),surfalk(i,j),
     I          ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I          ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I          aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I          aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I          ak0(i,j,bi,bj), fugf(i,j,bi,bj),
     I          ff(i,j,bi,bj),
     I          bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U          pH(i,j,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I          i,j,kLev,bi,bj,myIter, myThid )
#ifdef CARBONCHEM_SOLVESAPHE
            ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */
          ELSE
            pCO2(i,j,bi,bj)=0. _d 0
          ENDIF
        ENDDO
       ENDDO

       DO j=jMin,jMax
        DO i=iMin,iMax

          IF ( maskC(i,j,kLev,bi,bj).NE.0. _d 0 ) THEN
C calculate SCHMIDT NO. for CO2
              SchmidtNoDIC(i,j) =
     &            sca1
     &          + sca2 * theta(i,j,kLev,bi,bj)
     &          + sca3 * theta(i,j,kLev,bi,bj)*theta(i,j,kLev,bi,bj)
     &          + sca4 * theta(i,j,kLev,bi,bj)*theta(i,j,kLev,bi,bj)
     &                *theta(i,j,kLev,bi,bj)
c make sure Schmidt number is not negative (will happen if temp>39C)
             SchmidtNoDIC(i,j)=max(1.0 _d -2, SchmidtNoDIC(i,j))

C Determine surface flux (FDIC)
C first correct pCO2at for surface atmos pressure
              pCO2sat(i,j) =
     &          AtmosP(i,j,bi,bj)*AtmospCO2(i,j,bi,bj)

C then account for Schmidt number
              Kwexch(i,j) = Kwexch_Pre(i,j,bi,bj)
     &                    / sqrt(SchmidtNoDIC(i,j)/660.0 _d 0)

#ifdef WATERVAP_BUG
C Calculate flux in terms of DIC units using K0, solubility
C Flux = Vp * ([CO2sat] - [CO2])
C CO2sat = K0*pCO2atmos*P/P0
C Converting pCO2 to [CO2] using ff, as in CALC_PCO2
              FluxCO2(i,j,bi,bj) =
     &         Kwexch(i,j)*(
     &         ak0(i,j,bi,bj)*pCO2sat(i,j) -
     &         ff(i,j,bi,bj)*pCO2(i,j,bi,bj)
     &         )
#else
C Corrected by Val Bennington Nov 2010 per G.A. McKinley s finding
C of error in application of water vapor correction
c Flux = kw*rho*(ff*pCO2atm-k0*FugFac*pCO2ocean)
               FluxCO2(i,j,bi,bj) =
     &          Kwexch(i,j)*(
     &            ff(i,j,bi,bj)*pCO2sat(i,j) -
     &            pCO2(i,j,bi,bj)*fugf(i,j,bi,bj)
     &            *ak0(i,j,bi,bj) )

#endif
          ELSE
              FluxCO2(i,j,bi,bj) = 0. _d 0
          ENDIF
C convert flux (mol kg-1 m s-1) to (mol m-2 s-1)
            FluxCO2(i,j,bi,bj) = FluxCO2(i,j,bi,bj)/permil

#ifdef ALLOW_OLD_VIRTUALFLUX
            IF (maskC(i,j,kLev,bi,bj).NE.0. _d 0) THEN
c calculate virtual flux
c EminusPforV = dS/dt*(1/Sglob)
C NOTE: Be very careful with signs here!
C Positive EminusPforV => loss of water to atmos and increase
C in salinity. Thus, also increase in other surface tracers
C (i.e. positive virtual flux into surface layer)
C ...so here, VirtualFLux = dC/dt!
              VirtualFlux(i,j)=gsm_DIC*surfaceForcingS(i,j,bi,bj)/gsm_s
c OR
c let virtual flux be zero
c              VirtualFlux(i,j)=0.d0
c
            ELSE
              VirtualFlux(i,j)=0. _d 0
            ENDIF
#endif /* ALLOW_OLD_VIRTUALFLUX */
          ENDDO
         ENDDO

C update tendency
         DO j=jMin,jMax
          DO i=iMin,iMax
           GDC(i,j)= recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
     &              *(FluxCO2(i,j,bi,bj)
#ifdef ALLOW_OLD_VIRTUALFLUX
     &              + VirtualFlux(i,j)
#endif
     &               )
          ENDDO
         ENDDO

#endif /* ALLOW_DIC */
        RETURN
        END
