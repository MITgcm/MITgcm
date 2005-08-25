C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_surfforcing.F,v 1.9 2005/08/25 22:32:44 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: DIC_SURFFORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SURFFORCING( PTR_CO2 , GDC, 
     I           bi,bj,imin,imax,jmin,jmax,
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
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
c  PTR_CO2              :: DIC tracer field
      INTEGER myIter, myThid
      _RL myTime
      _RL  PTR_CO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

C !OUTPUT PARAMETERS: ===================================================
c GDC                   :: tendency due to air-sea exchange
      _RL  GDC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
       INTEGER I,J, kLev, it
C Number of iterations for pCO2 solvers...
C Solubility relation coefficients
      _RL SchmidtNoDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pCO2sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C local variables for carbon chem
      _RL surfalk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfphos(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsi(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VirtualFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      kLev=1

C PRE-INDUSTRIAL STEADY STATE pCO2 = 278.0 ppmv
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
           AtmospCO2(i,j,bi,bj)=278.0d-6
        ENDDO
       ENDDO


C =================================================================
C determine inorganic carbon chem coefficients
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx

#ifdef DIC_BIOTIC
cQQQQ check ptracer numbers
             surfalk(i,j) = PTRACER(i,j,klev,bi,bj,2)
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = PTRACER(i,j,klev,bi,bj,3)
     &                          * maskC(i,j,kLev,bi,bj)
#else
             surfalk(i,j) = 2.366595 * salt(i,j,kLev,bi,bj)/gsm_s
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = 5.1225e-4 * maskC(i,j,kLev,bi,bj)
#endif
C FOR NON-INTERACTIVE Si
             surfsi(i,j)   = SILICA(i,j,bi,bj) * maskC(i,j,kLev,bi,bj)
          ENDDO
         ENDDO

         CALL CARBON_COEFFS(
     I                       theta,salt,
     I                       bi,bj,iMin,iMax,jMin,jMax)
C====================================================================

c pCO2 solver...
C$TAF LOOP = parallel
       DO j=1-OLy,sNy+OLy
C$TAF LOOP = parallel
        DO i=1-OLx,sNx+OLx

          IF(maskC(i,j,kLev,bi,bj) .NE. 0.)THEN
            CALL CALC_PCO2_APPROX(
     I        theta(i,j,kLev,bi,bj),salt(i,j,kLev,bi,bj),
     I        PTR_CO2(i,j,kLev), surfphos(i,j),
     I        surfsi(i,j),surfalk(i,j),
     I        ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I        ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I        aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I        aksi(i,j,bi,bj),akf(i,j,bi,bj),ff(i,j,bi,bj),
     I        bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U        pH(i,j,bi,bj),pCO2(i,j,bi,bj) )
          ELSE
             pCO2(i,j,bi,bj)=0. _d 0
          END IF
        ENDDO
       ENDDO

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx

            IF (maskC(i,j,kLev,bi,bj).NE.0.) THEN
C calculate SCHMIDT NO. for CO2
              SchmidtNoDIC(i,j) = 
     &            sca1 
     &          + sca2 * theta(i,j,kLev,bi,bj)
     &          + sca3 * theta(i,j,kLev,bi,bj)*theta(i,j,kLev,bi,bj)  
     &          + sca4 * theta(i,j,kLev,bi,bj)*theta(i,j,kLev,bi,bj) 
     &                *theta(i,j,kLev,bi,bj)

C Determine surface flux (FDIC)
C first correct pCO2at for surface atmos pressure
              pCO2sat(i,j) = 
     &          AtmosP(i,j,bi,bj)*AtmospCO2(i,j,bi,bj)
c find exchange coefficient
c  account for schmidt number and and varible piston velocity
              Kwexch(i,j) =
     &             pisvel(i,j,bi,bj)
     &             / sqrt(SchmidtNoDIC(i,j)/660.0)
c OR use a constant  coeff
c             Kwexch(i,j) = 5e-5
c ice influence
              Kwexch(i,j)  =(1.d0-Fice(i,j,bi,bj))*Kwexch(i,j)


C Calculate flux in terms of DIC units using K0, solubility
C Flux = Vp * ([CO2sat] - [CO2])
C CO2sat = K0*pCO2atmos*P/P0
C Converting pCO2 to [CO2] using ff, as in CALC_PCO2
              FluxCO2(i,j,bi,bj) = 
     &         maskC(i,j,kLev,bi,bj)*Kwexch(i,j)*( 
     &         ak0(i,j,bi,bj)*pCO2sat(i,j) - 
     &         ff(i,j,bi,bj)*pCO2(i,j,bi,bj) 
     &         ) 
            ELSE
               FluxCO2(i,j,bi,bj) = 0.
            ENDIF
C convert flux (mol kg-1 m s-1) to (mol m-2 s-1)
            FluxCO2(i,j,bi,bj) = FluxCO2(i,j,bi,bj)/permil

            IF (maskC(i,j,kLev,bi,bj).NE.0.) THEN
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
          ENDDO
         ENDDO

C update tendency      
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GDC(i,j)= maskC(i,j,kLev,bi,bj)*recip_drF(kLev)*
     &                     recip_hFacC(i,j,kLev,bi,bj)*(
     &                    FluxCO2(i,j,bi,bj) + VirtualFlux(i,j)
     &                                              )
          ENDDO
         ENDDO

#endif
        RETURN
        END
