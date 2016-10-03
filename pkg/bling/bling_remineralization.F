C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_remineralization.F,v 1.6 2016/10/03 16:45:38 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_REMIN(
     I           PTR_NO3, PTR_FE, PTR_O2, irr_inst,
     I           N_spm, P_spm, Fe_spm, CaCO3_uptake,
     O           N_reminp, P_reminp, Fe_reminsum,
     O           N_den_benthic, CaCO3_diss,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myIter, myTime, myThid )

C     =================================================================
C     | subroutine bling_remin
C     | o Organic matter export and remineralization.
C     | - Sinking particulate flux and diel migration contribute to
C     |   export.
C     | - Benthic denitrification
C     | - Iron source from sediments
C     | - Iron scavenging
C     =================================================================

      implicit none

C     === Global variables ===

#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     PTR_NO3       :: nitrate concentration
C     PTR_FE        :: iron concentration
C     PTR_O2        :: oxygen concentration
      _RL     PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     irr_inst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     N_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     P_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     Fe_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     CaCO3_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      
C     === Output ===
C     N_reminp      :: remineralization of particulate organic nitrogen
C     N_den_benthic :: Benthic denitrification
C     P_reminp      :: remineralization of particulate organic nitrogen
C     Fe_reminsum   :: iron remineralization and adsorption
C     CaCO3_diss    :: Calcium carbonate dissolution
      _RL     N_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     N_den_benthic(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     P_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     Fe_reminsum(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     CaCO3_diss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_BLING
C     === Local variables ===
C     i,j,k         :: loop indices
      
      INTEGER i,j,k
      INTEGER bttmlyr
      _RL PONflux_u
      _RL PONflux_l
      _RL POPflux_u
      _RL POPflux_l
      _RL PFEflux_u
      _RL PFEflux_l
      _RL CaCO3flux_u
      _RL CaCO3flux_l
      _RL depth_l
      _RL zremin
      _RL zremin_caco3
      _RL wsink
      _RL POC_sed
      _RL Fe_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PO4_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL O2_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL lig_stability
      _RL FreeFe
      _RL Fe_ads_inorg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_ads_org(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL log_btm_flx
      _RL Fe_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_burial(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

c ---------------------------------------------------------------------
c  Initialize output and diagnostics

       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              Fe_ads_org(i,j,k)   = 0. _d 0
              Fe_ads_inorg(i,j,k) = 0. _d 0
              N_reminp(i,j,k)     = 0. _d 0
              P_reminp(i,j,k)     = 0. _d 0
              Fe_reminp(i,j,k)    = 0. _d 0
              Fe_reminsum(i,j,k)  = 0. _d 0
              N_den_benthic(i,j,k)= 0. _d 0
              CaCO3_diss(i,j,k)   = 0. _d 0
          ENDDO
        ENDDO
       ENDDO
        DO j=jmin,jmax
          DO i=imin,imax
              Fe_burial(i,j)       = 0. _d 0
              NO3_sed(i,j)         = 0. _d 0
              PO4_sed(i,j)         = 0. _d 0
              O2_sed(i,j)          = 0. _d 0
          ENDDO
        ENDDO

c ---------------------------------------------------------------------
c  Remineralization

C$TAF LOOP = parallel
       DO j=jmin,jmax
C$TAF LOOP = parallel
        DO i=imin,imax

C  Initialize upper flux
        PONflux_u            = 0. _d 0
        POPflux_u            = 0. _d 0
        PFEflux_u            = 0. _d 0
        CaCO3flux_u          = 0. _d 0

        DO k=1,Nr

C Initialization here helps taf
         Fe_ads_org(i,j,k)    = 0. _d 0

C ARE WE ON THE BOTTOM
         bttmlyr = 1
          IF (k.LT.Nr) THEN
           IF (hFacC(i,j,k+1,bi,bj).GT.0) bttmlyr = 0
C          we are not yet at the bottom
          ENDIF

         IF ( hFacC(i,j,k,bi,bj).gt.0. _d 0 ) THEN

C  Sinking speed is evaluated at the bottom of the cell
          depth_l=-rF(k+1)
          IF (depth_l .LE. wsink0z)  THEN
           wsink = wsink0
          ELSE
           wsink = wsinkacc * (depth_l - wsink0z) + wsink0
          ENDIF

C  Nutrient remineralization lengthscale
C  Not an e-folding scale: this term increases with remineralization.
          zremin = gamma_POM * ( PTR_O2(i,j,k)**2 /
     &               (k_O2**2 + PTR_O2(i,j,k)**2) * (1-remin_min)
     &               + remin_min )/(wsink + epsln)

C  Calcium remineralization relaxed toward the inverse of the
C  ca_remin_depth constant value as the calcite saturation approaches 0.
          zremin_caco3 = 1. _d 0/ca_remin_depth*(1. _d 0 - min(1. _d 0,
     &               omegaC(i,j,k,bi,bj) + epsln ))

C  POM flux leaving the cell
          PONflux_l = (PONflux_u+N_spm(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &           *hFacC(i,j,k,bi,bj))

          POPflux_l = (POPflux_u+P_spm(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  CaCO3 flux leaving the cell
          CaCO3flux_l = (caco3flux_u+CaCO3_uptake(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin_caco3*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  Start with cells that are not the deepest cells
          IF (bttmlyr.EQ.0) THEN
C  Nutrient accumulation in a cell is given by the biological production
C  (and instant remineralization) of particulate organic matter
C  plus flux thought upper interface minus flux through lower interface.
C  (Since not deepest cell: hFacC=1)
           N_reminp(i,j,k) = (PONflux_u + N_spm(i,j,k)*drF(k)
     &                    - PONflux_l)*recip_drF(k)

           P_reminp(i,j,k) = (POPflux_u + P_spm(i,j,k)*drF(k)
     &                    - POPflux_l)*recip_drF(k)

           CaCO3_diss(i,j,k) = (CaCO3flux_u + CaCO3_uptake(i,j,k)
     &                    *drF(k) - CaCO3flux_l)*recip_drF(k)

           Fe_sed(i,j,k) = 0. _d 0
C NOW DO BOTTOM LAYER
          ELSE
C  If this layer is adjacent to bottom topography or it is the deepest
C  cell of the domain, then remineralize/dissolve in this grid cell
C  i.e. do not subtract off lower boundary fluxes when calculating remin

           N_reminp(i,j,k) = PONflux_u*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj) + N_spm(i,j,k)

           P_reminp(i,j,k) = POPflux_u*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj) + P_spm(i,j,k)

           CaCO3_diss(i,j,k) = CaCO3flux_u*recip_drF(k)
     &                  *recip_hFacC(i,j,k,bi,bj) + CaCO3_uptake(i,j,k)

c  Efflux Fed out of sediments
C  The phosphate flux hitting the bottom boundary
C  is used to scale the return of iron to the water column.
C  Maximum value added for numerical stability.

           POC_sed = PONflux_l * CtoN

           Fe_sed(i,j,k) = min(1. _d -11,
     &            max(epsln, FetoC_sed * POC_sed * recip_drF(k)
     &            *recip_hFacC(i,j,k,bi,bj)))

cav temporary until I figure out why this is problematic for adjoint
#ifndef BLING_ADJOINT_SAFE

#ifndef USE_SGS_SED
c Calculate benthic denitrification and Fe efflux here, if the subgridscale sediment module is not being used.

           IF (POC_sed .gt. 0. _d 0) THEN

            log_btm_flx = 0. _d 0

c Convert from mol N m-2 s-1 to umol C cm-2 d-1 and take the log

            log_btm_flx = log10(min(43.0 _d 0, POC_sed *
     &           86400. _d 0 * 100.0 _d 0))

c Metamodel gives units of umol C cm-2 d-1, convert to mol N m-2 s-1 and multiply by
c no3_2_n to give NO3 consumption rate

             N_den_benthic(i,j,k) = min (POC_sed * NO3toN / CtoN,
     &         (10 _d 0)**(-0.9543 _d 0 + 0.7662 _d 0 *
     &         log_btm_flx - 0.235 _d 0 * log_btm_flx * log_btm_flx)
     &         / (CtoN * 86400. _d 0 * 100.0 _d 0) * NO3toN *
     &         PTR_NO3(i,j,k) / (k_no3 + PTR_NO3(i,j,k)) ) *
     &         recip_drF(k)

          endif

#endif

#endif

c ---------------------------------------------------------------------
c Calculate external bottom fluxes for tracer_vertdiff. Positive fluxes are into the water
c column from the seafloor. For P, the bottom flux puts the sinking flux reaching the bottom
c cell into the water column through diffusion. For iron, the sinking flux disappears into the
c sediments if bottom waters are oxic (assumed adsorbed as oxides). If bottom waters are anoxic,
c the sinking flux of Fe is returned to the water column.
c
c For oxygen, the consumption of oxidant required to respire
c the settling flux of organic matter (in support of the
c no3 bottom flux) diffuses from the bottom water into the sediment.

c Assume all NO3 for benthic denitrification is supplied from the bottom water, and that
c all organic N is also consumed under denitrification (Complete Denitrification, sensu
c Paulmier, Biogeosciences 2009). Therefore, no NO3 is regenerated from organic matter
c respired by benthic denitrification (necessitating the second term in b_no3).

          NO3_sed(i,j) = PONflux_l*drF(k)*hFacC(i,j,k,bi,bj)
     &                   - N_den_benthic(i,j,k) / NO3toN

          PO4_sed(i,j) = POPflux_l*drF(k)*hFacC(i,j,k,bi,bj)

c Oxygen flux into sediments is that required to support non-denitrification respiration,
c assuming a 4/5 oxidant ratio of O2 to NO3. Oxygen consumption is allowed to continue
c at negative oxygen concentrations, representing sulphate reduction.

          O2_sed(i,j) = -(O2toN * PONflux_l*drF(k)*hFacC(i,j,k,bi,bj)
     &                  - N_den_benthic(i,j,k)* 1.25)

          ENDIF

C  Begin iron uptake calculations by determining ligand bound and free iron.
C  Both forms are available for biology, but only free iron is scavenged
C  onto particles and forms colloids.

          lig_stability = kFe_eq_lig_max-(KFe_eq_lig_max-kFe_eq_lig_min)
     &             *(irr_inst(i,j,k)**2
     &             /(kFe_eq_lig_irr**2+irr_inst(i,j,k)**2))
     &             *max(epsln,min(1. _d 0,(PTR_FE(i,j,k)
     &             -kFe_eq_lig_Femin)/
     &             (PTR_FE(i,j,k)+epsln)*1.2  _d 0))

C  Use the quadratic equation to solve for binding between iron and ligands

          FreeFe = (-(1+lig_stability*(ligand-PTR_FE(i,j,k)))
     &            +((1+lig_stability*(ligand-PTR_FE(i,j,k)))**2+4*
     &            lig_stability*PTR_FE(i,j,k))**(0.5))/(2*
     &            lig_stability)

C  Iron scavenging does not occur in anoxic water (Fe2+ is soluble), so set
C  FreeFe = 0 when anoxic.  FreeFe should be interpreted the free iron that
C  participates in scavenging.

#ifndef BLING_ADJOINT_SAFE
          IF (PTR_O2(i,j,k) .LT. oxic_min)  THEN
           FreeFe = 0. _d 0
          ENDIF
#endif

c  Two mechanisms for iron uptake, in addition to biological production:
c  colloidal scavenging and scavenging by organic matter.

c  Colloidal scavenging:
c  Minimum function for numerical stability
c           Fe_uptake(i,j,k) = Fe_uptake(i,j,k)+
c     &       min(0.5/PTRACERS_dTLev(1), kFe_inorg*FreeFe**(0.5))*FreeFe

           Fe_ads_inorg(i,j,k) =
     &       kFe_inorg*(max(1. _d -8,FreeFe))**(1.5)

C  Scavenging of iron by organic matter:
c  The POM value used is the bottom boundary flux. This does not occur in
c  oxic waters, but FreeFe is set to 0 in such waters earlier.
           IF ( PONflux_l .GT. 0. _d 0 ) THEN

c  Minimum function for numerical stability
c            Fe_uptake(i,j,k) = Fe_uptake(i,j,k)+
c     &           min(0.5/PTRACERS_dTLev(1), kFE_org*(POMflux_l
c     &           *CtoP/NUTfac*12.01/wsink)**(0.58)*FreeFe

#ifndef BLING_ADJOINT_SAFE
            Fe_ads_org(i,j,k) =
     &           kFE_org*(PONflux_l/(epsln + wsink)
     &             * MasstoN)**(0.58)*FreeFe
#else
            Fe_ads_org(i,j,k) =
     &           kFE_org*(PONflux_l/(epsln + wsink0)
     &             * MasstoN)**(0.58)*FreeFe
#endif
           ENDIF

C  If water is oxic then the iron is remineralized normally. Otherwise
C  it is completely remineralized (fe 2+ is soluble, but unstable
C  in oxidizing environments).

           PFEflux_l = (PFEflux_u+(Fe_spm(i,j,k)+Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &            *hFacC(i,j,k,bi,bj))

c  Added the burial flux of sinking particulate iron here as a
c  diagnostic, needed to calculate mass balance of iron.
c  this is calculated last for the deepest cell

           Fe_burial(i,j) = PFeflux_l

#ifndef BLING_ADJOINT_SAFE
           IF ( PTR_O2(i,j,k) .LT. oxic_min ) THEN
            PFEflux_l = 0
           ENDIF
#endif

           Fe_reminp(i,j,k) = (pfeflux_u+(Fe_spm(i,j,k)
     &            +Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj)-pfeflux_l)*recip_drF(k)
     &            *recip_hFacC(i,j,k,bi,bj)
C!! there is an intercept_frac here... need to add

C  Prepare the tracers for the next layer down
          PONflux_u   = PONflux_l
          POPflux_u   = POPflux_l
          PFEflux_u   = PFEflux_l
          CaCO3flux_u = CaCO3flux_l

c
          Fe_reminsum(i,j,k) = Fe_reminp(i,j,k) + Fe_sed(i,j,k)
     &                 - Fe_ads_org(i,j,k) - Fe_ads_inorg(i,j,k)

         ENDIF

        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

c 3d local variables
c        CALL DIAGNOSTICS_FILL(POC_flux,'BLGPOCF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_ads_org,   'BLGFEAO ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_ads_inorg, 'BLGFEAI ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_sed,   'BLGFESED',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_reminp,'BLGFEREM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_reminp, 'BLGNREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_reminp, 'BLGPREM ',0,Nr,2,bi,bj,myThid)
c 2d local variables
        CALL DIAGNOSTICS_FILL(Fe_burial,'BLGFEBUR',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NO3_sed,  'BLGNSED ',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PO4_sed,  'BLGPSED ',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(O2_sed,   'BLGO2SED',0,1,2,bi,bj,myThid)
c these variables are currently 1d, could be 3d for diagnostics
c (or diag_fill could be called inside loop - which is faster?)
c        CALL DIAGNOSTICS_FILL(zremin,'BLGZREM ',0,Nr,2,bi,bj,myThid)

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN
      END
