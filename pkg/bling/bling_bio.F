#include "BLING_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_BIO(
     I           PTR_O2, PTR_FE, PTR_PO4, PTR_DOP,
     O           G_DIC, G_ALK, G_O2, G_FE,
     O           G_PO4, G_DOP,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_bio_nitrogen
C     | o Nutrient uptake and partitioning between organic pools.
C     | - Phytoplankton biomass-specific growth rate is calculated
C     |   as a function of light, nutrient limitation, and
C     |   temperature.
C     | - A simple relationship between growth rate,
C     |   biomass, and uptake is derived by assuming that growth is
C     |   exactly balanced by losses.
C     | o Organic matter export and remineralization.
C     | - Calculate the nutrient flux to depth from bio activity
C     | - Iron source from sediments
C     | - Iron scavenging
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
C     phyto_sm     :: Small phytoplankton biomass
C     phyto_lg     :: Large phytoplankton biomass
C     phyto_diaz   :: Diazotroph phytoplankton biomass
C *** if ADVECT_PHYTO, these are fraction of total biomass instead ***

#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation DOPain: 1rst index range
C     jMin,jMax     :: computation DOPain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     PTR_O2        :: oxygen concentration
C     PTR_FE        :: iron concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
      _RL     PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     === Output ===
C     G_xxx         :: tendency term for tracer xxx
      _RL     G_DIC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_ALK     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_O2      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_FE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_PO4     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DOP     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_BLING
# ifdef USE_BLING_V1
C     === Local variables ===
C     i,j,k         :: loop indices
C     Phy_lg_local  :: biomass in large phytoplankton
C     Phy_sm_local  :: biomass in small phytoplankton
C     PO4_lim       :: phosphate limitation
C     Fe_lim        :: iron limitation for phytoplankton
C     FetoP_up      :: iron to phosphorus uptake ratio
C     light_lim     :: light limitation
C     expkT         :: temperature function
C     Pc_m          :: light-saturated max photosynthesis rate for phyto
C     Pc_tot        :: carbon-specific photosynthesis rate
C     theta_Fe      :: Chl:C ratio
C     theta_Fe_inv  :: C:Chl ratio
C     theta_Fe_max  :: Fe-replete maximum Chl:C ratio
C     alpha_Fe      :: initial slope of the P-I curve
C     irrk          :: nut-limited efficiency of algal photosystems
C     irr_inst      :: instantaneous light
C     irr_eff       :: effective irradiance
C     mld           :: mixed layer depth
C     mu            :: net carbon-specific growth rate for phyt
C     biomass_sm    :: nutrient concentration in small phyto biomass
C     biomass_lg    :: nutrient concentration in large phyto biomass
C     P_uptake      :: PO4 utilization by phytoplankton
C     Fe_uptake     :: dissolved Fe utilization by phytoplankton
C     CaCO3_uptake  :: Calcium carbonate uptake for shell formation
C     CaCO3_diss    :: Calcium carbonate dissolution
C     G_CaCO3       :: tendency of calcium carbonate
C     DOP_prod      :: production of dissolved organic phosphorus
C     DOP_remin     :: remineralization of dissolved organic phosphorus
C     frac_exp      :: fraction of sinking particulate organic matter
C     P_spm         :: particulate sinking of phosphorus
C     Fe_spm        :: particulate sinking of iron
C     P_recycle     :: recycling of newly-produced organic phosphorus
C     Fe_recycle    :: recycling of newly-produced organic iron
C     P_reminp      :: remineralization of particulate organic phosphorus
C     Fe_reminsum   :: iron remineralization and adsorption
C     POC_flux      :: particulate organic carbon flux
C     NPP           :: net primary production
C     NCP           :: net community production
C     depth_l       :: depth of lower interface
C     *flux_u       :: "*" flux through upper interface
C     *flux_l       :: "*" flux through lower interface
C     *_export      :: vertically-integrated export of "*"
C     zremin        :: remineralization lengthscale for nutrients
C     zremin_caco3  :: remineralization lengthscale for CaCO3
C     wsink         :: speed of sinking particles
C     POC_sed       :: flux of particulate organic carbon to sediment
C     Fe_sed        :: sediment iron efflux
C     kFe_eq_lig    :: iron ligand stability constant
C     FreeFe        :: free (unbound) iron concentration
C     Fe_ads_inorg  :: iron adsorption
C     Fe_ads_org    :: iron adsorption  (2nd order)
C
      INTEGER i,j,k
      INTEGER bottomlayer
      _RL Phy_lg_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_sm_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PO4_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL FetoP_up(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL light_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL expkT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Pc_m
      _RL Pc_tot
      _RL theta_Fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL theta_Fe_inv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL theta_Fe_max
      _RL alpha_Fe
      _RL irrk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_inst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_eff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL biomass_sm
      _RL biomass_lg
      _RL P_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CaCO3_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CaCO3_diss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL G_CaCO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DOP_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DOP_remin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL frac_exp
      _RL P_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_reminsum(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL POC_flux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NCP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL depth_l
      _RL POPflux_u
      _RL POPflux_l
      _RL PFEflux_u
      _RL PFEflux_l
      _RL CaCO3flux_u
      _RL CaCO3flux_l
      _RL POP_export(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PFE_export(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CaCO3_export(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zremin
      _RL zremin_caco3
      _RL wsink
      _RL POC_sed
      _RL Fe_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kFe_eq_lig
      _RL FreeFe
      _RL Fe_ads_inorg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_ads_org(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_burial(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL po4_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fe_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL o2_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dop_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ML_MEAN_PHYTO
      _RL tmp_p_sm_ML
      _RL tmp_p_lg_ML
      _RL tmp_p_diaz_ML
      _RL tmp_ML
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      itdkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif

c ---------------------------------------------------------------------
c  Initialize output and diagnostics
      DO j=jmin,jmax
       DO i=imin,imax
        mld(i,j)           = 0. _d 0
        POP_export(i,j)    = 0. _d 0
        PFE_export(i,j)    = 0. _d 0
        CaCO3_export(i,j)  = 0. _d 0
       ENDDO
      ENDDO
       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              G_DIC(i,j,k)          = 0. _d 0
              G_ALK(i,j,k)          = 0. _d 0
              G_O2(i,j,k)           = 0. _d 0
              G_FE(i,j,k)           = 0. _d 0
              G_PO4(i,j,k)          = 0. _d 0
              G_DOP(i,j,k)          = 0. _d 0
              G_CaCO3(i,j,k)        = 0. _d 0
              Phy_lg_local(i,j,k)   = 0. _d 0
              Phy_sm_local(i,j,k)   = 0. _d 0
              PO4_lim(i,j,k)        = 0. _d 0
              Fe_lim(i,j,k)         = 0. _d 0
              FetoP_up(i,j,k)       = 0. _d 0
              light_lim(i,j,k)      = 0. _d 0
              expkT(i,j,k)          = 0. _d 0
              theta_Fe(i,j,k)       = 0. _d 0
              theta_Fe_inv(i,j,k)   = 0. _d 0
              irrk(i,j,k)           = 0. _d 0
              irr_inst(i,j,k)       = 0. _d 0
              irr_eff(i,j,k)        = 0. _d 0
              mu(i,j,k)             = 0. _d 0
              P_uptake(i,j,k)       = 0. _d 0
              Fe_uptake(i,j,k)      = 0. _d 0
              CaCO3_uptake(i,j,k)   = 0. _d 0
              CaCO3_diss(i,j,k)     = 0. _d 0
              DOP_prod(i,j,k)       = 0. _d 0
              DOP_remin(i,j,k)      = 0. _d 0
              P_spm(i,j,k)          = 0. _d 0
              Fe_spm(i,j,k)         = 0. _d 0
              P_recycle(i,j,k)      = 0. _d 0
              Fe_recycle(i,j,k)     = 0. _d 0
              P_reminp(i,j,k)       = 0. _d 0
              Fe_reminp(i,j,k)      = 0. _d 0
              Fe_reminsum(i,j,k)    = 0. _d 0
              POC_flux(i,j,k)       = 0. _d 0
              NPP(i,j,k)            = 0. _d 0
              NCP(i,j,k)            = 0. _d 0
              Fe_sed(i,j,k)         = 0. _d 0
              Fe_ads_org(i,j,k)     = 0. _d 0
              Fe_ads_inorg(i,j,k)   = 0. _d 0
          ENDDO
       ENDDO
      ENDDO

c-----------------------------------------------------------
c  avoid negative nutrient concentrations that can result from
c  advection when low concentrations

#ifdef BLING_NO_NEG
      CALL BLING_MIN_VAL(PTR_O2,  1. _d -11, o2_adj,  bi, bj)
      CALL BLING_MIN_VAL(PTR_FE,  1. _d -11, fe_adj,  bi, bj)
      CALL BLING_MIN_VAL(PTR_PO4, 1. _d -8,  po4_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_DOP, 1. _d -11, dop_adj, bi, bj)
#endif

c-----------------------------------------------------------
c  Phytoplankton size classes

       DO k=1,Nr
        DO j=jmin,jmax
         DO i=imin,imax
          Phy_lg_local(i,j,k) = phyto_lg(i,j,k,bi,bj)
          Phy_sm_local(i,j,k) = phyto_sm(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO

c-----------------------------------------------------------
c  Mixed layer depth calculation for light, phytoplankton and dvm.
c  Do not need to calculate if not using ML_MEAN_LIGHT, ML_MEAN_PHYTO,
c  and USE_BLING_DVM
c  (with BLING_ADJOINT_SAFE flag, USE_BLING_DVM is undefined)

#if ( defined (ML_MEAN_LIGHT) || \
      defined (ML_MEAN_PHYTO) || \
      defined (USE_BLING_DVM) )
       CALL BLING_MIXEDLAYER(
     U                         mld,
     I                         bi, bj, imin, imax, jmin, jmax,
     I                         myTime, myIter, myThid)
#endif

c  Phytoplankton mixing
c  The mixed layer is assumed to homogenize vertical gradients of phytoplankton.
c  This allows for basic Sverdrup dynamics in a qualitative sense.
c  This has not been thoroughly tested, and care should be
c  taken with its interpretation.

#ifdef ML_MEAN_PHYTO
      DO j=jmin,jmax
       DO i=imin,imax

        tmp_p_sm_ML = 0. _d 0
        tmp_p_lg_ML = 0. _d 0
        tmp_ML  = 0. _d 0

        DO k=1,Nr

         IF (hFacC(i,j,k,bi,bj).gt.0. _d 0) THEN
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.MLmix_max)) THEN
          tmp_p_sm_ML = tmp_p_sm_ML+Phy_sm_local(i,j,k)*drF(k)
     &                  *hFacC(i,j,k,bi,bj)
          tmp_p_lg_ML = tmp_p_lg_ML+Phy_lg_local(i,j,k)*drF(k)
     &                  *hFacC(i,j,k,bi,bj)
          tmp_ML = tmp_ML + drF(k)
         ENDIF
         ENDIF

        ENDDO

        DO k=1,Nr

         IF (hFacC(i,j,k,bi,bj).gt.0. _d 0) THEN
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.MLmix_max)) THEN

          Phy_lg_local(i,j,k)   = max(1. _d -8,tmp_p_lg_ML/tmp_ML)
          Phy_sm_local(i,j,k)   = max(1. _d -8,tmp_p_sm_ML/tmp_ML)

         ENDIF
         ENDIF

        ENDDO
       ENDDO
      ENDDO

#endif

c-----------------------------------------------------------
c  light availability for biological production

       CALL BLING_LIGHT(
     I                    mld,
     U                    irr_inst, irr_eff,
     I                    bi, bj, imin, imax, jmin, jmax,
     I                    myTime, myIter, myThid )

c  phytoplankton photoadaptation to local light level
c  This represents the fact that phytoplankton cells are adapted
c  to the averaged light field to which they've been exposed over
c  their lifetimes, rather than the instantaneous light. The
c  timescale is set by gamma_irr_mem.

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

          irr_mem(i,j,k,bi,bj) = irr_mem(i,j,k,bi,bj) +
     &           (irr_eff(i,j,k) - irr_mem(i,j,k,bi,bj))*
     &           min( 1. _d 0, gamma_irr_mem*PTRACERS_dTLev(k) )

        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------
c  Nutrient uptake and partitioning between organic pools

c  Phytoplankton are assumed to grow according to the general properties
c  described in Geider (1997). This formulation gives a biomass-specific
c  growthrate as a function of light, nutrient limitation, and
c  temperature. We modify this relationship slightly here, as described
c  below, and also use the assumption of steady state growth vs. loss to
c  derive a simple relationship between growth rate, biomass and uptake.

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c ---------------------------------------------------------------------
c  First, calculate the limitation terms for PO4 and Fe, and the
c  Fe-limited Chl:C maximum. The light-saturated maximal photosynthesis
c  rate term (Pc_m) is simply the product of a prescribed maximal
c  photosynthesis rate (Pc_0), the Eppley temperature dependence, and a
c  resource limitation term. The iron limitation term has a lower limit
c  of Fe_lim_min and is scaled by (k_Fe2P + Fe2P_max) / Fe2P_max so that
c  it approaches 1 as Fe approaches infinity. Thus, it is of comparable
c  magnitude to the macro-nutrient limitation term.

c  Macro-nutrient limitation

          PO4_lim(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4)

c  Iron to macro-nutrient uptake. More iron is utilized relative
c  to macro-nutrient under iron-replete conditions.

          FetoP_up(i,j,k) = FetoP_max*PTR_FE(i,j,k)/(k_Fe+PTR_FE(i,j,k))

c  Iron limitation

          Fe_lim(i,j,k) = Fe_lim_min + (1-Fe_lim_min)*
     &                    (FetoP_up(i,j,k)/(k_FetoP + FetoP_up(i,j,k)))*
     &                    (k_FetoP+FetoP_max)/FetoP_max

c ---------------------------------------------------------------------
c  Light-saturated maximal photosynthesis rate

c  NB: The temperature effect of Eppley (1972) is used instead of that in
c  Geider et al (1997) for both simplicity and to incorporate combined
c  effects on uptake, incorporation into organic matter and photorespiration.
c  Values of PCc_m are normalized to 0C rather than 20C in Geider et al. (1997)

          expkT(i,j,k) = exp(kappa_eppley * theta(i,j,k,bi,bj))

c  For the effective resource limitation, there is an option to replace
c  the default Liebig limitation (the minimum of Michaelis-Menton
c  PO4-limitation, or iron-limitation) by the product (safer for adjoint)

#ifdef MULT_NUT_LIM
          Pc_m = Pc_0*expkT(i,j,k)
     &           *PO4_lim(i,j,k)*Fe_lim(i,j,k)*maskC(i,j,k,bi,bj)
#else
          Pc_m = Pc_0*expkT(i,j,k)
     &           *min(PO4_lim(i,j,k), Fe_lim(i,j,k))*maskC(i,j,k,bi,bj)
#endif

c ---------------------------------------------------------------------
c  Fe limitation 1) reduces photosynthetic efficiency (alpha_Fe)
c  and 2) reduces the maximum achievable Chl:C ratio (theta_Fe)
c  below a prescribed, Fe-replete maximum value (theta_Fe_max),
c  to approach a prescribed minimum Chl:C (theta_Fe_min) under extreme
c  Fe-limitation.

          alpha_Fe = alpha_min + (alpha_max-alpha_min)*Fe_lim(i,j,k)

          theta_Fe_max = theta_Fe_max_lo+
     &                  (theta_Fe_max_hi-theta_Fe_max_lo)*Fe_lim(i,j,k)

          theta_Fe(i,j,k) = theta_Fe_max/(1. _d 0 + alpha_Fe
     &                  *theta_Fe_max*irr_mem(i,j,k,bi,bj)/
     &                  (2. _d 0*Pc_m))

c  for diagnostics: C:Chl ratio in g C / g Chl

          IF ( theta_Fe(i,j,k) .EQ.0. ) THEN
           theta_Fe_inv(i,j,k) = 0.
          ELSE
           theta_Fe_inv(i,j,k) = 1./theta_Fe(i,j,k)
          ENDIF

c ---------------------------------------------------------------------
c  Nutrient-limited efficiency of algal photosystems, irrk, is calculated
c  with the iron limitation term included as a multiplier of the
c  theta_Fe_max to represent the importance of Fe in forming chlorophyll
c  accessory antennae, which do not affect the Chl:C but still affect the
c  phytoplankton ability to use light (eg Stzrepek & Harrison, Nature 2004).

          irrk(i,j,k) = Pc_m/(epsln + alpha_Fe*theta_Fe_max) +
     &              irr_mem(i,j,k,bi,bj)/2. _d 0

          light_lim(i,j,k) = ( 1. _d 0 - exp(-irr_eff(i,j,k)
     &               /(epsln + irrk(i,j,k))))

c  Carbon-specific photosynthesis rate

          Pc_tot = Pc_m * light_lim(i,j,k)

c ---------------------------------------------------------------------
c  Account for the maintenance effort that phytoplankton must exert in
c  order to combat decay. This is prescribed as a fraction of the
c  light-saturated photosynthesis rate, resp_frac. The result of this
c  is to set a level of energy availability below which net growth
c  (and therefore nutrient uptake) is zero, given by resp_frac * Pc_m.

          mu(i,j,k) = max(0. _d 0, Pc_tot - resp_frac*Pc_m)

c ---------------------------------------------------------------------
c  In order to convert this net carbon-specific growth rate to nutrient
c  uptake rates, the quantities we are interested in, a biomass is required.
c  This is determined by the balance between growth and grazing.

c  Since there is no explicit biomass tracer, use the result of Dunne
c  et al. (GBC, 2005) to calculate an implicit biomass from the uptake
c  rate through the application of a simple idealized grazing law.

c  instantaneous nutrient concentration in phyto biomass

          biomass_lg = pivotal*(mu(i,j,k)/(lambda_0
     &                *expkT(i,j,k)))**3

          biomass_sm = pivotal*(mu(i,j,k)/(lambda_0
     &                *expkT(i,j,k)))

c  phytoplankton biomass diagnostic
c  for no lag: set gamma_biomass to 0

          phy_sm_local(i,j,k) = phy_sm_local(i,j,k) +
     &       (biomass_sm - phy_sm_local(i,j,k))
     &       *min(1., gamma_biomass*PTRACERS_dTLev(k))

          phy_lg_local(i,j,k) = phy_lg_local(i,j,k) +
     &       (biomass_lg - phy_lg_local(i,j,k))
     &       *min(1., gamma_biomass*PTRACERS_dTLev(k))

c  use the diagnostic biomass to calculate the chl concentration
c  in mg/m3 (carbon = 12.01 g/mol)

          chl(i,j,k,bi,bj) = max(chl_min, CtoP * 12.01 * 1. _d 3 *
     &           theta_Fe(i,j,k) *
     &           (Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k)))

c  Nutrient uptake

          P_uptake(i,j,k) = mu(i,j,k)*(phy_sm_local(i,j,k)
     &                        + phy_lg_local(i,j,k))

c  Iron is then taken up as a function of nutrient uptake and iron
c  limitation, with a maximum Fe:P uptake ratio of Fe2p_max

          Fe_uptake(i,j,k) = P_uptake(i,j,k)*FetoP_up(i,j,k)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c  Separate loop for adjoint stores
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Phy_sm_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_lg_local   = comlev1_bibj, key=itdkey, kind=isbyte
#endif

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c  update biomass
          phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)
          phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------
c  Partitioning between organic pools

c  The uptake of nutrients is assumed to contribute to the growth of
c  phytoplankton, which subsequently die and are consumed by heterotrophs.
c  This can involve the transfer of nutrient elements between many
c  organic pools, both particulate and dissolved, with complex histories.
c  We take a simple approach here, partitioning the total uptake into two
c  fractions - sinking and non-sinking - as a function of temperature,
c  following Dunne et al. (2005).
c  Then, the non-sinking fraction is further subdivided, such that the
c  majority is recycled instantaneously to the inorganic nutrient pool,
c  representing the fast turnover of labile dissolved organic matter via
c  the microbial loop, and the remainder is converted to semi-labile
c  dissolved organic matter. Iron and macro-nutrient are treated
c  identically for the first step, but all iron is recycled
c  instantaneously in the second step (i.e. there is no dissolved organic
c  iron pool).

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c  sinking particulate organic matter

          frac_exp = (phi_sm + phi_lg *
     &               (mu(i,j,k)/(lambda_0*expkT(i,j,k)))**2.)/
     &               (1. + (mu(i,j,k)/(lambda_0*expkT(i,j,k)))**2.)*
     &               exp(kappa_remin * theta(i,j,k,bi,bj))

          P_spm(i,j,k) = frac_exp * P_uptake(i,j,k)

          Fe_spm(i,j,k) = P_spm(i,j,k)*FetoP_up(i,j,k)

c  the remainder is divided between instantaneously recycled and
c  long-lived dissolved organic matter.
c  (recycling = P_uptake - P_spm - DOP_prod)

          DOP_prod(i,j,k) = phi_DOM*(P_uptake(i,j,k)
     &                      - P_spm(i,j,k))

          P_recycle(i,j,k) = P_uptake(i,j,k) - P_spm(i,j,k)
     &                        - DOP_prod(i,j,k)

          Fe_recycle(i,j,k) = Fe_uptake(i,j,k) - Fe_spm(i,j,k)

c  Carbon flux diagnostic

          POC_flux(i,j,k) = CtoP*P_spm(i,j,k)

c ---------------------------------------------------------------------
c  Calcium carbonate production

c  Alkalinity is consumed through the production of CaCO3. Here, this is
c  simply a linear function of the implied growth rate of small
c  phytoplankton, which gave a reasonably good fit to the global
c  observational synthesis of Dunne (2009). This is consistent
c  with the findings of Jin et al. (GBC,2006).

          CaCO3_uptake(i,j,k) = phy_sm_local(i,j,k)*phi_sm
     &           *expkT(i,j,k)*mu(i,j,k)*CatoP

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------
c  Nutrients export/remineralization, CaCO3 export/dissolution
c
c  The flux at the bottom of a grid cell equals
C  Fb = (Ft + prod*dz) / (1 + zremin*dz)
C  where Ft is the flux at the top, and prod*dz is the integrated
C  production of new sinking particles within the layer.
C  Ft = 0 in the first layer.

C$TAF LOOP = parallel
       DO j=jmin,jmax
C$TAF LOOP = parallel
        DO i=imin,imax

C  Initialize upper flux

        POPflux_u            = 0. _d 0
        PFEflux_u            = 0. _d 0
        CaCO3flux_u          = 0. _d 0

        DO k=1,Nr

C Initialization here helps taf

         Fe_ads_org(i,j,k)    = 0. _d 0

c  check if we are at a bottom cell

         bottomlayer = 1
          IF (k.LT.Nr) THEN
           IF (hFacC(i,j,k+1,bi,bj).GT.0) THEN
c  not a bottom cell
            bottomlayer = 0
           ENDIF
          ENDIF

         IF ( hFacC(i,j,k,bi,bj).gt.0. _d 0 ) THEN

C  Sinking speed is evaluated at the bottom of the cell

          depth_l=-rF(k+1)
          IF (depth_l .LE. wsink0z)  THEN
           wsink = wsink0_2d(i,j,bi,bj)
          ELSE
           wsink = wsinkacc * (depth_l - wsink0z) + wsink0_2d(i,j,bi,bj)
          ENDIF

C  Nutrient remineralization lengthscale
C  Not an e-folding scale: this term increases with remineralization.

          zremin = gamma_POM_2d(i,j,bi,bj) * ( PTR_O2(i,j,k)**2 /
     &               (k_O2**2 + PTR_O2(i,j,k)**2) * (1-remin_min)
     &               + remin_min )/(wsink + epsln)

C  Calcium remineralization relaxed toward the inverse of the
C  ca_remin_depth constant value as the calcite saturation approaches 0.

          zremin_caco3 = 1. _d 0/ca_remin_depth*(1. _d 0 - min(1. _d 0,
     &               omegaC(i,j,k,bi,bj) + epsln ))

C  POM flux leaving the cell

          POPflux_l = (POPflux_u+P_spm(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  CaCO3 flux leaving the cell

          CaCO3flux_l = (caco3flux_u+CaCO3_uptake(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin_caco3*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  Begin iron uptake calculations by determining ligand bound and free iron.
C  Both forms are available for biology, but only free iron is scavenged
C  onto particles and forms colloids.

          kFe_eq_lig = kFe_eq_lig_max-(kFe_eq_lig_max-kFe_eq_lig_min)
     &             *(irr_inst(i,j,k)**2
     &             /(kFe_eq_lig_irr**2+irr_inst(i,j,k)**2))
     &             *max(epsln,min(1. _d 0,(PTR_FE(i,j,k)
     &             -kFe_eq_lig_Femin)/
     &             (PTR_FE(i,j,k)+epsln)*1.2 _d 0))

C  Use the quadratic equation to solve for binding between iron and ligands

          FreeFe = (-(1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))
     &            +((1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))**2+4*
     &            kFe_eq_lig*PTR_FE(i,j,k))**(0.5))/(2*
     &            kFe_eq_lig)

C  Iron scavenging does not occur in anoxic water (Fe2+ is soluble), so set
C  FreeFe = 0 when anoxic.  FreeFe should be interpreted the free iron that
C  participates in scavenging.

          IF (PTR_O2(i,j,k) .LT. oxic_min)  THEN
           FreeFe = 0. _d 0
          ENDIF

C  Two mechanisms for iron uptake, in addition to biological production:
C  colloidal scavenging and scavenging by organic matter.

           Fe_ads_inorg(i,j,k) =
     &       kFe_inorg*(max(1. _d -8,FreeFe))**(1.5)

C  Scavenging of iron by organic matter:
C  The POM value used is the bottom boundary flux. This does not occur in
C  oxic waters, but FreeFe is set to 0 in such waters earlier.

           IF ( POPflux_l .GT. 0. _d 0 ) THEN
            Fe_ads_org(i,j,k) =
     &           kFE_org*(POPflux_l/(epsln + wsink)
     &             * MasstoN*NtoP)**(0.58)*FreeFe
           ENDIF

C  If water is oxic then the iron is remineralized normally. Otherwise
C  it is completely remineralized (fe 2+ is soluble, but unstable
C  in oxidizing environments).

           IF ( PTR_O2(i,j,k) .LT. oxic_min ) THEN
            PFEflux_l = 0. _d 0
           ELSE
            PFEflux_l = (PFEflux_u+(Fe_spm(i,j,k)+Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &            *hFacC(i,j,k,bi,bj))
           ENDIF

C  Nutrient accumulation in a cell is given by the biological production
C  (and instant remineralization) of particulate organic matter
C  plus flux thought upper interface minus flux through lower interface.
C  If this layer is adjacent to bottom topography or it is the deepest
C  cell of the domain, then remineralize/dissolve in this grid cell
C  i.e. do not subtract off lower boundary fluxes when calculating remin

C  For the deepest cells:

          IF (bottomlayer.EQ.1) THEN

           POPflux_l   = 0. _d 0
           CACO3flux_l = 0. _d 0

C  Efflux Fed out of sediments
C  The phosphate flux hitting the bottom boundary
C  is used to scale the return of iron to the water column.
C  Maximum value added for numerical stability.

           POC_sed = POPflux_l * CtoP

           Fe_sed(i,j,k) = max(epsln, FetoC_sed * POC_sed * recip_drF(k)
     &                   * recip_hFacC(i,j,k,bi,bj))

          ELSE

           Fe_sed(i,j,k) = 0. _d 0

          ENDIF

          P_reminp(i,j,k) = (POPflux_u + P_spm(i,j,k) * drF(k)
     &                      * hFacC(i,j,k,bi,bj) - POPflux_l)
     &                      * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          CaCO3_diss(i,j,k) = (CaCO3flux_u + CaCO3_uptake(i,j,k)
     &                      * drF(k) * hFacC(i,j,k,bi,bj) - CaCO3flux_l)
     &                      * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          Fe_sed(i,j,k) = 0. _d 0

          Fe_reminp(i,j,k) = (PFEflux_u + (Fe_spm(i,j,k)
     &                     + Fe_ads_inorg(i,j,k) + Fe_ads_org(i,j,k))
     &                     * drF(k) * hFacC(i,j,k,bi,bj) - PFEflux_l)
     &                     * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          Fe_reminsum(i,j,k) = Fe_reminp(i,j,k) + Fe_sed(i,j,k)
     &                       - Fe_ads_org(i,j,k) - Fe_ads_inorg(i,j,k)

C  Added the burial flux of sinking particulate iron here as a
C  diagnostic, needed to calculate mass balance of iron.
C  this is calculated last for the deepest cell

           Fe_burial(i,j) = PFEflux_l

C  Prepare the tracers for the next layer down

           POPflux_u   = POPflux_l
           PFEflux_u   = PFEflux_l
           CaCO3flux_u = CaCO3flux_l

         ENDIF

        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C  add all tendencies

       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax

C  Dissolved organic matter slow remineralization

#ifdef BLING_NO_NEG
           DOP_remin(i,j,k) = MAX(maskC(i,j,k,bi,bj)*gamma_DOP
     &                    *PTR_DOP(i,j,k),0. _d 0)
#else
           DOP_remin(i,j,k) = maskC(i,j,k,bi,bj)*gamma_DOP
     &                    *PTR_DOP(i,j,k)
#endif

c  Tendencies

           G_PO4(i,j,k) = -P_uptake(i,j,k) + P_recycle(i,j,k)
     &                    + DOP_remin(i,j,k)
     &                    + (1-phi_DOM) * P_reminp(i,j,k)

           G_DOP(i,j,k) = DOP_prod(i,j,k) - DOP_remin(i,j,k)
     &                   + phi_DOM * P_reminp(i,j,k)

           if ( PTR_O2(i,j,k) .GT. oxic_min ) then
             G_O2(i,j,k) = -O2toP*G_PO4(i,j,k)
           else
             G_O2(i,j,k) = 0. _d 0
           endif

           G_FE(i,j,k) = - Fe_uptake(i,j,k) + Fe_reminsum(i,j,k)
     &                   + Fe_recycle(i,j,k)

C  Carbon system diagnostics
C  Change in DIC from primary production, from recycling and
C  remineralization, change in carbonate ions concentration
C  from biological activity:

           G_CaCO3(i,j,k) = CaCO3_diss(i,j,k) - CaCO3_uptake(i,j,k)

           NPP(i,j,k) = P_uptake(i,j,k) * CtoP

           NCP(i,j,k) = -G_PO4(i,j,k)*CtoP

           G_ALK(i,j,k) = 2. _d 0*G_CaCO3(i,j,k) - NtoP*G_PO4(i,j,k)

           G_DIC(i,j,k) = -NCP(i,j,k) + G_CaCO3(i,j,k)

c  Carbon flux diagnostic

           POC_flux(i,j,k) = CtoP * P_spm(i,j,k)

c  for diagnostics: convert to mol C/m3

           Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) * CtoP
           Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) * CtoP

c  for constraints determine POC, assuming that phytoplankton carbon
c  is 30% of POC

           poc(i,j,k,bi,bj) = (Phy_lg_local(i,j,k) +
     &                        Phy_sm_local(i,j,k)) * 3.33333 _d 0

          ENDDO
         ENDDO
       ENDDO

c ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

c 3d global variables
        CALL DIAGNOSTICS_FILL(chl,    'BLGCHL  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(poc,    'BLGPOC  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_mem,'BLGIMEM ',0,Nr,1,bi,bj,myThid)
c 3d local variables
        CALL DIAGNOSTICS_FILL(G_DIC   ,'BLGBIOC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_ALK   ,'BLGBIOAL',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_O2    ,'BLGBIOO2',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_Fe    ,'BLGBIOFE',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_PO4   ,'BLGBIOP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Phy_sm_local,'BLGPSM  ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Phy_lg_local,'BLGPLG  ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(irrk,    'BLGIRRK ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_eff, 'BLGIEFF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(theta_Fe,'BLGCHL2C',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(theta_Fe_inv,'BLGC2CHL',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_lim,  'BLGFELIM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PO4_lim, 'BLGPLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(light_lim,'BLGLLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(POC_flux,'BLGPOCF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NPP,     'BLGNPP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NCP,     'BLGNCP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_spm,  'BLGFESPM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_recycle,'BLGFEREC',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_uptake,'BLGFEUP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DOP_prod, 'BLGDOPP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DOP_remin,'BLGDOPR ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_spm,    'BLGPSPM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_recycle,'BLGPREC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_reminp, 'BLGPREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_uptake, 'BLGPUP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(mu,       'BLGMU   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(CaCO3_diss,'BLGCCdis',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(CaCO3_uptake,'BLGCCpro',0,Nr,2,bi,bj,
     &       myThid)
c 2d local variables
        CALL DIAGNOSTICS_FILL(mld,       'BLGMLD  ',0,1,1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* USE_BLING_V1 */

#endif /* ALLOW_BLING */

      RETURN

      END
