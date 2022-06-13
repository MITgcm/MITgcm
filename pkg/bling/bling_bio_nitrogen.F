#include "BLING_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_BIO_NITROGEN(
     I           PTR_O2, PTR_FE, PTR_PO4, PTR_DOP,
     I           PTR_NO3, PTR_DON,
#ifdef USE_SIBLING
     I           PTR_SI,
#endif
#ifdef ADVECT_PHYTO
     I           PTR_PHY,
#endif
     O           G_DIC, G_ALK, G_O2, G_FE,
     O           G_PO4, G_DOP, G_NO3, G_DON,
#ifdef USE_SIBLING
     O           G_SI,
#endif
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     =================================================================
C     | subroutine bling_bio_nitrogen
C     | o Nutrient uptake and partitioning between organic pools.
C     | - Phytoplankton specific growth rate is calculated
C     |   as a function of light, nutrient limitation, and
C     |   temperature.
C     | - Population growth is calculated as a function of the local
C     |   phytoplankton biomass.
C     | o Organic matter export and remineralization.
C     | - Sinking particulate flux and diel migration contribute to
C     |   export.
C     | - Benthic denitrification
C     | - Iron source from sediments
C     | - Iron scavenging
C     | o Diel Vertical Migration
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
C     phyto_sm     :: small phytoplankton biomass
C     phyto_lg     :: large phytoplankton biomass
C     phyto_diaz   :: diazotroph phytoplankton biomass
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
C     PTR_O2        :: oxygen concentration
C     PTR_FE        :: iron concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_SI        :: silica concentration
C     PTR_PHY       :: total phytoplankton biomass
      _RL     PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL     PTR_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ADVECT_PHYTO
      _RL     PTR_PHY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif

C     === Output ===
C     G_xxx         :: tendency term for tracer xxx
      _RL     G_DIC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_ALK     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_O2      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_FE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_PO4     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DOP     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_NO3     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DON     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL     G_SI      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif

#ifdef ALLOW_BLING
# ifndef USE_BLING_V1
C     === Local variables ===
C     i,j,k         :: loop indices
C     Phy_lg_local  :: biomass in large phytoplankton
C     Phy_sm_local  :: biomass in small phytoplankton
C     Phy_diaz_local:: biomass in diazotroph phytoplankton
C     NO3_lim       :: nitrate limitation
C     PO4_lim       :: phosphate limitation
C     Fe_lim        :: iron limitation for phytoplankton
C     Fe_lim_diaz   :: iron limitation for diazotrophs
C     light_lim     :: light limitation
C     expkT         :: temperature function
C     Pc_m          :: light-saturated max photosynthesis rate for phyto
C     Pc_m_diaz     :: light-saturated max photosynthesis rate for diaz
C     theta_Fe      :: Chl:C ratio
C     theta_Fe_inv  :: C:Chl ratio
C     theta_Fe_max  :: Fe-replete maximum Chl:C ratio
C     irrk          :: nut-limited efficiency of algal photosystems
C     irr_inst      :: instantaneous light
C     irr_eff       :: effective irradiance
C     mld           :: mixed layer depth
C     mu            :: net carbon-specific growth rate for phyt
C     mu_diaz       :: net carbon-specific growth rate for diaz
C     PtoN          :: variable ratio of phosphorus to nitrogen
C     FetoN         :: variable ratio of iron to nitrogen
C     N_uptake      :: NO3 utilization by phytoplankton
C     N_fix         :: Nitrogen fixation by diazotrophs
C     N_den_pelag   :: Pelagic denitrification
C     N_den_benthic :: Benthic denitrification
C     P_uptake      :: PO4 utilization by phytoplankton
C     Fe_uptake     :: dissolved Fe utilization by phytoplankton
C     CaCO3_uptake  :: Calcium carbonate uptake for shell formation
C     CaCO3_diss    :: Calcium carbonate dissolution
C     G_CaCO3       :: tendency of calcium carbonate
C     DON_prod      :: production of dissolved organic nitrogen
C     DOP_prod      :: production of dissolved organic phosphorus
C     DON_remin     :: remineralization of dissolved organic nitrogen
C     DOP_remin     :: remineralization of dissolved organic phosphorus
C     O2_prod       :: production of oxygen
C     frac_exp      :: fraction of sinking particulate organic matter
C     N_spm         :: particulate sinking of nitrogen
C     P_spm         :: particulate sinking of phosphorus
C     Fe_spm        :: particulate sinking of iron
C     N_dvm         :: vertical transport of nitrogen by DVM
C     P_dvm         :: vertical transport of phosphorus by DVM
C     Fe_dvm        :: vertical transport of iron by DVM
C     N_recycle     :: recycling of newly-produced organic nitrogen
C     P_recycle     :: recycling of newly-produced organic phosphorus
C     Fe_recycle    :: recycling of newly-produced organic iron
C     N_reminp      :: remineralization of particulate organic nitrogen
C     P_reminp      :: remineralization of particulate organic nitrogen
C     Fe_reminp     :: remineralization of particulate organic iron
C     Fe_reminsum   :: iron remineralization and adsorption
C     N_remindvm    :: nitrogen remineralization due to diel vertical migration
C     P_remindvm    :: phosphorus remineralization due to diel vertical migration
C     Fe_remindvm   :: iron remineralization due to diel vertical migration
C     POC_flux      :: particulate organic carbon flux
C     NPP           :: net primary production
C     NCP           :: net community production
C     depth_l       :: depth of lower interface
C     *flux_u       :: "*" flux through upper interface
C     *flux_l       :: "*" flux through lower interface
C     zremin        :: remineralization lengthscale for nutrients
C     zremin_caco3  :: remineralization lengthscale for CaCO3
C     wsink         :: speed of sinking particles
C     POC_sed       :: flux of particulate organic carbon to sediment
C     Fe_sed        :: sediment iron efflux
C     kFe_eq_lig    :: iron ligand stability constant
C     FreeFe        :: free (unbound) iron concentration
C     Fe_ads_inorg  :: iron adsorption
C     Fe_ads_org    :: iron adsorption  (2nd order)
C     NO3_btmflx    :: bottom flux of nitrate
C     PO4_btmflx    :: bottom flux of phosphate
C     O2_btmflx     :: bottom flux of oxygen
C     Fe_burial     :: flux of iron to sediment as particulate
C
      INTEGER i,j,k
      _RL Phy_lg_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_sm_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_diaz_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PO4_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL light_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL expkT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Pc_m
      _RL Pc_m_diaz
      _RL theta_Fe_max
      _RL theta_Fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL theta_Fe_inv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irrk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_eff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_inst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mu_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PtoN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL FetoN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_fix(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_den_pelag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_den_benthic(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CaCO3_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CaCO3_diss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL G_CACO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DON_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DOP_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DON_remin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DOP_remin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL O2_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL frac_exp
      _RL N_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_reminsum(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL POC_flux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NCP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL depth_l
      _RL PONflux_u
      _RL PONflux_l
      _RL POPflux_u
      _RL POPflux_l
      _RL PFEflux_u
      _RL PFEflux_l
      _RL CaCO3flux_u
      _RL CaCO3flux_l
      _RL zremin
      _RL zremin_caco3
      _RL wsink
      _RL POC_sed
      _RL Fe_sed(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kFe_eq_lig
      _RL FreeFe
      _RL Fe_ads_inorg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_ads_org(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL log_btm_flx
      _RL NO3_btmflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PO4_btmflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL O2_btmflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fe_burial(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL no3_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL po4_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fe_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL o2_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL don_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dop_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ML_MEAN_PHYTO
      _RL tmp_p_sm_ML
      _RL tmp_p_lg_ML
      _RL tmp_p_diaz_ML
      _RL tmp_ML
#endif
#ifdef USE_BLING_DVM
      INTEGER tmp_dvm
      _RL o2_upper
      _RL o2_lower
      _RL dz_upper
      _RL dz_lower
      _RL temp_upper
      _RL temp_lower
      _RL z_dvm_regr
      _RL frac_migr
      _RL fdvm_migr
      _RL fdvm_stat
      _RL fdvmn_vint
      _RL fdvmp_vint
      _RL fdvmfe_vint
      _RL z_dvm
      _RL dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL x_erfcc,z_erfcc,t_erfcc,erfcc
#endif
#ifdef USE_SIBLING
      _RL Si_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Si_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Si_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Si_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Si_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL SitoN_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_diatom_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL frac_diss_Si(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Pc_m_diatom
      _RL mu_diatom(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL zremin_Si
      _RL Siflux_u
      _RL Siflux_l
      _RL si_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ADVECT_PHYTO
      _RL phy_adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef SIZE_DEP_LIM
      _RL k_NO3_sm
      _RL k_NO3_lg
      _RL k_PO4_sm
      _RL k_PO4_lg
      _RL k_Fe_sm
      _RL k_Fe_lg
      _RL light_lim_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL light_lim_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_lim_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_lim_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PO4_lim_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PO4_lim_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Pc_m_sm
      _RL Pc_m_lg
      _RL mu_sm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mu_lg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itdkey, ijkkey
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      itdkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif

C  Initialize output and diagnostics
       DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            mld(i,j)             = 0. _d 0
            NO3_btmflx(i,j)      = 0. _d 0
            PO4_btmflx(i,j)      = 0. _d 0
            O2_btmflx(i,j)       = 0. _d 0
            Fe_burial(i,j)       = 0. _d 0
         ENDDO
       ENDDO
       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
              G_DIC(i,j,k)          = 0. _d 0
              G_ALK(i,j,k)          = 0. _d 0
              G_O2(i,j,k)           = 0. _d 0
              G_Fe(i,j,k)           = 0. _d 0
              G_PO4(i,j,k)          = 0. _d 0
              G_DOP(i,j,k)          = 0. _d 0
              G_NO3(i,j,k)          = 0. _d 0
              G_DON(i,j,k)          = 0. _d 0
              G_CaCO3(i,j,k)        = 0. _d 0
              Phy_lg_local(i,j,k)   = 0. _d 0
              Phy_sm_local(i,j,k)   = 0. _d 0
              Phy_diaz_local(i,j,k) = 0. _d 0
              NO3_lim(i,j,k)        = 0. _d 0
              PO4_lim(i,j,k)        = 0. _d 0
              Fe_lim(i,j,k)         = 0. _d 0
              Fe_lim_diaz(i,j,k)    = 0. _d 0
              light_lim(i,j,k)      = 0. _d 0
              expkT(i,j,k)          = 0. _d 0
              theta_Fe(i,j,k)       = 0. _d 0
              theta_Fe_inv(i,j,k)   = 0. _d 0
              irrk(i,j,k)           = 0. _d 0
              irr_inst(i,j,k)       = 0. _d 0
              irr_eff(i,j,k)        = 0. _d 0
              mu(i,j,k)             = 0. _d 0
              mu_diaz(i,j,k)        = 0. _d 0
              PtoN(i,j,k)           = 0. _d 0
              FetoN(i,j,k)          = 0. _d 0
              N_uptake(i,j,k)       = 0. _d 0
              N_fix(i,j,k)          = 0. _d 0
              N_den_pelag(i,j,k)    = 0. _d 0
              N_den_benthic(i,j,k)  = 0. _d 0
              P_uptake(i,j,k)       = 0. _d 0
              Fe_uptake(i,j,k)      = 0. _d 0
              CaCO3_uptake(i,j,k)   = 0. _d 0
              CaCO3_diss(i,j,k)     = 0. _d 0
              DON_prod(i,j,k)       = 0. _d 0
              DOP_prod(i,j,k)       = 0. _d 0
              DON_remin(i,j,k)      = 0. _d 0
              DOP_remin(i,j,k)      = 0. _d 0
              O2_prod(i,j,k)        = 0. _d 0
              N_spm(i,j,k)          = 0. _d 0
              P_spm(i,j,k)          = 0. _d 0
              Fe_spm(i,j,k)         = 0. _d 0
              N_dvm(i,j,k)          = 0. _d 0
              P_dvm(i,j,k)          = 0. _d 0
              Fe_dvm(i,j,k)         = 0. _d 0
              N_recycle(i,j,k)      = 0. _d 0
              P_recycle(i,j,k)      = 0. _d 0
              Fe_recycle(i,j,k)     = 0. _d 0
              N_reminp(i,j,k)       = 0. _d 0
              P_reminp(i,j,k)       = 0. _d 0
              Fe_reminp(i,j,k)      = 0. _d 0
              Fe_reminsum(i,j,k)    = 0. _d 0
              N_remindvm(i,j,k)     = 0. _d 0
              P_remindvm(i,j,k)     = 0. _d 0
              Fe_remindvm(i,j,k)    = 0. _d 0
              POC_flux(i,j,k)       = 0. _d 0
              NPP(i,j,k)            = 0. _d 0
              NCP(i,j,k)            = 0. _d 0
              Fe_sed(i,j,k)         = 0. _d 0
              Fe_ads_org(i,j,k)     = 0. _d 0
              Fe_ads_inorg(i,j,k)   = 0. _d 0
#ifdef USE_BLING_DVM
              dvm(i,j,k)            = 0. _d 0
#endif
#ifdef USE_SIBLING
              G_SI(i,j,k)             = 0. _d 0
              Si_lim(i,j,k)           = 0. _d 0
              Si_spm(i,j,k)           = 0. _d 0
              Si_uptake(i,j,k)        = 0. _d 0
              Si_recycle(i,j,k)       = 0. _d 0
              Si_reminp(i,j,k)        = 0. _d 0
              SitoN_uptake(i,j,k)     = 0. _d 0
              Phy_diatom_local(i,j,k) = 0. _d 0
              frac_diss_Si(i,j,k)     = 0. _d 0
              mu_diatom(i,j,k)        = 0. _d 0
#endif
           ENDDO
         ENDDO
       ENDDO

C-----------------------------------------------------------
C  avoid negative nutrient concentrations that can result from
C  advection when low concentrations

#ifdef BLING_NO_NEG
      CALL BLING_MIN_VAL(PTR_O2,  1. _d -11, o2_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_FE,  1. _d -11, fe_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_PO4, 1. _d -8, po4_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_DOP, 1. _d -11, dop_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_NO3, 1. _d -7, no3_adj, bi, bj)
      CALL BLING_MIN_VAL(PTR_DON, 1. _d -11, don_adj, bi, bj)
# ifdef USE_SIBLING
      CALL BLING_MIN_VAL(PTR_SI , 1. _d -7, si_adj, bi, bj)
# endif
# ifdef ADVECT_PHYTO
      CALL BLING_MIN_VAL(PTR_PHY, 1. _d -11, phy_adj, bi, bj)
# endif
#endif

C-----------------------------------------------------------
C  Phytoplankton size classes

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax
#ifdef ADVECT_PHYTO
          Phy_lg_local(i,j,k)   = PTR_PHY(i,j,k)*phyto_lg(i,j,k,bi,bj)
          Phy_sm_local(i,j,k)   = PTR_PHY(i,j,k)*phyto_sm(i,j,k,bi,bj)
          Phy_diaz_local(i,j,k) = PTR_PHY(i,j,k)*phyto_diaz(i,j,k,bi,bj)
#else
          Phy_lg_local(i,j,k)   = phyto_lg(i,j,k,bi,bj)
          Phy_sm_local(i,j,k)   = phyto_sm(i,j,k,bi,bj)
          Phy_diaz_local(i,j,k) = phyto_diaz(i,j,k,bi,bj)
#endif
        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C  Mixed layer depth calculation for light, phytoplankton and dvm.
C  Do not need to calculate if not using ML_MEAN_LIGHT, ML_MEAN_PHYTO,
C  and USE_BLING_DVM
C  (with BLING_ADJOINT_SAFE flag, USE_BLING_DVM is undefined)

#if ( defined (ML_MEAN_LIGHT) || \
      defined (ML_MEAN_PHYTO) || \
      defined (USE_BLING_DVM) )
      CALL BLING_MIXEDLAYER(
     U                         mld,
     I                         bi, bj, imin, imax, jmin, jmax,
     I                         myTime, myIter, myThid)
# ifdef ALLOW_AUTODIFF_TAMC
C     save result of s/r bling_mixedlayer to avoid calling it in AD-code
CADJ STORE mld = comlev1_bibj, key=itdkey, kind=isbyte
# endif
#endif

C  Phytoplankton mixing
C  The mixed layer is assumed to homogenize vertical gradients of phytoplankton.
C  This allows for basic Sverdrup dynamics in a qualitative sense.
C  This has not been thoroughly tested, and care should be
C  taken with its interpretation.

#ifdef ML_MEAN_PHYTO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Phy_sm_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_lg_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_diaz_local = comlev1_bibj, key=itdkey, kind=isbyte
#endif
      DO j=jmin,jmax
       DO i=imin,imax

        tmp_p_sm_ML = 0. _d 0
        tmp_p_lg_ML = 0. _d 0
        tmp_p_diaz_ML = 0. _d 0
        tmp_ML  = 0. _d 0

        DO k=1,Nr

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
          IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.MLmix_max)) THEN
           tmp_p_sm_ML = tmp_p_sm_ML+Phy_sm_local(i,j,k)*drF(k)
     &                  *hFacC(i,j,k,bi,bj)
           tmp_p_lg_ML = tmp_p_lg_ML+Phy_lg_local(i,j,k)*drF(k)
     &                  *hFacC(i,j,k,bi,bj)
           tmp_p_diaz_ML = tmp_p_diaz_ML+Phy_diaz_local(i,j,k)*drF(k)
     &                  *hFacC(i,j,k,bi,bj)
           tmp_ML = tmp_ML + drF(k)
          ENDIF
         ENDIF

        ENDDO

        DO k=1,Nr

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
          IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.MLmix_max)) THEN

           Phy_lg_local(i,j,k)   = max(1. _d -8,tmp_p_lg_ML/tmp_ML)
           Phy_sm_local(i,j,k)   = max(1. _d -8,tmp_p_sm_ML/tmp_ML)
           Phy_diaz_local(i,j,k) = max(1. _d -8,tmp_p_diaz_ML/tmp_ML)

          ENDIF
         ENDIF

        ENDDO
       ENDDO
      ENDDO

#endif

#ifdef USE_SIBLING
      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax
         Phy_diatom_local(i,j,k) = Phy_lg_local(i,j,k)
        ENDDO
       ENDDO
      ENDDO
#endif

C-----------------------------------------------------------
C  light availability for biological production

      CALL BLING_LIGHT(
     I                    mld,
     U                    irr_inst, irr_eff,
     I                    bi, bj, imin, imax, jmin, jmax,
     I                    myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
C     save results of s/r bling_light to avoid calling it in AD-code
CADJ STORE irr_inst = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE irr_eff  = comlev1_bibj, key=itdkey, kind=isbyte
#endif

C  phytoplankton photoadaptation to local light level
C  This represents the fact that phytoplankton cells are adapted
C  to the averaged light field to which they've been exposed over
C  their lifetimes, rather than the instantaneous light. The
C  timescale is set by gamma_irr_mem.

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

          irr_mem(i,j,k,bi,bj) = irr_mem(i,j,k,bi,bj) +
     &           (irr_eff(i,j,k) - irr_mem(i,j,k,bi,bj))*
     &           min( 1. _d 0, gamma_irr_mem*PTRACERS_dTLev(k) )

        ENDDO
       ENDDO
      ENDDO

C ---------------------------------------------------------------------
C  Nutrient uptake and partitioning between organic pools

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

C ---------------------------------------------------------------------
C  First, calculate the limitation terms for NUT and Fe, and the
C  Fe-limited Chl:C maximum. The light-saturated maximal photosynthesis
C  rate term (Pc_m) is simply the product of a prescribed maximal
C  photosynthesis rate (Pc_0), the Eppley temperature dependence, and a
C  resource limitation term. The iron limitation term has a lower limit
C  of Fe_lim_min and is scaled by (k_Fe2P + Fe2P_max) / Fe2P_max so that
C  it approaches 1 as Fe approaches infinity. Thus, it is of comparable
C  magnitude to the macro-nutrient limitation term.

C  Macro-nutrient limitation

          NO3_lim(i,j,k) = PTR_NO3(i,j,k)/(PTR_NO3(i,j,k)+k_NO3)

          PO4_lim(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4)

C  Iron limitation

          Fe_lim(i,j,k) = PTR_FE(i,j,k)
     &                    / (PTR_FE(i,j,k)+k_Fe_2d(i,j,bi,bj))

          Fe_lim_diaz(i,j,k) = PTR_FE(i,j,k)
     &                         / (PTR_FE(i,j,k)+k_Fe_diaz_2d(i,j,bi,bj))

#ifdef USE_SIBLING
          Si_lim(i,j,k) = PTR_Si(i,j,k)/(PTR_Si(i,j,k)+k_Si)
#endif

C ---------------------------------------------------------------------
C  Temperature functionality of growth and grazing

C  NB The temperature function of Eppley (1972) is used to represent the
C  effects of temperature on uptake, synthesis and ecosystem function.
C  The reference value is 0 C. Note that this differs slightly from the
C  more common Arrhenius form of the equation, but by a tiny amount (less
C  than 4% difference over the range -2 to 32 C for an Arrhenius constant
C  (Ae) of 5150).

          expkT(i,j,k) = exp(kappa_eppley * theta(i,j,k,bi,bj))

C  The light-saturated maximal photosynthesis rate term (pc_m) is
C  the product of a prescribed maximal photosynthesis rate (pc_0), the
C  Eppley temperature dependence, and a co-limitation term (the product
C  of Michaelis-Menten no3-limitation, po4-limitation, or iron-limitation).
C  Note this was altered from the Liebig limitation of BLINGv0, following
C  the arguments of Danger (Oikos, 2008) and Harpole (Ecol. Letters 2011).
C  Taking the cube root, to provide the geometric mean.

C  Geometric mean is causing instability in b-SOSE. Returning to Liebig

#ifdef MIN_NUT_LIM
          Pc_m = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * min(NO3_lim(i,j,k), PO4_lim(i,j,k), Fe_lim(i,j,k))
     &           * maskC(i,j,k,bi,bj)
#else
          Pc_m = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * (NO3_lim(i,j,k) * PO4_lim(i,j,k) * Fe_lim(i,j,k))
     &           **(1./3.) * maskC(i,j,k,bi,bj)
#endif

C  Diazotrophs are assumed to be more strongly temperature sensitive,
C  given their observed restriction to relatively warm waters. Presumably
C  this is because of the difficulty of achieving N2 fixation in an oxic
C  environment. Thus, they have lower pc_0 and higher kappa_eppley.
C  Taking the square root, to provide the geometric mean.

#ifdef MIN_NUT_LIM
          Pc_m_diaz = Pc_0_diaz_2d(i,j,bi,bj)
     &           * exp(kappa_eppley_diaz * theta(i,j,k,bi,bj))
     &           * min(PO4_lim(i,j,k), Fe_lim_diaz(i,j,k))
     &           * maskC(i,j,k,bi,bj)
#else
          Pc_m_diaz = Pc_0_diaz_2d(i,j,bi,bj)
     &           * exp(kappa_eppley_diaz * theta(i,j,k,bi,bj))
     &           * (PO4_lim(i,j,k) * Fe_lim_diaz(i,j,k))**(1./2.)
     &           * maskC(i,j,k,bi,bj)
#endif

C  Diatoms are limited by the abundance of silica
C  Note: this is not in original SiBLING code

#ifdef USE_SIBLING
# ifdef MIN_NUT_LIM
          Pc_m_diatom = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * min(NO3_lim(i,j,k), PO4_lim(i,j,k), Fe_lim(i,j,k),
     &           Si_lim(i,j,k)) * maskC(i,j,k,bi,bj)
# else
          Pc_m_diatom = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * (NO3_lim(i,j,k) * PO4_lim(i,j,k) * Fe_lim(i,j,k)
     &           * Si_lim(i,j,k)) **(1./4.) * maskC(i,j,k,bi,bj)
# endif
#endif

C  (Pc_m and Pc_m_diaz crash adjoint if get too small)
#ifdef BLING_ADJOINT_SAFE
          Pc_m        = max(Pc_m       ,maskC(i,j,k,bi,bj)*1. _d -15)
          Pc_m_diaz   = max(Pc_m_diaz  ,maskC(i,j,k,bi,bj)*1. _d -15)
# ifdef USE_SIBLING
          Pc_m_diatom = max(Pc_m_diatom,maskC(i,j,k,bi,bj)*1. _d -15)
# endif
#endif

C ---------------------------------------------------------------------
C  Fe limitation reduces the maximum achievable Chl:C ratio (theta_Fe)
C  below a prescribed, Fe-replete maximum value (theta_Fe_max),
C  to approach a prescribed minimum Chl:C (theta_Fe_min) under extreme
C  Fe-limitation.

          theta_Fe_max = theta_Fe_max_lo+
     &                  (theta_Fe_max_hi-theta_Fe_max_lo)*Fe_lim(i,j,k)

          theta_Fe(i,j,k) = theta_Fe_max
     &                  / (1. _d 0 + alpha_photo_2d(i,j,bi,bj)
     &                  *theta_Fe_max
     &                  *irr_mem(i,j,k,bi,bj)/(epsln + 2. _d 0*Pc_m))

C  for diagnostics: C:Chl ratio in g C / g Chl

          IF ( theta_Fe(i,j,k) .EQ.0. ) THEN
             theta_Fe_inv(i,j,k) = 0.
          ELSE
             theta_Fe_inv(i,j,k) = 1./theta_Fe(i,j,k)
          ENDIF

C ---------------------------------------------------------------------
C  Nutrient-limited efficiency of algal photosystems, irrk, is calculated
C  with the iron limitation term included as a multiplier of the
C  theta_Fe_max to represent the importance of Fe in forming chlorophyll
C  accessory antennae, which do not affect the Chl:C but still affect the
C  phytoplankton ability to use light (eg Stzrepek & Harrison, Nature 2004).
C  Use same thetamax for diazotrophs, since their extra Fe requirement
C  is for nitrogenase, not for the photosystem.

          irrk(i,j,k) = Pc_m
     &             /(epsln + alpha_photo_2d(i,j,bi,bj)*theta_Fe_max)
     &             + irr_mem(i,j,k,bi,bj)/2. _d 0

          light_lim(i,j,k) = ( 1. _d 0 - exp(-irr_eff(i,j,k)
     &               /(epsln + irrk(i,j,k))))

C  Carbon-specific photosynthesis rate

          mu(i,j,k) = Pc_m * light_lim(i,j,k)

#ifdef USE_SIBLING
          mu_diatom(i,j,k) = Pc_m_diatom * light_lim(i,j,k)
#endif

C  Temperature threshold for diazotrophs

          IF (theta(i,j,k,bi,bj) .gt. 14) THEN
           mu_diaz(i,j,k) = Pc_m_diaz * light_lim(i,j,k)
          ELSE
           mu_diaz(i,j,k) = 0. _d 0
          ENDIF

C  Stoichiometry

          PtoN(i,j,k) = PtoN_min + (PtoN_max - PtoN_min) *
     &                  PTR_PO4(i,j,k) / (k_PtoN + PTR_PO4(i,j,k))

          FetoN(i,j,k) = FetoN_min + (FetoN_max - FetoN_min) *
     &                   PTR_FE(i,j,k) / (k_FetoN + PTR_FE(i,j,k))

#ifdef USE_SIBLING
C  Si uptake by diatoms varies with both Si-limitation and growth-rate
C  limitation by other factors: increasing Si limitation decreases Si
C  uptake, but growth limitation by other factors under high Si conditions
C  results in increase Si:P of uptake.

          SitoN_uptake(i,j,k) = min(SitoN_uptake_max,
     &                   Si_lim(i,j,k) * max(SitoN_uptake_min,
     &                   (1. / epsln + SitoN_uptake_scale + min(
     &                   PO4_lim(i,j,k), Fe_lim(i,j,k) ) * (1. -
     &              exp(-irr_eff(i,j,k)/(epsln + irrk(i,j,k) )) ))
     &                   **SitoN_uptake_exp))
#endif

C  Nutrient uptake

          N_uptake(i,j,k) = mu(i,j,k) * (Phy_sm_local(i,j,k)
     &                      + Phy_lg_local(i,j,k))

CAV(
#ifdef SIZE_DEP_LIM

CAV Darwin (uM)
C kPO4: sm/lg 2.4e-2 4.5e-2
C kNO3: 0.4, 0.73
C kSi: 0, 2
C kFe: 3e-5, 5.6e-5

CAV Topaz (mol/kg)
C kPO4: 2e-7, 6e-7
C kNO3: 2e-6, 6e-6
C kFe: 3e-9, 9e-9

CAV BLING (mol/kg)
C kPO4: 1e-8
C kNO3: 2e-6
C kFe: 1.6e-10

          k_NO3_sm = 2.0 _d -6 / permil
          k_NO3_lg = 6.0 _d -6 / permil
          k_PO4_sm = 1.0 _d -8 / permil
          k_PO4_lg = 3.0 _d -8 / permil
          k_Fe_sm = 1.6 _d -10 / permil
          k_Fe_lg = 4.8 _d -10 / permil

          NO3_lim_sm(i,j,k) = PTR_NO3(i,j,k)/(PTR_NO3(i,j,k)+k_NO3_sm)

          PO4_lim_sm(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4_sm)

          Fe_lim_sm(i,j,k) = PTR_FE(i,j,k)/(PTR_FE(i,j,k)+k_Fe_sm)

          NO3_lim_lg(i,j,k) = PTR_NO3(i,j,k)/(PTR_NO3(i,j,k)+k_NO3_lg)

          PO4_lim_lg(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4_lg)

          Fe_lim_lg(i,j,k) = PTR_FE(i,j,k)/(PTR_FE(i,j,k)+k_Fe_lg)

          Pc_m_sm = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * min(NO3_lim_sm(i,j,k), PO4_lim_sm(i,j,k),
     &           Fe_lim_sm(i,j,k)) * maskC(i,j,k,bi,bj)

          Pc_m_lg = Pc_0_2d(i,j,bi,bj) * expkT(i,j,k)
     &           * min(NO3_lim_lg(i,j,k), PO4_lim_lg(i,j,k),
     &           Fe_lim_lg(i,j,k)) * maskC(i,j,k,bi,bj)

          light_lim_sm(i,j,k) = light_lim(i,j,k)

          light_lim_lg(i,j,k) = light_lim(i,j,k)

          mu_sm(i,j,k) = Pc_m_sm * light_lim_sm(i,j,k)
          mu_lg(i,j,k) = Pc_m_lg * light_lim_lg(i,j,k)

          N_uptake(i,j,k) = mu_sm(i,j,k) * Phy_sm_local(i,j,k)
     &                      + mu_lg(i,j,k) * Phy_lg_local(i,j,k)

#endif /* SIZE_DEP_LIM */
CAV)

          N_fix(i,j,k) = mu_diaz(i,j,k) * Phy_diaz_local(i,j,k)

          P_uptake(i,j,k) = (N_uptake(i,j,k) +
     &                      N_fix(i,j,k)) * PtoN(i,j,k)

          Fe_uptake(i,j,k) = (N_uptake(i,j,k) +
     &                       N_fix(i,j,k)) * FetoN(i,j,k)

#ifdef USE_SIBLING
          Si_uptake(i,j,k) = mu(i,j,k) * Phy_diatom_local(i,j,k)
     &                       * SitoN_uptake(i,j,k)
#endif

C  Calcium carbonate production

C  Alkalinity is consumed through the production of CaCO3. Here, this is
C  simply a linear function of the implied growth rate of small
C  phytoplankton, which gave a reasonably good fit to the global
C  observational synthesis of Dunne (2009). This is consistent
C  with the findings of Jin et al. (GBC,2006).

          CaCO3_uptake(i,j,k) = mu(i,j,k) * Phy_sm_local(i,j,k)
     &                          * phi_sm_2d(i,j,bi,bj) * CatoN

CAV(
#ifdef SIZE_NUT_LIM
          CaCO3_uptake(i,j,k) = mu_sm(i,j,k) * Phy_sm_local(i,j,k)
     &                          * phi_sm_2d(i,j,bi,bj) * CatoN
#endif
CAV)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

C ---------------------------------------------------------------------
C  Grazing is treated using the size-dependent grazing laws of
C  Dunne et al. (GBC, 2005), with alpha=1/3 (eq. 5b):
C   lambda_small = lambda0 * e^kT * (small/pivotal)
C   lambda_large = lambda0 * e^hT * (large/pivotal)^alpha
C  where small/large are the biomasses of small and large phytoplankton,
C  and lambda is mortality. The constants were determined empirically
C  from a global dataset.
C  Basically, small phytoplankton have a linear increase in mortality
C  with biomass, reflecting the rapid increase of small zooplankton as
C  growth rates increase, while large zooplankton respond weakly to
C  increases in the growth rates of large phytoplankton. This produces
C  a simple bloom dynamic.
C  Because diazotrophs have specialized grazers (O'Neil and Roman, 1992;
C  Mulholland, Biogeosciences 2007), they are treated independently from
C  other phytoplankton. This has an important impact, reducing direct
C  competition between diazotrophs and other phytoplankton.
C  Note that the phytoplankton concentrations here are not part of the
C  conserved nutrient pools: they are only used to keep track of the
C  biomass, which is required to calculate the uptake fluxes.

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax
         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

          Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) +
     &                Phy_lg_local(i,j,k)*(mu(i,j,k) - lambda_0
     &                * expkT(i,j,k) *
     &                (Phy_lg_local(i,j,k)/pivotal)**(1. / 3.))
     &                * PTRACERS_dTLev(k)

          Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) +
     &                Phy_sm_local(i,j,k)*(mu(i,j,k) - lambda_0
     &                * expkT(i,j,k) * (Phy_sm_local(i,j,k)/pivotal))
     &                * PTRACERS_dTLev(k)

          Phy_diaz_local(i,j,k) = Phy_diaz_local(i,j,k) +
     &                Phy_diaz_local(i,j,k)*(mu_diaz(i,j,k) - lambda_0
     &                * expkT(i,j,k) * (Phy_diaz_local(i,j,k)/pivotal))
     &                * PTRACERS_dTLev(k)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

#if ( ( defined SIZE_DEP_LIM ) || ( defined USE_SIBLING ) )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Phy_sm_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_lg_local   = comlev1_bibj, key=itdkey, kind=isbyte
#endif
      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax
         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
CAV(
#ifdef SIZE_DEP_LIM

          Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) +
     &                Phy_lg_local(i,j,k)*(mu_lg(i,j,k) - lambda_0
     &                * expkT(i,j,k) *
     &                (Phy_lg_local(i,j,k)/pivotal)**(1. / 3.))
     &                * PTRACERS_dTLev(k)

          Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) +
     &                Phy_sm_local(i,j,k)*(mu_sm(i,j,k) - lambda_0
     &                * expkT(i,j,k) * (Phy_sm_local(i,j,k)/pivotal))
     &                * PTRACERS_dTLev(k)

#endif
CAV)

#ifdef USE_SIBLING
C  For now: assuming that all large phyto are diatoms
C  Another option would be to have a separate diatom population and give
C  them a higher growth rate to be competitive. Would also need to add
C  a global variable "phyto_diatom"
          Phy_diatom_local(i,j,k) = Phy_lg_local(i,j,k)

c          Phy_diatom_local(i,j,k) = Phy_diatom_local(i,j,k) +
c     &                Phy_diatom_local(i,j,k)*(mu_diatom(i,j,k)-lambda_0
c     &                * expkT(i,j,k) * (Phy_diatom_local(i,j,k)/pivotal))
c     &                * PTRACERS_dTLev(k)
#endif

         ENDIF
        ENDDO
       ENDDO
      ENDDO
#endif /* SIZE_DEP_LIM or USE_SIBLING */

C  Separate loop for adjoint stores
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Phy_sm_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_lg_local   = comlev1_bibj, key=itdkey, kind=isbyte
CADJ STORE Phy_diaz_local = comlev1_bibj, key=itdkey, kind=isbyte
#endif

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

#ifdef ADVECT_PHYTO
C  update tracer here
          PTR_PHY(i,j,k) = Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k)
     &                     + Phy_diaz_local(i,j,k)
C  update fractional abundance
          phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)/PTR_PHY(i,j,k)
          phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)/PTR_PHY(i,j,k)
          phyto_diaz(i,j,k,bi,bj) = Phy_diaz_local(i,j,k)/PTR_PHY(i,j,k)
#else
C  update biomass
          phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)
          phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)
          phyto_diaz(i,j,k,bi,bj) = Phy_diaz_local(i,j,k)
#endif

C  use the diagnostic biomass to calculate the chl concentration
C  in mg/m3 (carbon = 12.01 g/mol)

          chl(i,j,k,bi,bj) = max(chl_min, CtoN * 12.01 * 1. _d 3 *
     &           theta_Fe(i,j,k) *
     &           (Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k)
     &           + Phy_diaz_local(i,j,k)))

         ENDIF
        ENDDO
       ENDDO
      ENDDO

C ---------------------------------------------------------------------
C  Partitioning between organic pools

C  The uptake of nutrients is assumed to contribute to the growth of
C  phytoplankton, which subsequently die and are consumed by heterotrophs.
C  This can involve the transfer of nutrient elements between many
C  organic pools, both particulate and dissolved, with complex histories.
C  We take a simple approach here, partitioning the total uptake into two
C  fractions - sinking and non-sinking - as a function of temperature,
C  following Dunne et al. (2005).
C  Then, the non-sinking fraction is further subdivided, such that the
C  majority is recycled instantaneously to the inorganic nutrient pool,
C  representing the fast turnover of labile dissolved organic matter via
C  the microbial loop, and the remainder is converted to semi-labile
C  dissolved organic matter. Iron and macro-nutrient are treated
C  identically for the first step, but all iron is recycled
C  instantaneously in the second step (i.e. there is no dissolved organic
C  iron pool).
C  Assume that the fraction of uptake that produces sinking particulate
C  organic matter is the same for diazotrophic and non-diazotrophic
C  phytoplankton.

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

C  sinking fraction: particulate organic matter

#ifdef NEW_FRAC_EXP
CAV fraction that does into POM is
CAV (assume diaz has same export frac as small phyto)
CAV e^kT * [phi_lg * B_lg/Btot + phi_sm * B_sm/Btot + phi_sm * Bdiaz/Bto]

          frac_exp = (phi_sm_2d(i,j,bi,bj) * (phyto_sm(i,j,k,bi,bj) +
     &      phyto_diaz(i,j,k,bi,bj)) + phi_lg_2d(i,j,bi,bj) *
     &      phyto_lg(i,j,k,bi,bj)) /
     &      (phyto_sm(i,j,k,bi,bj) + phyto_diaz(i,j,k,bi,bj) +
     &      phyto_lg(i,j,k,bi,bj)) *
     &      exp(kappa_remin * theta(i,j,k,bi,bj))

#else

CAV this is from blingv1, where B_sm and B_lg are diagnosed from
CAV the value of mu...

          frac_exp = (phi_sm_2d(i,j,bi,bj) + phi_lg_2d(i,j,bi,bj) *
     &           (mu(i,j,k)/(epsln + lambda_0*expkT(i,j,k)))**2)/
     &           (1. + (mu(i,j,k)/
     &           (epsln + lambda_0*expkT(i,j,k)))**2)*
     &           exp(kappa_remin * theta(i,j,k,bi,bj))

#endif

#ifdef USE_BLING_DVM
          N_dvm(i,j,k) = phi_dvm * frac_exp *
     &             (N_uptake(i,j,k) + N_fix(i,j,k))

          P_dvm(i,j,k) = phi_dvm * frac_exp * P_uptake(i,j,k)

          Fe_dvm(i,j,k) = phi_dvm * frac_exp * Fe_uptake(i,j,k)

          N_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) *
     &                 (N_uptake(i,j,k) + N_fix(i,j,k))

          P_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) *
     &                 P_uptake(i,j,k)

          Fe_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) *
     &                  Fe_uptake(i,j,k)
#else
          N_spm(i,j,k) = frac_exp * (N_uptake(i,j,k) + N_fix(i,j,k))
          P_spm(i,j,k) = frac_exp * P_uptake(i,j,k)
          Fe_spm(i,j,k) = frac_exp * Fe_uptake(i,j,k)

          N_dvm(i,j,k) = 0
          P_dvm(i,j,k) = 0
          Fe_dvm(i,j,k) = 0
#endif

C  the remainder is divided between instantaneously recycled and
C  long-lived dissolved organic matter.

          DON_prod(i,j,k) = phi_DOM_2d(i,j,bi,bj)*(N_uptake(i,j,k)
     &                      + N_fix(i,j,k) - N_spm(i,j,k)
     &                      - N_dvm(i,j,k))

          DOP_prod(i,j,k) = phi_DOM_2d(i,j,bi,bj)*(P_uptake(i,j,k)
     &                      - P_spm(i,j,k) - P_dvm(i,j,k))

          N_recycle(i,j,k) = N_uptake(i,j,k) + N_fix(i,j,k)
     &                       - N_spm(i,j,k) - DON_prod(i,j,k)
     &                       - N_dvm(i,j,k)

          P_recycle(i,j,k) = P_uptake(i,j,k)
     &                       - P_spm(i,j,k) - DOP_prod(i,j,k)
     &                       - P_dvm(i,j,k)

          Fe_recycle(i,j,k) = Fe_uptake(i,j,k)
     &                        - Fe_spm(i,j,k) - Fe_dvm(i,j,k)

#ifdef USE_SIBLING
          frac_diss_Si(i,j,k) = exp(-SitoN_uptake(i,j,k) /
     &                        (q_SitoN_diss *
     &                        exp(kappa_remin_Si * theta(i,j,k,bi,bj))))

          Si_recycle(i,j,k) = Si_uptake(i,j,k) * frac_diss_Si(i,j,k)

          Si_spm(i,j,k) = Si_uptake(i,j,k) - Si_recycle(i,j,k)
#endif

         ENDIF

        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C     these stores avoid recomputing the previous loop, but avoiding
C     that may not be necessary
CADJ STORE N_spm     = comlev1_bibj, key = itdkey, kind = isbyte
CADJ STORE P_spm     = comlev1_bibj, key = itdkey, kind = isbyte
CADJ STORE Fe_spm    = comlev1_bibj, key = itdkey, kind = isbyte
CADJ STORE don_prod  = comlev1_bibj, key = itdkey, kind = isbyte
CADJ STORE n_recycle = comlev1_bibj, key = itdkey, kind = isbyte
#endif

C ---------------------------------------------------------------------
C  Remineralization
C  In general, the flux at the bottom of a grid cell should equal
C  Fb = (Ft + Prod*dz) / (1 + zremin*dz)
C  where Ft is the flux at the top, and prod*dz is the integrated
C  production of new sinking particles within the layer.
C  This is also multiplied by the fraction of the grid cell that is
C  not intercepted by the bottom at this level, according to the
C  subgridscale sediment interception fraction.
C  Since Ft=0 in the first layer,

C$TAF LOOP = parallel
      DO j=jmin,jmax
C$TAF LOOP = parallel
       DO i=imin,imax

C  Initialize upper flux

        PONflux_u            = 0. _d 0
        POPflux_u            = 0. _d 0
        PFEflux_u            = 0. _d 0
        CaCO3flux_u          = 0. _d 0
#ifdef USE_SIBLING
        Siflux_u             = 0. _d 0
#endif

        DO k=1,Nr
#ifdef ALLOW_AUTODIFF_TAMC
         ijkkey = (itdkey - 1) * (2*OLx+sNx) * (2*OLy+sNy) * Nr
     &        +   ( (j-1) * (2*OLx+sNx) + (i-1) ) * Nr + k
CADJ STORE PONflux_u   = comlev1_bibj_ijk, key = ijkkey, kind = isbyte
CADJ STORE POPflux_u   = comlev1_bibj_ijk, key = ijkkey, kind = isbyte
CADJ STORE PFEflux_u   = comlev1_bibj_ijk, key = ijkkey, kind = isbyte
CADJ STORE CaCO3flux_u = comlev1_bibj_ijk, key = ijkkey, kind = isbyte
# ifdef USE_SIBLING
CADJ STORE PONflux_u   = comlev1_bibj_ijk, key = ijkkey, kind = isbyte
# endif
#endif

C  Initialization here helps taf

         Fe_ads_org(i,j,k)    = 0. _d 0

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

C  Calculate the remineralization lengthscale matrix, zremin, a function
C  of z. Sinking rate (wsink) is constant over the upper wsink0_z metres,
C  then  increases linearly with depth.
C  The remineralization rate is a function of oxygen concentrations,
C  following a Holling type 2 dependence, decreasing to a minimum value
C  of remin_min. This is ad hoc, following work by Bianchi, Sarmiento,
C  Galbraith and Kwon (unpublished).
C  Sinking speed is evaluated at the bottom of the cell

          depth_l=-rF(k+1)
          IF (depth_l .LE. wsink0z)  THEN
           wsink = wsink0_2d(i,j,bi,bj)
          ELSE
           wsink = wsinkacc * (depth_l - wsink0z) + wsink0_2d(i,j,bi,bj)
          ENDIF

          zremin = gamma_POM_2d(i,j,bi,bj) * ( PTR_O2(i,j,k)**2 /
     &               (k_O2**2 + PTR_O2(i,j,k)**2) * (1-remin_min)
     &               + remin_min )/(wsink + epsln)

C  Calcium remineralization relaxed toward the inverse of the
C  ca_remin_depth constant value as the calcite saturation approaches 0.

          zremin_caco3 = 1. _d 0/ca_remin_depth*(1. _d 0 - min(1. _d 0,
     &               omegaC(i,j,k,bi,bj) + epsln ))

#ifdef USE_SIBLING
          zremin_Si = gamma_Si_0 / wsink_Si
#endif

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

#ifdef USE_SIBLING
          Siflux_l = (Siflux_u+Si_spm(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin_Si*drF(k)
     &           *hFacC(i,j,k,bi,bj))
#endif

C  Check if we are not a bottom cell

          IF ( k.NE.kLowC(i,j,bi,bj) ) THEN
C  Start with cells that are not the deepest cells

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

#ifdef USE_SIBLING
           Si_reminp(i,j,k) = (Siflux_u + Si_spm(i,j,k)*drF(k)
     &                     - Siflux_l)*recip_drF(k)
#endif

           Fe_sed(i,j,k) = 0. _d 0

          ELSE
C  Now do bottom layer

C  If this layer is adjacent to bottom topography or it is the deepest
C  cell of the domain, then remineralize/dissolve in this grid cell
C  i.e. do not subtract off lower boundary fluxes when calculating remin

           N_reminp(i,j,k) = PONflux_u*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj) + N_spm(i,j,k)

           P_reminp(i,j,k) = POPflux_u*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj) + P_spm(i,j,k)

           CaCO3_diss(i,j,k) = CaCO3flux_u*recip_drF(k)
     &                  *recip_hFacC(i,j,k,bi,bj) + CaCO3_uptake(i,j,k)

#ifdef USE_SIBLING
           Si_reminp(i,j,k) = Siflux_u*recip_drF(k)
     &                  *recip_hFacC(i,j,k,bi,bj) + Si_spm(i,j,k)
#endif

C  Efflux Fed out of sediments
C  The phosphate flux hitting the bottom boundary
C  is used to scale the return of iron to the water column.
C  Maximum value added for numerical stability.

           POC_sed = PONflux_l * CtoN

           Fe_sed(i,j,k) = max(epsln, FetoC_sed * POC_sed * recip_drF(k)
     &            *recip_hFacC(i,j,k,bi,bj))

           log_btm_flx = 1. _d -20

CMM: this is causing instability in the adjoint. Needs debugging
#ifndef BLING_ADJOINT_SAFE
           IF (POC_sed .gt. 0. _d 0) THEN

C  Convert from mol N m-2 s-1 to umol C cm-2 d-1 and take the log

            log_btm_flx = log10(min(43.0 _d 0, POC_sed *
     &           86400. _d 0 * 100.0 _d 0))

C  Metamodel gives units of umol C cm-2 d-1, convert to mol N m-2 s-1 and
C  multiply by no3_2_n to give NO3 consumption rate

            N_den_benthic(i,j,k) = min (POC_sed * NO3toN / CtoN,
     &         (10 _d 0)**(-0.9543 _d 0 + 0.7662 _d 0 *
     &         log_btm_flx - 0.235 _d 0 * log_btm_flx * log_btm_flx)
     &         / (CtoN * 86400. _d 0 * 100.0 _d 0) * NO3toN *
     &         PTR_NO3(i,j,k) / (k_no3 + PTR_NO3(i,j,k)) ) *
     &         recip_drF(k)

           ENDIF
#endif

C  ---------------------------------------------------------------------
C  Calculate external bottom fluxes. Positive fluxes
C  are into the water column from the seafloor. For P, the bottom flux puts
C  the sinking flux reaching the bottom cell into the water column through
C  diffusion. For iron, the sinking flux disappears into the sediments if
C  bottom waters are oxic (assumed adsorbed as oxides). If bottom waters are
C  anoxic, the sinking flux of Fe is returned to the water column.
C
C  For oxygen, the consumption of oxidant required to respire the settling flux
C  of organic matter (in support of the no3 bottom flux) diffuses from the
C  bottom water into the sediment.

C  Assume all NO3 for benthic denitrification is supplied from the bottom water,
C  and that all organic N is also consumed under denitrification (Complete
C  Denitrification, sensu Paulmier, Biogeosciences 2009). Therefore, no NO3 is
C  regenerated from organic matter respired by benthic denitrification
C  (necessitating the second term in b_no3).

           NO3_btmflx(i,j) = PONflux_l*drF(k)*hFacC(i,j,k,bi,bj)
     &                   - N_den_benthic(i,j,k) / NO3toN

           PO4_btmflx(i,j) = POPflux_l*drF(k)*hFacC(i,j,k,bi,bj)

C  Oxygen flux into sediments is that required to support non-denitrification
C  respiration, assuming a 4/5 oxidant ratio of O2 to NO3. Oxygen consumption
C  is allowed to continue at negative oxygen concentrations, representing
C  sulphate reduction.

           O2_btmflx(i,j) = -( O2toN*PONflux_l*drF(k)*hFacC(i,j,k,bi,bj)
     &                  - N_den_benthic(i,j,k)* 1.25)

          ENDIF

C  Calculate free and inorganically associated iron concentration for
C  scavenging. We assume that there is a spectrum of iron ligands present
C  in seawater, with  varying binding strengths and whose composition
C  varies with light and iron concentrations. For example,
C  photodissocation of ligand complexes occurs under bright light,
C  weakening the binding strength (e.g. Barbeau et al., Nature 2001),
C  while at very low iron concentrations (order kfe_eq_lig_femin),
C  siderophores are thought to be produced as a response to extreme iron
C  stress. In anoxic waters, iron should be reduced, and therefore mostly
C  immune to scavenging; therefore, the feprime calculation is not made
C  if oxygen is less than 0.

          kFe_eq_lig = kFe_eq_lig_max-(kFe_eq_lig_max-kFe_eq_lig_min)
     &             *(irr_inst(i,j,k)**2
     &             /(kFe_eq_lig_irr**2+irr_inst(i,j,k)**2))
     &             *max(epsln,min(1. _d 0,(PTR_FE(i,j,k)
     &             -kFe_eq_lig_Femin)/
     &             (PTR_FE(i,j,k)+epsln)*1.2 _d 0))

          FreeFe = (-(1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))
     &            +((1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))**2+4*
     &            kFe_eq_lig*PTR_FE(i,j,k))**(0.5))/(2*
     &            kFe_eq_lig)

          IF (PTR_O2(i,j,k) .LT. oxic_min)  THEN
           FreeFe = 0. _d 0
          ENDIF

          Fe_ads_inorg(i,j,k) =
     &       kFe_inorg*(max(1. _d -8,FreeFe))**(1.5)

C  Calculate the Fe adsorption using this PONflux_l:
C  The absolute first order rate constant is calculated from the
C  concentration of organic particles, after Parekh et al. (2005).
C  (Galbraith's code limits the value to 1/2dt for numerical stability).

          IF ( PONflux_l .GT. 0. _d 0 ) THEN
            Fe_ads_org(i,j,k) =
     &           kFE_org*(PONflux_l/(epsln + wsink)
     &             * MasstoN)**(0.58)*FreeFe
          ENDIF

          PFEflux_l = (PFEflux_u+(Fe_spm(i,j,k)+Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &            *hFacC(i,j,k,bi,bj))

C  Added the burial flux of sinking particulate iron here as a
C  diagnostic, needed to calculate mass balance of iron.
C  this is calculated last for the deepest cell

          Fe_burial(i,j) = PFEflux_l

          IF ( PTR_O2(i,j,k) .LT. oxic_min ) THEN
            PFEflux_l = 0. _d 0
          ENDIF

          Fe_reminp(i,j,k) = (PFEflux_u+(Fe_spm(i,j,k)
     &            +Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj)-PFEflux_l)*recip_drF(k)
     &            *recip_hFacC(i,j,k,bi,bj)

          Fe_reminsum(i,j,k) = Fe_reminp(i,j,k) + Fe_sed(i,j,k)
     &                 - Fe_ads_org(i,j,k) - Fe_ads_inorg(i,j,k)

C  Prepare the tracers for the next layer down

          PONflux_u   = PONflux_l
          POPflux_u   = POPflux_l
          PFEflux_u   = PFEflux_l
          CaCO3flux_u = CaCO3flux_l
#ifdef USE_SIBLING
          Siflux_u    = Siflux_l
#endif

         ENDIF

        ENDDO
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE N_reminp = comlev1_bibj, key=itdkey, kind=isbyte
#endif
C-----------------------------------------------------------
C  remineralization from diel vertical migration
#ifdef USE_BLING_DVM

C  DIEL VERTICAL MIGRATOR EXPORT
C  The effect of vertically-migrating animals on the export flux of organic
C  matter from the ocean surface is treated similarly to the scheme of
C  Bianchi et al., Nature Geoscience 2013.
C  This involves calculating the stationary depth of vertical migrators, using
C  an empirical multivariate regression, and ensuring that this remains
C  above the bottom as well as any suboxic waters.
C  The total DVM export flux is partitioned between a swimming migratory
C  component and the stationary component, and these are summed.

C$TAF LOOP = parallel
      DO j=jmin,jmax
C$TAF LOOP = parallel
       DO i=imin,imax

C  Initialize
        o2_upper = 0.
        o2_lower = 0.
        dz_upper = 0.
        dz_lower = 0.
        temp_upper = 0.
        temp_lower = 0.
        z_dvm_regr = 0.
        z_dvm     = 0.
        frac_migr = 0.
        fdvm_migr = 0.
        fdvm_stat = 0.
        fdvmn_vint = 0.
        fdvmp_vint = 0.
        fdvmfe_vint = 0.

        DO k=1,Nr

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

C  Calculate the depth of migration based on linear regression.

          depth_l=-rF(k+1)

C  Average temperature and oxygen over upper 35 m, and 140-515m.
C  Also convert O2 to mmol m-3.

          if ( abs(depth_l) .lt. 35.) then
            dz_upper = dz_upper + drf(k)
            temp_upper = temp_upper + theta(i,j,k,bi,bj)*drf(k)
            o2_upper = o2_upper + PTR_O2(i,j,k) * drf(k)*1.0 _d 3
          endif
          if ( (abs(depth_l) .gt. 140.0 _d 0) .and.
     &          (abs(depth_l) .lt. 515. _d 0)) then
            dz_lower = dz_lower + drf(k)
            temp_lower = temp_lower + theta(i,j,k,bi,bj)*drf(k)
            o2_lower = o2_lower + PTR_O2(i,j,k) * drf(k)*1.0 _d 3
          endif

         ENDIF
        ENDDO

        o2_upper = o2_upper / (epsln + dz_upper)
        temp_upper = temp_upper / (epsln + dz_upper)
        o2_lower = o2_lower / (epsln + dz_lower)
        temp_lower = temp_lower / (epsln + dz_lower)

C  Calculate the regression, using the constants given in Bianchi et al. (2013).
C  The variable values are bounded to lie within reasonable ranges:
C         O2 gradient   : [-10,300] mmol/m3
C         Log10 Chl     : [-1.8,0.85] log10(mg/m3)
C         mld           : [0,500] m
C         T gradient    : [-3,20] C

        z_dvm_regr = 398. _d 0
     &   - 0.56 _d 0*min(300. _d 0,max(-10. _d 0,(o2_upper - o2_lower)))
     &   - 115. _d 0*min(0.85 _d 0,max(-1.80 _d 0,
     &   log10(max(chl(i,j,1,bi,bj),chl_min))))
     &   + 0.36 _d 0*min(500. _d 0,max(epsln,mld(i,j)))
     &   - 2.40 _d 0*min(20. _d 0,max(-3. _d 0,(temp_upper-temp_lower)))

C  Limit the depth of migration in polar winter.
C  Use irr_mem since this is averaged over multiple days, dampening the
C  diurnal cycle.
C  Tapers Z_DVM to the minimum when surface irradince is below a given
C  threshold (here 10 W/m2).

        if ( irr_mem(i,j,1,bi,bj) .lt. 10. ) then
          z_dvm_regr = 150. _d 0 + (z_dvm_regr - 150. _d 0) *
     &       irr_mem(i,j,1,bi,bj) / 10. _d 0
        endif

C  Check for suboxic water within the column. If found, set dvm
C  stationary depth to 2 layers above it. This is not meant to
C  represent a cessation of downward migration, but rather the
C  requirement for aerobic DVM respiration to occur above the suboxic
C  water, where O2 is available.

        tmp_dvm = 0
        DO k=1,Nr-2

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS .AND. tmp_dvm.EQ.0 ) THEN

          z_dvm = -rf(k+1)
          if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp_dvm = 1

         ENDIF

        ENDDO

C  The stationary depth is constrained between 150 and 700, above any
C  anoxic waters found, and above the bottom.

        z_dvm = min(700. _d 0,max(150. _d 0,z_dvm_regr),z_dvm,-rf(k+1))

C  Calculate the fraction of migratory respiration that occurs
C  during upwards and downwards swimming. The remainder is
C  respired near the stationary depth.
C  Constants for swimming speed and resting time are hard-coded
C  after Bianchi et al, Nature Geoscience 2013.

        frac_migr = max( 0.0 _d 0, min( 1.0 _d 0, (2.0 _d 0 * z_dvm) /
     &        (epsln + 0.05 _d 0 * 0.5 _d 0 * 86400. _d 0)))

C  Calculate the vertical profile shapes of DVM fluxes.
C  These are given as the downward organic flux due to migratory
C  DVM remineralization, defined at the bottom of each layer k.

        tmp_dvm = 0
        DO k=1,Nr

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS .AND. tmp_dvm.EQ.0 ) THEN

C  First, calculate the part due to active migration above
C  the stationary depth.
          if (-rf(k+1) .lt. z_dvm) then
            fdvm_migr = frac_migr / (epsln + z_dvm - (-rf(2))) *
     &            (z_dvm - (-rf(k+1)) )
          else
            fdvm_migr  = 0.0
          endif

C  Then, calculate the part at the stationary depth.

C  Approximation of the complementary error function
C  From Numerical Recipes (F90, Ch. 6, p. 216)
C  Returns the complementary error function erfc(x)
C  with fractional error everywhere less than 1.2e-7

          x_erfcc = (-rf(k) - z_dvm) /
     &        ( (epsln + 2. _d 0 * sigma_dvm**2. _d 0)**0.5)

          z_erfcc = abs(x_erfcc)

          t_erfcc = 1. _d 0/(1. _d 0+0.5 _d 0*z_erfcc)

          erfcc = t_erfcc*exp(-z_erfcc*z_erfcc-1.26551223+t_erfcc*
     &       (1.00002368+t_erfcc*(0.37409196+t_erfcc*
     &       (.09678418+t_erfcc*(-.18628806+t_erfcc*(.27886807+
     &       t_erfcc*(-1.13520398+t_erfcc*(1.48851587+
     &       t_erfcc*(-0.82215223+t_erfcc*0.17087277)))))))))

          if (x_erfcc .lt. 0.0) then
            erfcc = 2.0 - erfcc
          endif

          fdvm_stat = (1. _d 0 - frac_migr) / 2. _d 0 * erfcc

C  Add the shapes, resulting in the 3-d DVM flux operator. If the
C  current layer is the bottom layer, or the layer beneath the
C  underlying layer is suboxic, all fluxes at and below the current
C  layer remain at the initialized value of zero. This will cause all
C  remaining DVM remineralization to occur in this layer.

          IF (k.LT.Nr-1) THEN
            if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp_dvm = 1
          ENDIF
c!!          if (k .eq. grid_kmt(i,j)) exit
          dvm(i,j,k)  = fdvm_migr + fdvm_stat

         ENDIF

        ENDDO

C  Sum up the total organic flux to be transported by DVM

        do k = 1, Nr
          fdvmn_vint  = fdvmn_vint  + N_dvm(i,j,k)  * drf(k)
          fdvmp_vint  = fdvmp_vint  + P_dvm(i,j,k)  * drf(k)
          fdvmfe_vint = fdvmfe_vint + Fe_dvm(i,j,k) * drf(k)
        enddo

C  Calculate the remineralization terms as the divergence of the flux

        N_remindvm(i,j,1)  = fdvmn_vint *  (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))
        P_remindvm(i,j,1)  = fdvmp_vint *  (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))
        Fe_remindvm(i,j,1) = fdvmfe_vint * (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))

        do k = 2, Nr
          N_remindvm(i,j,k)  = fdvmn_vint  *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
          P_remindvm(i,j,k)  = fdvmp_vint  *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
          Fe_remindvm(i,j,k) = fdvmfe_vint *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
        enddo

       enddo
      enddo

#endif

C-----------------------------------------------------------

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

C  Dissolved organic matter slow remineralization

#ifdef BLING_NO_NEG
          DON_remin(i,j,k) = MAX(maskC(i,j,k,bi,bj)*gamma_DON
     &                       *PTR_DON(i,j,k),0. _d 0)

          DOP_remin(i,j,k) = MAX(maskC(i,j,k,bi,bj)*gamma_DOP
     &                       *PTR_DOP(i,j,k),0. _d 0)
#else
          DON_remin(i,j,k) = maskC(i,j,k,bi,bj)*gamma_DON
     &                       *PTR_DON(i,j,k)

          DOP_remin(i,j,k) = maskC(i,j,k,bi,bj)*gamma_DOP
     &                       *PTR_DOP(i,j,k)
#endif

C  Pelagic denitrification
C  If anoxic

          IF (PTR_O2(i,j,k) .lt. oxic_min) THEN
           IF (PTR_NO3(i,j,k) .gt. oxic_min) THEN
            N_den_pelag(i,j,k) = max(epsln, (NO3toN *
     &                    ((1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                    * (N_reminp(i,j,k)
     &                    + N_remindvm(i,j,k)) + DON_remin(i,j,k) +
     &                    N_recycle(i,j,k))) - N_den_benthic(i,j,k))
           ENDIF
          ENDIF

C  oxygen production through photosynthesis

          O2_prod(i,j,k) = O2toN * N_uptake(i,j,k)
     &                     + (O2toN - 1.25 _d 0) * N_fix(i,j,k)

C-----------------------------------------------------------
C  ADD TERMS

C  Nutrients
C  Sum of fast recycling, decay of sinking POM, and decay of DOM,
C  less uptake, (less denitrification).

          G_PO4(i,j,k) = -P_uptake(i,j,k) + P_recycle(i,j,k)
     &                   + (1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                   * (P_reminp(i,j,k)
     &                   + P_remindvm(i,j,k)) + DOP_remin(i,j,k)

          G_NO3(i,j,k) = -N_uptake(i,j,k)
          IF (PTR_O2(i,j,k) .lt. oxic_min) THEN
C  Anoxic
           G_NO3(i,j,k) = G_NO3(i,j,k)
     &                    - N_den_pelag(i,j,k) - N_den_benthic(i,j,k)
          ELSE
C  Oxic
           G_NO3(i,j,k) = G_NO3(i,j,k)
     &                   + N_recycle(i,j,k)
     &                   + (1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                   * (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                   + DON_remin(i,j,k)
     &                   - N_den_benthic(i,j,k)
          ENDIF

C  Iron
C  remineralization, sediments and adsorption are all bundled into
C  Fe_reminsum

          G_FE(i,j,k) = -Fe_uptake(i,j,k) + Fe_reminsum(i,j,k)
     &                  + Fe_remindvm(i,j,k) + Fe_recycle(i,j,k)

C  Dissolved Organic Matter
C  A fraction of POM remineralization goes into dissolved pools.

          G_DON(i,j,k) = DON_prod(i,j,k) + phi_DOM_2d(i,j,bi,bj)
     &                   * (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                   - DON_remin(i,j,k)

          G_DOP(i,j,k) = DOP_prod(i,j,k) + phi_DOM_2d(i,j,bi,bj)
     &                   * (P_reminp(i,j,k) + P_remindvm(i,j,k))
     &                   - DOP_remin(i,j,k)

C  Oxygen:
C  Assuming constant O2:N ratio in terms of oxidant required per mol of
C  organic N. This implies a constant stoichiometry of C:N and H:N (where H is
C  reduced, organic H). Because the N provided by N2 fixation is reduced from
C  N2, rather than NO3-, the o2_2_n_fix is slightly less than the NO3- based
C  ratio (by 1.25 mol O2/ mol N). Account for the organic matter respired
C  through benthic denitrification by subtracting 5/4 times the benthic
C  denitrification NO3 utilization rate from the overall oxygen consumption.

          G_O2(i,j,k) = O2_prod(i,j,k)
C  If oxic
          IF (PTR_O2(i,j,k) .gt. oxic_min) THEN
           G_O2(i,j,k) = G_O2(i,j,k)
     &                  -O2toN * ((1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                  * (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                  + DON_remin(i,j,k) +  N_recycle(i,j,k))
     &                  + N_den_benthic(i,j,k) * 1.25 _d 0
C  If anoxic but NO3 concentration is very low
C  (generate negative O2; proxy for HS-).
          ELSEIF (PTR_NO3(i,j,k) .lt. oxic_min) THEN
           G_O2(i,j,k) = G_O2(i,j,k)
     &                  -O2toN * ((1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                  * (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                  + DON_remin(i,j,k) +  N_recycle(i,j,k))
          ENDIF

#ifdef USE_SIBLING
          G_Si(i,j,k) = -Si_uptake(i,j,k) + Si_recycle(i,j,k)
     &                  + Si_reminp(i,j,k)
#endif

C  Carbon flux diagnostic

          POC_flux(i,j,k) = CtoN * N_spm(i,j,k)

          NPP(i,j,k) = (N_uptake(i,j,k) + N_fix(i,j,k)) * CtoN

          NCP(i,j,k) = (N_uptake(i,j,k) + N_fix(i,j,k)
     &                   - N_recycle(i,j,k)
     &                   - (1. _d 0 - phi_DOM_2d(i,j,bi,bj))
     &                   * (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                   - DON_remin(i,j,k) ) * CtoN

          G_CaCO3(i,j,k) = CaCO3_diss(i,j,k) - CaCO3_uptake(i,j,k)

          G_ALK(i,j,k) = - G_NO3(i,j,k)
     &              + 2. _d 0*G_CaCO3(i,j,k)

          G_DIC(i,j,k) = -NCP(i,j,k) + G_CaCO3(i,j,k)

C  for diagnostics: convert to mol C/m3

          Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) * CtoN
          Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) * CtoN
          Phy_diaz_local(i,j,k) = Phy_diaz_local(i,j,k) * CtoN

C  for constraints determine POC, assuming that phytoplankton carbon
C  is 30% of POC

          poc(i,j,k,bi,bj) = (Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k)
     &                     + Phy_diaz_local(i,j,k) ) * 3.33333 _d 0

         ENDIF

        ENDDO
       ENDDO
      ENDDO

C ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

C 3d global variables
        CALL DIAGNOSTICS_FILL(chl,    'BLGCHL  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(poc,    'BLGPOC  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_mem,'BLGIMEM ',0,Nr,1,bi,bj,myThid)
C 3d local variables
        CALL DIAGNOSTICS_FILL(G_DIC   ,'BLGBIOC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_ALK   ,'BLGBIOAL',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_O2    ,'BLGBIOO2',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_Fe    ,'BLGBIOFE',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_PO4   ,'BLGBIOP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(G_NO3   ,'BLGBION ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Phy_sm_local,'BLGPSM  ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Phy_lg_local,'BLGPLG  ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Phy_diaz_local,'BLGPDIA ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(irrk,    'BLGIRRK ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_eff, 'BLGIEFF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_inst,'BLGIRRIS',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(theta_Fe,'BLGCHL2C',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(theta_Fe_inv,'BLGC2CHL',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_lim,  'BLGFELIM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NO3_lim, 'BLGNLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PO4_lim, 'BLGPLIM ',0,Nr,2,bi,bj,myThid)
#ifdef USE_SIBLING
        CALL DIAGNOSTICS_FILL(G_SI    ,'BLGBIOSI',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Si_lim,  'BLGSILIM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Si_uptake,'BLGSIUP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Si_recycle,'BLGSIREC',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Si_reminp,'BLGSIREM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(SitoN_uptake,'BLGSI2N ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(frac_diss_Si,'BLGSIDIS',0,Nr,2,bi,bj,
     &       myThid)
#endif
        CALL DIAGNOSTICS_FILL(light_lim,'BLGLLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(POC_flux,'BLGPOCF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PtoN,    'BLGP2N  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(FetoN,   'BLGFE2N ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NPP,     'BLGNPP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NCP,     'BLGNCP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_dvm,  'BLGFEDVM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_spm,  'BLGFESPM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_recycle,  'BLGFEREC',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_remindvm, 'BLGFERD ',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_uptake,'BLGFEUP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_den_benthic,'BLGNDENB',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(N_den_pelag, 'BLGNDENP',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(N_dvm,    'BLGNDVM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_fix,    'BLGNFIX ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DON_prod, 'BLGDONP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DON_remin,'BLGDONR ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_spm,    'BLGNSPM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_recycle,'BLGNREC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_remindvm,'BLGNRD  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_reminp, 'BLGNREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_uptake, 'BLGNUP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_dvm,    'BLGPDVM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DOP_prod, 'BLGDOPP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(DOP_remin,'BLGDOPR ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_spm,    'BLGPSPM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_recycle,'BLGPREC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_remindvm,'BLGPRD  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_reminp, 'BLGPREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_uptake, 'BLGPUP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(mu,       'BLGMU   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(mu_diaz,  'BLGMUDIA',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(CaCO3_diss,  'BLGCCdis',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(CaCO3_uptake,'BLGCCpro',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_ads_org,  'BLGFEADO',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_ads_inorg,'BLGFEADI',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_sed,    'BLGFESED',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_reminp, 'BLGFEREM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_reminp,  'BLGNREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_reminp,  'BLGPREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(no3_adj,   'BLGNONEN',0,Nr,2,bi,bj,myThid)
C 2d local variables
        CALL DIAGNOSTICS_FILL(mld,       'BLGMLD  ',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_burial, 'BLGFEBUR',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NO3_btmflx,'BLGNSED ',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PO4_btmflx,'BLGPSED ',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(O2_btmflx, 'BLGO2SED',0,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* USE_BLING_V1 */

#endif /* ALLOW_BLING */

      RETURN
      END
