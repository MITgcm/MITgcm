C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_production.F,v 1.7 2016/10/20 21:37:06 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_PROD( 
     I           PTR_NO3, PTR_PO4, PTR_FE, 
     I           PTR_O2, PTR_DON, PTR_DOP,
#ifdef ADVECT_PHYTO
     I           PTR_PHY,
#endif 
     O           G_NO3, G_PO4, G_FE, 
     O           G_O2, G_DON, G_DOP, 
     O           G_CACO3, NCP,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myIter, myTime, myThid )

C     =================================================================
C     | subroutine bling_prod
C     | o Nutrient uptake and partitioning between organic pools.
C     | - Phytoplankton specific growth rate is calculated 
C     |   as a function of light, nutrient limitation, and  
C     |   temperature. 
C     | - Population growth is calculated as a function of the local 
C     |   phytoplankton biomass.
C     =================================================================

      implicit none
      
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
C     PTR_PO4       :: phosphate concentration
C     PTR_FE        :: iron concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_O2        :: oxygen concentration
C     PTR_PHYTO     :: total phytoplankton biomass
      _RL     PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ADVECT_PHYTO
      _RL     PTR_PHY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif

C     === Output ===
C     G_xxx         :: Tendency of xxx
      _RL     G_NO3     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_PO4     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_FE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_O2      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DON     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_DOP     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     G_CACO3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_BLING
C     === Local variables ===
C     i,j,k         :: loop indices
C     Phy_lg_local  :: biomass in large phytoplankton 
C     Phy_sm_local  :: biomass in small phytoplankton 
C     Phy_diaz_local:: biomass in diazotroph phytoplankton 
C     NO3_lim       :: nitrate limitation 
C     PO4_lim       :: phosphate limitation           
C     Fe_lim        :: iron limitation for phytoplankton    
C     Fe_lim_diaz   :: iron limitation for diazotrophs
C     alpha_Fe      :: initial slope of the P-I curve
C     theta_Fe      :: Chl:C ratio 
C     theta_Fe_max  :: Fe-replete maximum Chl:C ratio  
C     irrk          :: nut-limited efficiency of algal photosystems
C     irr_inst      :: instantaneous light
C     irr_eff       :: effective irradiance
C     mld           :: mixed layer depth
C     Pc_m          :: light-saturated max photosynthesis rate for phyt
C     Pc_m_diaz     :: light-saturated max photosynthesis rate for diaz
C     Pc_tot        :: carbon-specific photosynthesis rate
C     expkT         :: temperature function
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
C     DON_prod      :: production of dissolved organic nitrogen
C     DON_remin     :: remineralization of dissolved organic nitrogen
C     DOP_prod      :: production of dissolved organic phosphorus
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
C     Fe_reminsum   :: iron remineralization and adsorption
C     N_remindvm    :: nitrogen remineralization due to diel vertical migration
C     P_remindvm    :: phosphorus remineralization due to diel vertical migration
C     Fe_remindvm   :: iron remineralization due to diel vertical migration
C     POC_flux      :: particulate organic carbon flux
C     NPP           :: net primary production
C     NCP           :: net community production
C
      INTEGER i,j,k        
      _RL Phy_lg_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_sm_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Phy_diaz_local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PO4_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_lim_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL expkT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Pc_m
      _RL Pc_m_diaz
      _RL theta_Fe_max       
      _RL theta_Fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irrk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)  
      _RL irr_inst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_eff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
      _RL Fe_reminsum(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL POC_flux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NCP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ML_MEAN_PHYTO     
      _RL tmp_p_sm_ML
      _RL tmp_p_lg_ML
      _RL tmp_p_diaz_ML
      _RL tmp_ML
#endif
CEOP

c  Initialize output and diagnostics
        DO j=jmin,jmax
          DO i=imin,imax
            mld(i,j) = 0. _d 0
          ENDDO
        ENDDO
       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              G_NO3(i,j,k)        = 0. _d 0
              G_PO4(i,j,k)        = 0. _d 0
              G_Fe(i,j,k)         = 0. _d 0
              G_O2(i,j,k)         = 0. _d 0
              G_DON(i,j,k)        = 0. _d 0
              G_DOP(i,j,k)        = 0. _d 0
              G_CaCO3(i,j,k)      = 0. _d 0
              N_uptake(i,j,k)     = 0. _d 0
              N_fix(i,j,k)        = 0. _d 0
              N_den_pelag(i,j,k)  = 0. _d 0
              N_den_benthic(i,j,k)= 0. _d 0
              P_uptake(i,j,k)     = 0. _d 0
              Fe_uptake(i,j,k)    = 0. _d 0
              N_dvm(i,j,k)        = 0. _d 0
              P_dvm(i,j,k)        = 0. _d 0
              Fe_dvm(i,j,k)       = 0. _d 0
              CaCO3_uptake(i,j,k) = 0. _d 0
              DON_prod(i,j,k)     = 0. _d 0
              DOP_prod(i,j,k)     = 0. _d 0
              O2_prod(i,j,k)      = 0. _d 0
              mu_diaz(i,j,k)      = 0. _d 0
              irr_eff(i,j,k)      = 0. _d 0
              irr_inst(i,j,k)     = 0. _d 0
              irrk(i,j,k)         = 0. _d 0
              NO3_lim(i,j,k)      = 0. _d 0
              PO4_lim(i,j,k)      = 0. _d 0
              Fe_lim(i,j,k)       = 0. _d 0
              Fe_lim_diaz(i,j,k)  = 0. _d 0
              PtoN(i,j,k)         = 0. _d 0
              FetoN(i,j,k)        = 0. _d 0
              NPP(i,j,k)          = 0. _d 0
              N_reminp(i,j,k)     = 0. _d 0
              P_reminp(i,j,k)     = 0. _d 0
              Fe_reminsum(i,j,k)  = 0. _d 0
              N_remindvm(i,j,k)   = 0. _d 0
              P_remindvm(i,j,k)   = 0. _d 0
          ENDDO
        ENDDO
       ENDDO


c-----------------------------------------------------------
c  avoid negative nutrient concentrations that can result from 
c  advection when low concentrations

#ifdef BLING_NO_NEG
      CALL BLING_MIN_VAL( PTR_NO3, 1. _d -7 ,bi ,bj)
      CALL BLING_MIN_VAL( PTR_PO4, 1. _d -8 ,bi ,bj)
      CALL BLING_MIN_VAL( PTR_FE,  1. _d -11 ,bi ,bj)
      CALL BLING_MIN_VAL( PTR_O2,  1. _d -11 ,bi ,bj)
      CALL BLING_MIN_VAL( PTR_DON, 1. _d -11 ,bi ,bj)
      CALL BLING_MIN_VAL( PTR_DOP, 1. _d -11 ,bi ,bj)
#endif

c-----------------------------------------------------------
c  Phytoplankton size classes
c
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

c-----------------------------------------------------------
c  mixed layer depth calculation for light, phytoplankton and dvm
c  do not need to calculate if not using ML_MEAN_LIGHT, ML_MEAN_PHYTO
c  and not using ML calculation in bling_dvm
c  (with BLING_ADJOINT_SAFE flag, MLD is assumed constant in bling_dvm)

#if ( defined (ML_MEAN_LIGHT) || \
      defined (ML_MEAN_PHYTO) || \
      !(defined (FIXED_MLD_DVM)) )
       CALL BLING_MIXEDLAYER(
     U                         mld, 
     I                         bi, bj, imin, imax, jmin, jmax,
     I                         myIter, myTime, myThid)
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
        tmp_p_diaz_ML = 0. _d 0
        tmp_ML  = 0. _d 0

        DO k=1,Nr

         IF (hFacC(i,j,k,bi,bj).gt.0. _d 0) THEN
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.200. _d 0)) THEN
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

         IF (hFacC(i,j,k,bi,bj).gt.0. _d 0) THEN
         IF ((-rf(k+1) .le. mld(i,j)).and.
     &               (-rf(k+1).lt.200. _d 0)) THEN

          Phy_lg_local(i,j,k)   = max(1. _d -8,tmp_p_lg_ML/tmp_ML)
          Phy_sm_local(i,j,k)   = max(1. _d -8,tmp_p_sm_ML/tmp_ML)
          Phy_diaz_local(i,j,k) = max(1. _d -8,tmp_p_diaz_ML/tmp_ML)

         ENDIF
         ENDIF

        ENDDO
       ENDDO
      ENDDO

#endif


c-----------------------------------------------------------
c  light availability for biological production
       CALL BLING_LIGHT(
     I               mld,
     U               irr_inst, irr_eff,
     I               bi, bj, imin, imax, jmin, jmax,
     I               myIter, myTime, myThid )

c  phytoplankton photoadaptation to local light level
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

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax  

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c ---------------------------------------------------------------------
c  First, calculate the limitation terms for NUT and Fe, and the 
c  Fe-limited Chl:C maximum. The light-saturated maximal photosynthesis 
c  rate term (Pc_m) is simply the product of a prescribed maximal 
c  photosynthesis rate (Pc_0), the Eppley temperature dependence, and a 
c  resource limitation term. The iron limitation term has a lower limit 
c  of Fe_lim_min and is scaled by (k_Fe2P + Fe2P_max) / Fe2P_max so that 
c  it approaches 1 as Fe approaches infinity. Thus, it is of comparable 
c  magnitude to the macro-nutrient limitation term.

c  Macro-nutrient limitation
          NO3_lim(i,j,k) = PTR_NO3(i,j,k)/(PTR_NO3(i,j,k)+k_NO3)

          PO4_lim(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4)

c  Iron limitation

          Fe_lim(i,j,k) = PTR_FE(i,j,k) / (PTR_FE(i,j,k)+k_Fe)
         
          Fe_lim_diaz(i,j,k) = PTR_FE(i,j,k) / (PTR_FE(i,j,k)+k_Fe_diaz)

c ---------------------------------------------------------------------
c Diazotrophs are assumed to be more strongly temperature sensitive,
c given their observed restriction to relatively warm waters. Presumably
c this is because of the difficulty of achieving N2 fixation in an oxic 
c environment. Thus, they have lower pc_0 and higher kappa_eppley. 
c Taking the square root, to provide the geometric mean.

          expkT(i,j,k) = exp(kappa_eppley * theta(i,j,k,bi,bj))   
 

c  Light-saturated maximal photosynthesis rate   

          Pc_m = Pc_0 * expkT(i,j,k)
     &           * min(NO3_lim(i,j,k), PO4_lim(i,j,k), Fe_lim(i,j,k))
     &           * maskC(i,j,k,bi,bj)

          Pc_m_diaz = Pc_0_diaz
     &           * exp(kappa_eppley_diaz * theta(i,j,k,bi,bj))
     &           * min(PO4_lim(i,j,k), Fe_lim_diaz(i,j,k))
     &           * maskC(i,j,k,bi,bj)

c  (Pc_m and Pc_m_diaz crash adjoint if get too small)
#ifdef BLING_ADJOINT_SAFE
          Pc_m      = max(Pc_m     ,maskC(i,j,k,bi,bj)*1. _d -15)
          Pc_m_diaz = max(Pc_m_diaz,maskC(i,j,k,bi,bj)*1. _d -15)
#endif


c ---------------------------------------------------------------------
c  Fe limitation 1) reduces photosynthetic efficiency (alpha_Fe)
c  and 2) reduces the maximum achievable Chl:C ratio (theta_Fe) 
c  below a prescribed, Fe-replete maximum value (theta_Fe_max), 
c  to approach a prescribed minimum Chl:C (theta_Fe_min) under extreme
c  Fe-limitation.

          theta_Fe_max = theta_Fe_max_lo+
     &                  (theta_Fe_max_hi-theta_Fe_max_lo)*Fe_lim(i,j,k)
     
          theta_Fe(i,j,k) = theta_Fe_max/(1. _d 0 + alpha_photo
     &                  *theta_Fe_max
     &                  *irr_mem(i,j,k,bi,bj)/(epsln + 2. _d 0*Pc_m))

c ---------------------------------------------------------------------
c  Nutrient-limited efficiency of algal photosystems, irrk, is calculated 
c  with the iron limitation term included as a multiplier of the 
c  theta_Fe_max to represent the importance of Fe in forming chlorophyll
c  accessory antennae, which do not affect the Chl:C but still affect the
c  phytoplankton ability to use light (eg Stzrepek & Harrison, Nature 2004).

          irrk(i,j,k) = Pc_m/(epsln + alpha_photo*theta_Fe_max) +
     &              irr_mem(i,j,k,bi,bj)/2. _d 0
     
c  Carbon-specific photosynthesis rate     
          mu(i,j,k) = Pc_m * ( 1. _d 0 - exp(-irr_eff(i,j,k)
     &               /(epsln + irrk(i,j,k))))

          mu_diaz(i,j,k) = Pc_m_diaz * ( 1. _d 0 - exp(-irr_eff(i,j,k)
     &               /(epsln + irrk(i,j,k))))

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c  Instantaneous nutrient concentration in phyto biomass
c  Separate loop so adjoint stuff above can be outside loop 
c  (fix for recomputations)
CMM(
CADJ STORE phyto_sm = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_lg = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_diaz  = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_sm_local = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_lg_local = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_diaz_local  = comlev1, key = ikey_dynamics, kind=isbyte
#ifdef ADVECT_PHYTO
CADJ STORE PTR_PHY  = comlev1, key = ikey_dynamics, kind=isbyte
#endif
CMM)
      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

          Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) + 
     &                Phy_lg_local(i,j,k)*(mu(i,j,k) - lambda_0
     &                *expkT(i,j,k) * 
     &                (Phy_lg_local(i,j,k)/pivotal)**(1. / 3.)) 
     &                * PTRACERS_dTLev(k)

          Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) + 
     &                Phy_sm_local(i,j,k)*(mu(i,j,k) - lambda_0
     &                *expkT(i,j,k) * (Phy_sm_local(i,j,k)/pivotal) ) 
     &                * PTRACERS_dTLev(k)
     
          Phy_diaz_local(i,j,k) = Phy_diaz_local(i,j,k) + 
     &                Phy_diaz_local(i,j,k)*(mu_diaz(i,j,k) - lambda_0
     &                *expkT(i,j,k) * (Phy_diaz_local(i,j,k)/pivotal) ) 
     &                * PTRACERS_dTLev(k)

#ifdef ADVECT_PHYTO
c  update tracer here
         PTR_PHY(i,j,k) = Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k) 
     &                     + Phy_diaz_local(i,j,k)
c  update fractional abundance
         phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)/PTR_PHY(i,j,k)
         phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)/PTR_PHY(i,j,k)
         phyto_diaz(i,j,k,bi,bj)  = Phy_diaz_local(i,j,k)/PTR_PHY(i,j,k) 
#else
c  update biomass
         phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)
         phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)
         phyto_diaz(i,j,k,bi,bj) = Phy_diaz_local(i,j,k)

#endif

         ENDIF
        ENDDO
       ENDDO
      ENDDO



CMM(
CADJ STORE phyto_sm = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_lg = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_diaz  = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_sm_local = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_lg_local = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE Phy_diaz_local  = comlev1, key = ikey_dynamics, kind=isbyte
#ifdef ADVECT_PHYTO
CADJ STORE PTR_PHY  = comlev1, key = ikey_dynamics, kind=isbyte
#endif
CMM)



      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
          
c  use the diagnostic biomass to calculate the chl concentration
          chl(i,j,k,bi,bj) = max(chl_min, CtoN * 12.01 * 
     &           theta_Fe(i,j,k) *
     &           (Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k) 
     &           + Phy_diaz_local(i,j,k)))

c  stoichiometry
          PtoN(i,j,k) = PtoN_min + (PtoN_max - PtoN_min) * 
     &                  PTR_PO4(i,j,k) / (k_PtoN + PTR_PO4(i,j,k))

          FetoN(i,j,k) = FetoN_min + (FetoN_max - FetoN_min) *
     &                   PTR_FE(i,j,k) / (k_FetoN + PTR_FE(i,j,k))
              
c  Nutrient uptake
          N_uptake(i,j,k) = mu(i,j,k)*(Phy_sm_local(i,j,k) 
     &                      + Phy_lg_local(i,j,k))

          N_fix(i,j,k) = mu_diaz(i,j,k) * Phy_diaz_local(i,j,k)
             
          P_uptake(i,j,k) = (N_uptake(i,j,k) +
     &                      N_fix(i,j,k)) * PtoN(i,j,k) 
                  
          Fe_uptake(i,j,k) = (N_uptake(i,j,k) + 
     &                       N_fix(i,j,k)) * FetoN(i,j,k) 

c ---------------------------------------------------------------------
c   Alkalinity is consumed through the production of CaCO3. Here, this is
c   simply a linear function of the implied growth rate of small
c   phytoplankton, which gave a reasonably good fit to the global 
c   observational synthesis of Dunne (2009). This is consistent
c   with the findings of Jin et al. (GBC,2006). 

          CaCO3_uptake(i,j,k) = Phy_sm_local(i,j,k) * phi_sm 
     &                          * expkT(i,j,k) * mu(i,j,k) * CatoN
     
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
  
c  sinking fraction: particulate organic matter 

          frac_exp = (phi_sm + phi_lg * (mu(i,j,k)/
     &           (epsln + lambda_0*expkT(i,j,k)))**2.)/  
     &           (1. + (mu(i,j,k)/(epsln + lambda_0*expkT(i,j,k)))**2.)*
     &           exp(kappa_remin * theta(i,j,k,bi,bj))

          N_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) *  
     &                 (N_uptake(i,j,k) + N_fix(i,j,k))

          P_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) * 
     &                 P_uptake(i,j,k)

          Fe_spm(i,j,k) = frac_exp * (1.0 - phi_dvm) * 
     &                  Fe_uptake(i,j,k)
          
          N_dvm(i,j,k) = frac_exp *  
     &             (N_uptake(i,j,k) + N_fix(i,j,k)) - N_spm(i,j,k)
          
          P_dvm(i,j,k) = frac_exp * P_uptake(i,j,k) - 
     &                   P_spm(i,j,k)

          Fe_dvm(i,j,k) = frac_exp * Fe_uptake(i,j,k) -  
     &                    Fe_spm(i,j,k)
        
c  the remainder is divided between instantaneously recycled and
c  long-lived dissolved organic matter. 
    
          DON_prod(i,j,k) = phi_DOM*(N_uptake(i,j,k) 
     &                      + N_fix(i,j,k) - N_spm(i,j,k) 
     &                      - N_dvm(i,j,k))

          DOP_prod(i,j,k) = phi_DOM*(P_uptake(i,j,k) 
     &                      - P_spm(i,j,k) - P_dvm(i,j,k))
                  
          N_recycle(i,j,k) = N_uptake(i,j,k) + N_fix(i,j,k)
     &                       - N_spm(i,j,k) - DON_prod(i,j,k) 
     &                       - N_dvm(i,j,k)

          P_recycle(i,j,k) = P_uptake(i,j,k)
     &                       - P_spm(i,j,k) - DOP_prod(i,j,k) 
     &                       - P_dvm(i,j,k)

          Fe_recycle(i,j,k) = Fe_uptake(i,j,k) 
     &                        - Fe_spm(i,j,k) - Fe_dvm(i,j,k)

         ENDIF
         
        ENDDO
       ENDDO
      ENDDO


c-----------------------------------------------------------
c  remineralization of sinking organic matter
       CALL BLING_REMIN( 
     I                 PTR_NO3, PTR_FE, PTR_O2, irr_inst,
     I                 N_spm, P_spm, Fe_spm, CaCO3_uptake,
     U                 N_reminp, P_reminp, Fe_reminsum,      
     U                 N_den_benthic, CACO3_diss,             
     I                 bi, bj, imin, imax, jmin, jmax, 
     I                 myIter, myTime, myThid)

c-----------------------------------------------------------
c  remineralization from diel vertical migration
       CALL BLING_DVM(
     I                 N_dvm,P_dvm,Fe_dvm,
     I                 PTR_O2, mld,
     O                 N_remindvm, P_remindvm, Fe_remindvm,
     I                 bi, bj, imin, imax, jmin, jmax, 
     I                 myIter, myTime, myThid)
     
     
c-----------------------------------------------------------
c  sub grid scale sediments
#ifdef USE_SGS_SED  
       CALL BLING_SGS(
c     I                 xxx,
c     O                 xxx,
     I                 bi, bj, imin, imax, jmin, jmax, 
     I                 myIter, myTime, myThid)
#endif


c-----------------------------------------------------------
c  

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax  

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c  Dissolved organic matter slow remineralization

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
    
      
c  Pelagic denitrification    
c  If anoxic  
          IF (PTR_O2(i,j,k) .lt. oxic_min) THEN 
           IF (PTR_NO3(i,j,k) .gt. oxic_min) THEN
            N_den_pelag(i,j,k) = max(epsln, (NO3toN *        
     &                    ((1. _d 0 - phi_DOM) * (N_reminp(i,j,k) 
     &                    + N_remindvm(i,j,k)) + DON_remin(i,j,k) + 
     &                    N_recycle(i,j,k))) - N_den_benthic(i,j,k)) 
           ENDIF
          ENDIF
          
c  Carbon flux diagnostic
          POC_flux(i,j,k) = CtoN * N_spm(i,j,k)

          NPP(i,j,k) = (N_uptake(i,j,k) + N_fix(i,j,k)) * CtoN  
          
c  oxygen production through photosynthesis
          O2_prod(i,j,k) = O2toN * N_uptake(i,j,k)               
     &                     + (O2toN - 1.25 _d 0) * N_fix(i,j,k)

   
   
c-----------------------------------------------------------
C  ADD TERMS 
   
c  Nutrients  
c  Sum of fast recycling, decay of sinking POM, and decay of DOM,
c  less uptake, (less denitrification).

          G_PO4(i,j,k) = -P_uptake(i,j,k) + P_recycle(i,j,k) 
     &                   + (1. _d 0 - phi_DOM) * (P_reminp(i,j,k) 
     &                   + P_remindvm(i,j,k)) + DOP_remin(i,j,k)
          
          G_NO3(i,j,k) = -N_uptake(i,j,k) 
          IF (PTR_O2(i,j,k) .lt. oxic_min) THEN 
c  Anoxic 
           G_NO3(i,j,k) = G_NO3(i,j,k) 
     &                    - N_den_pelag(i,j,k) - N_den_benthic(i,j,k)   
          ELSE
c  Oxic
           G_NO3(i,j,k) = G_NO3(i,j,k) 
     &                   + N_recycle(i,j,k) + (1. _d 0 - phi_DOM) * 
     &                   (N_reminp(i,j,k) + N_remindvm(i,j,k))
     &                   + DON_remin(i,j,k)
          ENDIF    

          NCP(i,j,k) = (N_uptake(i,j,k) + N_fix(i,j,k)
     &                   - N_recycle(i,j,k) - (1. _d 0 - phi_DOM) *
     &                   (N_reminp(i,j,k) - N_remindvm(i,j,k))
     &                   - DON_remin(i,j,k) ) * CtoN

c  Iron
c  remineralization, sediments and adsorption are all bundled into 
c  Fe_reminsum

          G_FE(i,j,k) = -Fe_uptake(i,j,k) + Fe_reminsum(i,j,k) 
     &                  + Fe_remindvm(i,j,k) + Fe_recycle(i,j,k) 

c  Dissolved Organic Matter
c  A fraction of POM remineralization goes into dissolved pools.

          G_DON(i,j,k) = DON_prod(i,j,k) + phi_DOM * 
     &                   (N_reminp(i,j,k) + N_remindvm(i,j,k)) 
     &                   - DON_remin(i,j,k)

          G_DOP(i,j,k) = DOP_prod(i,j,k) + phi_DOM * 
     &                   (P_reminp(i,j,k) + P_remindvm(i,j,k)) 
     &                   - DOP_remin(i,j,k)

c  Oxygen:
c  Assuming constant O2:N ratio in terms of oxidant required per mol of 
c  organic N. This implies a constant stoichiometry of C:N and H:N (where H is 
c  reduced, organic H). Because the N provided by N2 fixation is reduced from 
c  N2, rather than NO3-, the o2_2_n_fix is slightly less than the NO3- based 
c  ratio (by 1.25 mol O2/ mol N). Account for the organic matter respired 
c  through benthic denitrification by subtracting 5/4 times the benthic 
c  denitrification NO3 utilization rate from the overall oxygen consumption.

          G_O2(i,j,k) = O2_prod(i,j,k) 
c  If oxic
          IF (PTR_O2(i,j,k) .gt. oxic_min) THEN 
          G_O2(i,j,k) = G_O2(i,j,k) 
     &                  -O2toN * ((1. _d 0 - phi_DOM) * 
     &                  (N_reminp(i,j,k) + N_remindvm(i,j,k))           
     &                  + DON_remin(i,j,k) +  N_recycle(i,j,k))
c  If anoxic but NO3 concentration is very low
c  (generate negative O2; proxy for HS-).
          ELSEIF (PTR_NO3(i,j,k) .lt. oxic_min) THEN
          G_O2(i,j,k) = G_O2(i,j,k) 
     &                  -O2toN * ((1. _d 0 - phi_DOM) * 
     &                  (N_reminp(i,j,k) + N_remindvm(i,j,k))           
     &                  + DON_remin(i,j,k) +  N_recycle(i,j,k))
     &                  + N_den_benthic(i,j,k) * 1.25 _d 0
          ENDIF

          G_CaCO3(i,j,k) = CaCO3_diss(i,j,k) - CaCO3_uptake(i,j,k)
cxx sediments not accounted for

         ENDIF

        ENDDO
       ENDDO
      ENDDO

        
c ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

c 3d global variables
        CALL DIAGNOSTICS_FILL(phyto_sm,'BLGPSM  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(phyto_lg,'BLGPLG  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(phyto_diaz,'BLGPDIA ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(chl,    'BLGCHL  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_mem,'BLGIMEM ',0,Nr,1,bi,bj,myThid)
c 3d local variables
        CALL DIAGNOSTICS_FILL(irrk,    'BLGIRRK ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(irr_eff, 'BLGIEFF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_lim,  'BLGFELIM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NO3_lim, 'BLGNLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(PO4_lim, 'BLGPLIM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(POC_flux,'BLGPOCF ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NPP,     'BLGNPP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(NCP,     'BLGNCP  ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(Fe_ads_inorg,'BLGFEAI',0,Nr,2,bi,bj,
c     &       myThid)
c        CALL DIAGNOSTICS_FILL(Fe_dvm,'BLGFEDVM',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(Fe_sed,'BLGFESED',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_spm,  'BLGFESPM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_recycle,  'BLGFEREC',0,Nr,2,bi,bj,
     &       myThid)
        CALL DIAGNOSTICS_FILL(Fe_remindvm, 'BLGFERD ',0,Nr,2,bi,bj,
     &       myThid)
c        CALL DIAGNOSTICS_FILL(Fe_reminp,'BLGFEREM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Fe_reminsum, 'BLGFEREM',0,Nr,2,bi,bj,
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
        CALL DIAGNOSTICS_FILL(P_spm,    'BLGPSPM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_recycle,'BLGPREC ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_remindvm,'BLGPRD  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_reminp, 'BLGPREM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_uptake, 'BLGPUP  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(mu,       'BLGMU   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(mu_diaz,  'BLGMUDIA',0,Nr,2,bi,bj,myThid)
c 2d local variables
c        CALL DIAGNOSTICS_FILL(Fe_burial,'BLGFEBUR',0,1,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(NO3_sed,'BLGNSED ',0,1,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(PO4_sed,'BLGPSED ',0,1,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(O2_sed,'BLGO2SED',0,1,2,bi,bj,myThid)
c these variables are currently 1d, could be 3d for diagnostics
c (or diag_fill could be called inside loop - which is faster?)
c        CALL DIAGNOSTICS_FILL(frac_exp,'BLGFEXP ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(irr_mix,'BLGIRRM ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(irrk,'BLGIRRK ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(kFe_eq_lig,'BLGPUP  ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(mu,'BLGMU  ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(mu_diaz,'BLGMUDIA',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(PtoN,'BLGP2N ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(FetoN,'BLGFE2N ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(Pc_m,'BLGPCM ',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(Pc_m_diaz,'BLGPCMD',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(theta_Fe,'BLGTHETA',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(theta_Fe_max,'BLGTHETM',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(wsink,'BLGWSINK',0,Nr,2,bi,bj,myThid)
c        CALL DIAGNOSTICS_FILL(zremin,'BLGZREM ',0,Nr,2,bi,bj,myThid)

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN

      END
