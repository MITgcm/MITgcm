C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_dvm.F,v 1.4 2016/06/20 23:10:51 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_DVM(
     I           N_dvm,P_dvm,Fe_dvm,
     I           PTR_O2, mld,
     O           N_remindvm, P_remindvm, Fe_remindvm,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myIter, myTime, myThid )

C     =================================================================
C     | subroutine bling_prod
C     | o Nutrient uptake and partitioning between organic pools.
C     | - Phytoplankton biomass-specific growth rate is calculated
C     |   as a function of light, nutrient limitation, and
C     |   temperature.
C     | - Biomass growth xxx
C     | o Organic matter export, remineralization, and recycling.
C     | - Sinking particulate flux and diel migration contribute to
C     |   export.
C     | - Denitrification xxx
C     =================================================================

      implicit none

C     === Global variables ===
C     P_sm          :: Small phytoplankton biomass
C     P_lg          :: Large phytoplankton biomass
C     P_diaz        :: Diazotroph phytoplankton biomass
C     irr_mem       :: Phyto irradiance memory

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
      _RL     PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
C     irr_eff       :: effective irradiance
C     NO3_lim       :: nitrate limitation
C     PO4_lim       :: phosphate limitation
C     Fe_lim        :: iron limitation for phytoplankton
C     Fe_lim_diaz   :: iron limitation for diazotrophs
C     alpha_Fe      :: initial slope of the P-I curve
C     irrk          :: nut-limited efficiency of algal photosystems
C     Pc_m          :: light-saturated max photosynthesis rate for phyt
C     Pc_m_diaz     :: light-saturated max photosynthesis rate for diaz
C     Pc_tot        :: carbon-specific photosynthesis rate
C     expkT         :: temperature function
C     mu            :: net carbon-specific growth rate for phyt
C     mu_diaz       :: net carbon-specific growth rate for diaz
C     biomass_sm    :: nitrogen concentration in small phyto biomass
C     biomass_lg    :: nitrogen concentration in large phyto biomass
C     N_uptake      :: nitrogen uptake
C     N_fix         :: nitrogen fixation
C     P_uptake      :: phosphorus uptake
C     POC_flux      :: carbon export flux 3d field
C     PtoN          :: variable ratio of phosphorus to nitrogen
C     FetoN         :: variable ratio of iron to nitrogen
C     N_spm         :: particulate sinking of nitrogen
C     P_spm         :: particulate sinking of phosphorus
C     Fe_spm        :: particulate sinking of iron
C     N_dvm         :: vertical transport of nitrogen by DVM
C     P_dvm         :: vertical transport of phosphorus by DVM
C     Fe_dvm        :: vertical transport of iron by DVM
C     N_recycle     :: recycling of newly-produced organic nitrogen
C     P_recycle     :: recycling of newly-produced organic phosphorus
C     Fe_recycle    :: recycling of newly-produced organic iron
c xxx to be completed
      INTEGER i,j,k
      INTEGER tmp
      _RL irr_eff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NO3_lim
      _RL PO4_lim
      _RL Fe_lim
      _RL Fe_lim_diaz
      _RL expkT
      _RL Pc_m
      _RL Pc_m_diaz
      _RL irrk
      _RL mu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mu_diaz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PtoN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL FetoN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_fix(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_uptake(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL frac_exp
      _RL N_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_spm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DON_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL DOP_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_recycle(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL POC_flux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL NCP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CaCO3_prod(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL PONflux_u
      _RL POPflux_u
      _RL PFEflux_u
      _RL CaCO3flux_u
      _RL PONflux_l
      _RL POPflux_l
      _RL PFEflux_l
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
      _RL N_den_pelag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N_den_benthic(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL log_btm_flx
      _RL N_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_reminp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL CacO3_diss(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
      _RL N_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL P_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Fe_remindvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL mld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fe_burial(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL x_erfcc,z_erfcc,t_erfcc,erfcc
cxx order
CEOP

c ---------------------------------------------------------------------
c  Initialize output and diagnostics
       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              G_NO3(i,j,k)        = 0. _d 0
              G_PO4(i,j,k)        = 0. _d 0
              G_FE(i,j,k)         = 0. _d 0
              G_O2(i,j,k)         = 0. _d 0
              G_DON(i,j,k)        = 0. _d 0
              G_DOP(i,j,k)        = 0. _d 0
              G_CaCO3(i,j,k)      = 0. _d 0
              N_uptake(i,j,k)     = 0. _d 0
              P_uptake(i,j,k)     = 0. _d 0
              Fe_uptake(i,j,k)    = 0. _d 0
              Fe_ads_org(i,j,k)   = 0. _d 0
              Fe_ads_inorg(i,j,k) = 0. _d 0
              mu(i,j,k)           = 0. _d 0
              mu_diaz(i,j,k)      = 0. _d 0
              irr_eff(i,j,k)      = 0. _d 0
              PtoN(i,j,k)         = 0. _d 0
              FetoN(i,j,k)        = 0. _d 0
              N_fix(i,j,k)        = 0. _d 0
              N_spm(i,j,k)        = 0. _d 0
              P_spm(i,j,k)        = 0. _d 0
              Fe_spm(i,j,k)       = 0. _d 0
              dvm(i,j,k)          = 0. _d 0
              N_reminp(i,j,k)     = 0. _d 0
              P_reminp(i,j,k)     = 0. _d 0
              Fe_reminp(i,j,k)    = 0. _d 0
              N_recycle(i,j,k)    = 0. _d 0
              P_recycle(i,j,k)    = 0. _d 0
              Fe_recycle(i,j,k)   = 0. _d 0
              N_remindvm(i,j,k)   = 0. _d 0
              P_remindvm(i,j,k)   = 0. _d 0
              Fe_remindvm(i,j,k)  = 0. _d 0
              N_den_benthic(i,j,k)= 0. _d 0
              N_den_pelag(i,j,k)  = 0. _d 0
              DON_prod(i,j,k)     = 0. _d 0
              DOP_prod(i,j,k)     = 0. _d 0
              CaCO3_prod(i,j,k)   = 0. _d 0
              CaCO3_diss(i,j,k)   = 0. _d 0
              POC_flux(i,j,k)     = 0. _d 0
              NPP(i,j,k)          = 0. _d 0
              NCP(i,j,k)          = 0. _d 0
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
cxx order

C  ---------------------------------------------------------------------
c DIEL VERTICAL MIGRATOR EXPORT
c The effect of vertically-migrating animals on the export flux of organic
c matter from the ocean surface is treated similarly to the scheme of
c Bianchi et al., Nature Geoscience 2013.
c This involves calculating the stationary depth of vertical migrators, using
c an empirical multivariate regression, and ensuring that this remains
c above the bottom as well as any suboxic waters.
c The total DVM export flux is partitioned between a swimming migratory
c component and the stationary component, and these are summed.

C$TAF LOOP = parallel
      DO j=jmin,jmax
C$TAF LOOP = parallel
       DO i=imin,imax

C        ! Initialize the working variables to zero
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

         IF ( hFacC(i,j,k,bi,bj).gt.0. _d 0 ) THEN

      ! Calculate the depth of migration based on linear regression.

        depth_l=-rF(k+1)

        ! Average temperature and oxygen over upper 35 m, and 140-515m.
        ! Also convert O2 to mmol m-3.

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

        ! Calculate the regression, using the constants given
        ! in Bianchi et al. (2013).
        ! The variable values are bounded to lie within reasonable
        ! ranges: O2 gradient   : [-10,300] mmol/m3
        !         Log10 Chl     : [-1.8,0.85] log10(mg/m3)
        !         mld           : [0,500] m
        !         T gradient    : [-3,20] C

C!! I am replacing hblt_depth(i,j) with mld... not sure what hblt_depth is

#ifdef BLING_ADJOINT_SAFE
        z_dvm = 300. _d 0

#else

        z_dvm_regr = 398. _d 0
     &   - 0.56 _d 0*min(300. _d 0,max(-10. _d 0,(o2_upper - o2_lower)))
     &   - 115. _d 0*min(0.85 _d 0,max(-1.80 _d 0,
     &                            log10(max(chl(i,j,1,bi,bj),chl_min))))
     &   + 0.36 _d 0*min(500. _d 0,max(epsln,mld(i,j)))
     &   - 2.40 _d 0*min(20. _d 0,max(-3. _d 0,(temp_upper-temp_lower)))

c        ! Limit the depth of migration in polar winter.
c        ! Use irr_mem since this is averaged over multiple days, dampening the diurnal cycle.
c        ! Tapers Z_DVM to the minimum when surface irradince is below a given threshold (here 10 W/m2).

        if ( irr_mem(i,j,1,bi,bj) .lt. 10. ) then
          z_dvm_regr = 150. _d 0 + (z_dvm_regr - 150. _d 0) *
     &       irr_mem(i,j,1,bi,bj) / 10. _d 0
        endif

C Check for suboxic water within the column. If found, set dvm
C stationary depth to 2 layers above it. This is not meant to
C represent a cessation of downward migration, but rather the
C requirement for aerobic DVM respiration to occur above the suboxic
C water, where O2 is available.

        tmp = 0
        DO k=1,Nr-2

         IF ( (hFacC(i,j,k,bi,bj).gt.0. _d 0) .and. (tmp.eq.0)) THEN

          z_dvm = -rf(k+1)
          if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp = 1

         ENDIF

        enddo

C The stationary depth is constrained between 150 and 700, above any
C anoxic waters found, and above the bottom.

        z_dvm = min(700. _d 0,max(150. _d 0,z_dvm_regr),z_dvm,-rf(k+1))
c!!           bling%zbot(i,j,grid_kmt(i,j))) * grid_tmask(i,j,1)
c!! what is grid_mkt??

#endif

        ! Calculate the fraction of migratory respiration that occurs
        ! during upwards and downwards swimming. The remainder is
        ! respired near the stationary depth.
        ! Constants for swimming speed and resting time are hard-coded
        ! after Bianchi et al, Nature Geoscience 2013.

        frac_migr = max( 0.0 _d 0, min( 1.0 _d 0, (2.0 _d 0 * z_dvm) /
     &        (epsln + 0.05 _d 0 * 0.5 _d 0 * 86400. _d 0)))

        ! Calculate the vertical profile shapes of DVM fluxes.
        ! These are given as the downward organic flux due to migratory
        ! DVM remineralization, defined at the bottom of each layer k.

        tmp = 0
        DO k=1,Nr

         IF ( (hFacC(i,j,k,bi,bj).gt.0. _d 0) .and. (tmp.eq.0)) THEN

          ! First, calculate the part due to active migration above
          ! the stationary depth.
          if (-rf(k+1) .lt. z_dvm) then
            fdvm_migr = frac_migr / (epsln + z_dvm - (-rf(2))) *
     &            (z_dvm - (-rf(k+1)) )
          else
            fdvm_migr  = 0.0
          endif

          ! Then, calculate the part at the stationary depth.
c          fdvm_stat = (1. - frac_migr) / 2. * erfcc((-rf(k) - z_dvm) /
c     &        ( (epsln + 2. * sigma_dvm**2.)**0.5))

c  ! Approximation of the complementary error function
c  ! From Numerical Recipes (F90, Ch. 6, p. 216)
c  ! Returns the complementary error function erfc(x)
c    with fractional error everywhere less than 1.2e-7
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

C Add the shapes, resulting in the 3-d DVM flux operator. If the
C current layer is the bottom layer, or the layer beneath the
C underlying layer is suboxic, all fluxes at and below the current
C layer remain at the initialized value of zero. This will cause all
C remaining DVM remineralization to occur in this layer.
          IF (k.LT.NR-1) THEN
            if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp = 1
          ENDIF
c!!          if (k .eq. grid_kmt(i,j)) exit
          dvm(i,j,k)  = fdvm_migr + fdvm_stat

         ENDIF

        enddo

c Sum up the total organic flux to be transported by DVM

        do k = 1, nr
          fdvmn_vint  = fdvmn_vint  + N_dvm(i,j,k)  * drf(k)
          fdvmp_vint  = fdvmp_vint  + P_dvm(i,j,k)  * drf(k)
          fdvmfe_vint = fdvmfe_vint + Fe_dvm(i,j,k) * drf(k)
        enddo

c Calculate the remineralization terms as the divergence of the flux

        N_remindvm(i,j,1)  = fdvmn_vint *  (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))
        P_remindvm(i,j,1)  = fdvmp_vint *  (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))
        Fe_remindvm(i,j,1) = fdvmfe_vint * (1 - dvm(i,j,1)) /
     &     (epsln + drf(1))

        do k = 2, nr
          N_remindvm(i,j,k)  = fdvmn_vint  *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
          P_remindvm(i,j,k)  = fdvmp_vint  *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
          Fe_remindvm(i,j,k) = fdvmfe_vint *
     &       (dvm(i,j,k-1) - dvm(i,j,k)) / (epsln + drf(k))
        enddo

       enddo
      enddo

c ---------------------------------------------------------------------

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

        CALL DIAGNOSTICS_FILL(Fe_dvm,'BLGFEDVM',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N_dvm,'BLGNDVM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(P_dvm,'BLGPDVM ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(dvm,'BLGDVM  ',0,Nr,2,bi,bj,myThid)

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN
      END
