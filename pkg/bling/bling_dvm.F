C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_dvm.F,v 1.5 2016/09/12 20:00:28 mmazloff Exp $
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
C     | subroutine bling_dvm
C     | o Diel Vertical Migration
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
C     N_dvm         :: vertical transport of nitrogen by DVM
C     P_dvm         :: vertical transport of phosphorus by DVM
C     Fe_dvm        :: vertical transport of iron by DVM
C     PTR_O2        :: nitrate concentration
C     mld           :: mixed layer depth
      _RL     N_dvm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     P_dvm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     Fe_dvm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     mld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      
C     === Output ===
C     N_remindvm    :: nitrogen remineralization due to diel vertical migration
C     P_remindvm    :: phosphorus remineralization due to diel vertical migration
C     Fe_remindvm   :: iron remineralization due to diel vertical migration
      _RL     N_remindvm     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     P_remindvm     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     Fe_remindvm     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_BLING
C     === Local variables ===
C     i,j,k         :: loop indices
      INTEGER i,j,k
      INTEGER tmp
      _RL depth_l
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
      _RL Fe_burial(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL x_erfcc,z_erfcc,t_erfcc,erfcc
CEOP

c ---------------------------------------------------------------------
c  Initialize output and diagnostics
       DO k=1,Nr
        DO j=jmin,jmax
          DO i=imin,imax
              N_remindvm(i,j,k)        = 0. _d 0
              P_remindvm(i,j,k)        = 0. _d 0
              Fe_remindvm(i,j,k)       = 0. _d 0
              dvm(i,j,k)               = 0. _d 0
          ENDDO
          Fe_burial(i,j)               = 0. _d 0
        ENDDO
       ENDDO
       
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

c  Initialize 
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

c  Calculate the depth of migration based on linear regression.

        depth_l=-rF(k+1)

c  Average temperature and oxygen over upper 35 m, and 140-515m.
c  Also convert O2 to mmol m-3.

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

c  Calculate the regression, using the constants given in Bianchi et al. (2013).
c  The variable values are bounded to lie within reasonable ranges: 
c         O2 gradient   : [-10,300] mmol/m3
c         Log10 Chl     : [-1.8,0.85] log10(mg/m3)
c         mld           : [0,500] m
c         T gradient    : [-3,20] C

c!! replacing hblt_depth(i,j) with mld... 

#ifdef BLING_ADJOINT_SAFE

        z_dvm = 300. _d 0

#else

        z_dvm_regr = 398. _d 0
     &   - 0.56 _d 0*min(300. _d 0,max(-10. _d 0,(o2_upper - o2_lower)))
     &   - 115. _d 0*min(0.85 _d 0,max(-1.80 _d 0,
     &   log10(max(chl(i,j,1,bi,bj),chl_min))))
     &   + 0.36 _d 0*min(500. _d 0,max(epsln,mld(i,j)))
     &   - 2.40 _d 0*min(20. _d 0,max(-3. _d 0,(temp_upper-temp_lower)))

c  Limit the depth of migration in polar winter.
c  Use irr_mem since this is averaged over multiple days, dampening the 
c  diurnal cycle.
c  Tapers Z_DVM to the minimum when surface irradince is below a given  
c  threshold (here 10 W/m2).

        if ( irr_mem(i,j,1,bi,bj) .lt. 10. ) then
          z_dvm_regr = 150. _d 0 + (z_dvm_regr - 150. _d 0) *
     &       irr_mem(i,j,1,bi,bj) / 10. _d 0
        endif

c  Check for suboxic water within the column. If found, set dvm
c  stationary depth to 2 layers above it. This is not meant to
c  represent a cessation of downward migration, but rather the
c  requirement for aerobic DVM respiration to occur above the suboxic
c  water, where O2 is available.

        tmp = 0
        DO k=1,Nr-2

         IF ( (hFacC(i,j,k,bi,bj).gt.0. _d 0) .and. (tmp.eq.0)) THEN

          z_dvm = -rf(k+1)
          if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp = 1

         ENDIF

        enddo

c  The stationary depth is constrained between 150 and 700, above any
c  anoxic waters found, and above the bottom.

        z_dvm = min(700. _d 0,max(150. _d 0,z_dvm_regr),z_dvm,-rf(k+1))
c!!           bling%zbot(i,j,grid_kmt(i,j))) * grid_tmask(i,j,1)
c!! what is grid_kmt?

#endif

c  Calculate the fraction of migratory respiration that occurs
c  during upwards and downwards swimming. The remainder is
c  respired near the stationary depth.
c  Constants for swimming speed and resting time are hard-coded
c  after Bianchi et al, Nature Geoscience 2013.

        frac_migr = max( 0.0 _d 0, min( 1.0 _d 0, (2.0 _d 0 * z_dvm) /
     &        (epsln + 0.05 _d 0 * 0.5 _d 0 * 86400. _d 0)))

c  Calculate the vertical profile shapes of DVM fluxes.
c  These are given as the downward organic flux due to migratory
c  DVM remineralization, defined at the bottom of each layer k.

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

c  Then, calculate the part at the stationary depth.

c  Approximation of the complementary error function
c  From Numerical Recipes (F90, Ch. 6, p. 216)
c  Returns the complementary error function erfc(x)
c  with fractional error everywhere less than 1.2e-7
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

c  Add the shapes, resulting in the 3-d DVM flux operator. If the
c  current layer is the bottom layer, or the layer beneath the
c  underlying layer is suboxic, all fluxes at and below the current
c  layer remain at the initialized value of zero. This will cause all
c  remaining DVM remineralization to occur in this layer.
          IF (k.LT.NR-1) THEN
            if (PTR_O2(i,j,k+2) .lt. (5. _d 0*oxic_min)) tmp = 1
          ENDIF
c!!          if (k .eq. grid_kmt(i,j)) exit
          dvm(i,j,k)  = fdvm_migr + fdvm_stat

         ENDIF

        enddo

c  Sum up the total organic flux to be transported by DVM

        do k = 1, nr
          fdvmn_vint  = fdvmn_vint  + N_dvm(i,j,k)  * drf(k)
          fdvmp_vint  = fdvmp_vint  + P_dvm(i,j,k)  * drf(k)
          fdvmfe_vint = fdvmfe_vint + Fe_dvm(i,j,k) * drf(k)
        enddo

c  Calculate the remineralization terms as the divergence of the flux

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
