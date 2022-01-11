#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VISC_BETA_HYBRID ( myThid )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_HYBRID_STRESS

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l, m
      _RL ux, uy, vx, vy, exx, eyy, exy, unorm, second_inv
      _RL ub, vb, fb, mean_u_shear, mean_v_shear, umid, vmid
      _RL omega_temp (Nr+1), u_shear(Nr+1), v_shear(Nr+1)
      _RL C_fric_val, Bglen_val
#ifdef STREAMICE_FLOWLINE_BUTTRESS
      _RL buttr_param, pwr
#endif
#ifdef STREAMICE_COULOMB_SLIDING
      _RL effective_stress, hf
      _RL ETA_GL_STREAMICE
      _RL i_nbasalfric
      EXTERNAL ETA_GL_STREAMICE
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
#endif
      INTEGER ikey_1
#endif
      _RL STREAMICE_BSTRESS_EXP
c       _RL total_vol_out
      EXTERNAL STREAMICE_BSTRESS_EXP

#ifdef STREAMICE_FLOWLINE_BUTTRESS
      buttr_param = 5**(1./3.) / (streamice_buttr_width/2.)**(4./3.)
      pwr = 1./n_glen
#endif

#ifdef STREAMICE_COULOMB_SLIDING
      i_nbasalfric = 1. / n_basal_friction
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (STREAMICE_hmask(i,j,bi,bj).eq.1) THEN

           umid = 0
           vmid = 0

           DO k=0,1
            DO l=0,1
             umid = umid + 0.25 *
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) *
     &        recip_rA(i,j,bi,bj) *
     &        U_streamice(i+k,j+l,bi,bj)
             vmid = vmid + 0.25 *
     &        dxG(i,j+l,bi,bj)*dyG(i+k,j,bi,bj) *
     &        recip_rA(i,j,bi,bj) *
     &        V_streamice(i+k,j+l,bi,bj)
            ENDDO
           ENDDO

           ux = (U_streamice(i+1,j+1,bi,bj) +
     &           U_streamice(i+1,j,bi,bj)   -
     &           U_streamice(i,j+1,bi,bj)   -
     &           U_streamice(i,j,bi,bj)) /
     &           (2. * dxF(i,j,bi,bj))
           vx = (V_streamice(i+1,j+1,bi,bj) +
     &           V_streamice(i+1,j,bi,bj)   -
     &           V_streamice(i,j+1,bi,bj)   -
     &           V_streamice(i,j,bi,bj)) /
     &           (2. * dxF(i,j,bi,bj))
           uy = (U_streamice(i+1,j+1,bi,bj) -
     &           U_streamice(i+1,j,bi,bj)   +
     &           U_streamice(i,j+1,bi,bj)   -
     &           U_streamice(i,j,bi,bj)) /
     &           (2. * dyF(i,j,bi,bj))
           vy = (V_streamice(i+1,j+1,bi,bj) -
     &           V_streamice(i+1,j,bi,bj)   +
     &           V_streamice(i,j+1,bi,bj)   -
     &           V_streamice(i,j,bi,bj)) /
     &           (2. * dyF(i,j,bi,bj))

           exx = ux + k1AtC_str(i,j,bi,bj)*vmid
           eyy = vy + k2AtC_str(i,j,bi,bj)*umid
           exy = .5*(uy+vx) +
     &      k1AtC_str(i,j,bi,bj)*umid + k2AtC_str(i,j,bi,bj)*vmid

           visc_streamice (i,j,bi,bj) = 0.0
           streamice_omega(i,j,bi,bj) = 0.0
           omega_temp (Nr+1) = 0.0
           u_shear(Nr+1) = 0.0
           v_shear(Nr+1) = 0.0

           DO m=Nr,1,-1

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1

          ikey_1 = m
     &         + Nr*(i-1)
     &         + Nr*sNx*(j-1)
     &         + Nr*sNx*sNy*act1
     &         + Nr*sNx*sNy*max1*act2
     &         + Nr*sNx*sNy*max1*max2*act3
     &         + Nr*sNx*sNy*max1*max2*max3*act4

CADJ STORE visc_streamice_full(i,j,m,bi,bj)
CADJ &     = comlev1_stream_hybrid, key=ikey_1
#endif

            streamice_vert_shear_uz (m) = streamice_taubx(i,j,bi,bj) /
     &       visc_streamice_full(i,j,m,bi,bj)
     &       * streamice_sigma_coord(m)

            streamice_vert_shear_vz (m) = streamice_tauby(i,j,bi,bj) /
     &       visc_streamice_full(i,j,m,bi,bj)
     &       * streamice_sigma_coord(m)

            second_inv =
     &       sqrt(exx**2+eyy**2+exx*eyy+exy**2+eps_glen_min**2+
     &            0.25 * streamice_vert_shear_uz(m)**2 +
     &            0.25 * streamice_vert_shear_vz(m)**2)

#ifdef STREAMICE_3D_GLEN_CONST
            IF (.not.STREAMICE_use_log_ctrl) THEN
             Bglen_val = B_glen(i,j,m,bi,bj)**2
            ELSE
             Bglen_val = exp(B_glen(i,j,m,bi,bj))
            ENDIF
#else
            IF (.not.STREAMICE_use_log_ctrl) THEN
             Bglen_val = B_glen(i,j,bi,bj)**2
            ELSE
             Bglen_val = exp(B_glen(i,j,bi,bj))
            ENDIF
#endif

#if (defined (ALLOW_STREAMICE_OAD_FP))
            visc_full_new_si(i,j,m,bi,bj) =
#else
            visc_streamice_full(i,j,m,bi,bj) =
#endif
     &       .5 * Bglen_val *
     &        second_inv**((1-n_glen)/n_glen)

            visc_streamice (i,j,bi,bj) = visc_streamice (i,j,bi,bj) +
     &       H_streamice(i,j,bi,bj) * streamice_delsigma (m) *
#if (defined (ALLOW_STREAMICE_OAD_FP))
     &       visc_full_new_si(i,j,m,bi,bj)
#else
     &       visc_streamice_full(i,j,m,bi,bj)
#endif

            omega_temp (m) = omega_temp(m+1) +
     &       streamice_sigma_coord(m) * streamice_delsigma(m) /
#if (defined (ALLOW_STREAMICE_OAD_FP))
     &       visc_full_new_si(i,j,m,bi,bj)
#else
     &       visc_streamice_full(i,j,m,bi,bj)
#endif

            u_shear (m) = u_shear (m+1) +
     &       streamice_vert_shear_uz (m) * streamice_delsigma (m) *
     &       H_streamice(i,j,bi,bj)

            v_shear (m) = v_shear (m+1) +
     &       streamice_vert_shear_vz (m) * streamice_delsigma (m) *
     &       H_streamice(i,j,bi,bj)

           ENDDO

           mean_u_shear = 0.0
           mean_v_shear = 0.0

           DO m=Nr,1,-1

            streamice_omega(i,j,bi,bj) = streamice_omega(i,j,bi,bj) +
     &       streamice_delsigma(m)*(omega_temp(m)+omega_temp(m+1))*.5
     &       * H_streamice(i,j,bi,bj)**2

            mean_u_shear = mean_u_shear +
     &       streamice_delsigma(m)*(u_shear(m)+u_shear(m+1))*.5

            mean_v_shear = mean_v_shear +
     &       streamice_delsigma(m)*(v_shear(m)+v_shear(m+1))*.5

           ENDDO

           streamice_u_surf(i,j,bi,bj) =
     &      u_shear(1) + umid - mean_u_shear

           streamice_v_surf(i,j,bi,bj) =
     &      v_shear(1) + vmid - mean_v_shear

           ub = umid - streamice_taubx(i,j,bi,bj) *
     &      streamice_omega(i,j,bi,bj) / H_streamice(i,j,bi,bj)

           streamice_u_bed (i,j,bi,bj) = ub

           vb = vmid - streamice_tauby(i,j,bi,bj) *
     &      streamice_omega(i,j,bi,bj) / H_streamice(i,j,bi,bj)

           streamice_v_bed (i,j,bi,bj) = vb

           unorm = sqrt(ub**2+vb**2+eps_u_min**2)

           if (.not.STREAMICE_use_log_ctrl) THEN
            C_fric_val = C_basal_friction(i,j,bi,bj)**2
           else
            C_fric_val = exp(C_basal_friction(i,j,bi,bj))
           endif

#ifdef STREAMICE_COULOMB_SLIDING
           IF (.not.streamice_allow_reg_coulomb) THEN
#endif
           fb = C_fric_val *
     &      STREAMICE_BSTRESS_EXP (unorm,n_basal_friction) *
     &      streamice_basal_geom(i,j,bi,bj) *
     &      float_frac_streamice(i,j,bi,bj)

#ifdef STREAMICE_COULOMB_SLIDING
           ELSE

           hf = max (0.0, -1.0 *streamice_density_ocean_avg
     &        / streamice_density * R_low_si(i,j,bi,bj))

           effective_stress = streamice_density * gravity *
     &      (H_streamice(i,j,bi,bj)-hf)
           effective_stress= max(0.0, effective_stress)
           fb = C_fric_val *
     &      unorm ** n_basal_friction *
     &      .5 * effective_stress /
     &      (C_fric_val**(i_nbasalfric)*unorm+
     &       (0.5 * effective_stress) ** (i_nbasalfric)) **
     &        n_basal_friction *
     &      streamice_basal_geom(i,j,bi,bj) *
     &      float_frac_streamice(i,j,bi,bj) / unorm
           ENDIF

#endif

           tau_beta_eff_streamice(i,j,bi,bj) =
     &       fb /
     &       (1+fb*streamice_omega(i,j,bi,bj)/H_streamice(i,j,bi,bj))
#ifdef STREAMICE_FLOWLINE_BUTTRESS
           if (usestreamiceflowlineButtr) then
           tau_beta_eff_streamice(i,j,bi,bj) =
     &      tau_beta_eff_streamice(i,j,bi,bj) +
     &      buttr_param*B_glen(i,j,bi,bj)**2 *
     &      STREAMICE_BSTRESS_EXP (unorm,pwr)*
     &      H_streamice(i,j,bi,bj)
          endif
#endif

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef ALLOW_OPENAD
#ifdef STREAMICE_HYBRID_STRESS
      tau_beta_eff_streamice(1,1,1,1) =
     &  tau_beta_eff_streamice(1,1,1,1) +
     &  0. * streamice_u_surf(1,1,1,1) +
     &  0. * streamice_v_surf(1,1,1,1)
#endif
#endif

#endif
#endif
      RETURN
      END
