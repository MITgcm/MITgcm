C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_visc_beta_hybrid.F,v 1.5 2014/09/09 23:09:21 jmc Exp $
C $Name:  $

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
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_HYBRID_STRESS

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l, m
      INTEGER ikey_1
      _RL ux, uy, vx, vy, exx, eyy, exy, unorm, second_inv
      _RL ub, vb, fb, mean_u_shear, mean_v_shear, umid, vmid
      _RL omega_temp (Nr+1), u_shear(Nr+1), v_shear(Nr+1)

      _RL STREAMICE_BSTRESS_EXP
!       _RL total_vol_out
      external STREAMICE_BSTRESS_EXP

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
            visc_streamice_full(i,j,m,bi,bj) =
     &       .5 * B_glen(i,j,m,bi,bj)**2 *
     &        second_inv**((1-n_glen)/n_glen)
#else
            visc_streamice_full(i,j,m,bi,bj) =
     &       .5 * B_glen(i,j,bi,bj)**2 *
     &        second_inv**((1-n_glen)/n_glen)
#endif

            visc_streamice (i,j,bi,bj) = visc_streamice (i,j,bi,bj) +
     &       H_streamice(i,j,bi,bj) * streamice_delsigma (m) *
     &       visc_streamice_full(i,j,m,bi,bj)

            omega_temp (m) = omega_temp(m+1) +
     &       streamice_sigma_coord(m) * streamice_delsigma(m) /
     &       visc_streamice_full(i,j,m,bi,bj)

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

           fb = C_basal_friction(i,j,bi,bj)**2 *
     &      STREAMICE_BSTRESS_EXP (unorm,n_basal_friction) *
     &      streamice_basal_geom(i,j,bi,bj) *
     &      float_frac_streamice(i,j,bi,bj)

           tau_beta_eff_streamice(i,j,bi,bj) =
     &       fb /
     &       (1+fb*streamice_omega(i,j,bi,bj)/H_streamice(i,j,bi,bj))

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif
#endif
      RETURN
      END
