#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VISC_BETA ( myThid )

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

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l
      _RL ux, uy, vx, vy, exx, eyy, exy, second_inv, unorm
      _RL umid, vmid, C_fric_val, Bglen_val

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

!A_glen_isothermal, n_glen, eps_glen_min,

           second_inv =
     &      sqrt(exx**2+eyy**2+exx*eyy+exy**2+eps_glen_min**2)

#ifdef STREAMICE_3D_GLEN_CONST
           IF (.not.STREAMICE_use_log_ctrl) THEN
            Bglen_val = B_glen(i,j,1,bi,bj)**2
           ELSE
            Bglen_val = exp(B_glen(i,j,1,bi,bj))
           ENDIF
#else
           IF (.not.STREAMICE_use_log_ctrl) THEN
            Bglen_val = B_glen(i,j,bi,bj)**2
           ELSE
            Bglen_val = exp(B_glen(i,j,bi,bj))
           ENDIF
#endif

           visc_streamice(i,j,bi,bj) =
     &      .5 * Bglen_val *
     &      second_inv**((1-n_glen)/n_glen) * H_streamice(i,j,bi,bj)

           if (.not.STREAMICE_use_log_ctrl) THEN
            C_fric_val = C_basal_friction(i,j,bi,bj)**2
           else
            C_fric_val = exp(C_basal_friction(i,j,bi,bj))
           endif

           unorm = sqrt(umid**2+vmid**2+eps_u_min**2)
           tau_beta_eff_streamice(i,j,bi,bj) =
     &       C_fric_val *
     &       unorm ** (n_basal_friction-1.0) *
     &       float_frac_streamice (i,j,bi,bj)

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif
      RETURN
      END
