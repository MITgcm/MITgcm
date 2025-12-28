#include "CPP_EEOPTIONS.h"
#include "TAPENADE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif      

      SUBROUTINE GLOBAL_MAX_R8_B(rhsmax, rhsmaxb, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_MAX.h"
      _RL     rhsmax
      _RL     rhsmaxb
      INTEGER myThid

      CALL GLOBAL_ADMAX_R8(rhsmaxb, myThid)
      RETURN
      END

      SUBROUTINE GLOBAL_SUM_TILE_RL_B(phiTile, phiTileb,
     &     sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     phiTileb(nSx,nSy)
      _RL     sumPhib
      _RL     sumPhi
      INTEGER myThid

      CALL GLOBAL_ADSUM_TILE_RL(phiTileb, sumPhib, myThid)
      END

!     Adjoint of global_sum_r8 for arguments Arg1=(in;out)
      SUBROUTINE GLOBAL_SUM_R8_B(sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL sumPhi
      _RL sumPhib
      INTEGER myThid

      CALL GLOBAL_ADSUM_R8(sumPhib, myThid)
      END

      SUBROUTINE CG2D_B0(cg2d_b, cg2d_bb, cg2d_x,
     &                cg2d_xb, firstResidual, minResidualSq,
     &                lastResidual,numIters, nIterMin,myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
      _RL  cg2d_b(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_bb(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_xb(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid

! [llh] we assume the downstream cg2d_b is passive, which helps us
!    because it seems the input 2nd arg of cg2d() pollutes its output value:
      cg2d_bb = 0.d0
      CALL CG2D(cg2d_xb, cg2d_bb, firstResidual,
     +        minResidualSq, lastResidual, numIters,
     +        nIterMin, myThid)
! [llh] the upstream cg2d_x is passive:
      cg2d_xb = 0.d0
      END

      SUBROUTINE ADEXCH_3D_RL( adVar, Nr, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_UV_XY_RS( adU, adV, bool, myThid )
      IMPLICIT NONE
      Real*8 adU
      Real*8 adV
      LOGICAL bool
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_UV_3D_RL( aduVel,advVel, bool, Nr, myThid )
      IMPLICIT NONE
      LOGICAL bool
      Real*8 aduVel
      Real*8 advVel
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_XY_RS( adVar, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_XY_RL( adVar, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      
      SUBROUTINE STREAMICE_CG_SOLVE_D(cg_uin, cg_uind, cg_vin, cg_vind,
     &                                cg_bu, cg_bud, cg_bv, cg_bvd, a_uu
     &                                , a_uud, a_uv, a_uvd, a_vu, a_vud
     &                                , a_vv, a_vvd, tolerance, iters,
     &                                maxiter, mythid)
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      INTEGER myThid
      INTEGER iters
      INTEGER maxIter
      _RL tolerance
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL cg_Uind (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vind (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bud (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bvd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uud (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vud (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uvd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vvd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      ! LOCAL VARIABLES
      INTEGER i, j, bi, bj
      INTEGER colx, coly
      _RL deltaAu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAuMdBu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAvMdBv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE

      ! A x = b
      ! delta A x + A delta x = delta b
      ! A delta x = delta b - delta A x
      ! delta x = A^{-1} (delta b - delta A x)

      CALL STREAMICE_CG_SOLVE (cg_uin, cg_vin,
     &                                cg_bu, cg_bv, 
     &                                a_uu, a_uv, a_vu, a_vv, 
     &                                tolerance, iters,
     &                                maxiter, mythid)

      _EXCH_XY_RL(cg_uin, myThid)
      _EXCH_XY_RL(cg_vin, myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          deltaAu(i,j,bi,bj) = 0. _d 0
          deltaAv(i,j,bi,bj) = 0. _d 0

          DO colx=-1,1
           DO coly=-1,1
            deltaAu(i,j,bi,bj) = deltaAu(i,j,bi,bj) +
     &         A_uud(i,j,bi,bj,colx,coly)*
     &         cg_Uin(i+colx,j+coly,bi,bj)+
     &         A_uvd(i,j,bi,bj,colx,coly)*
     &         cg_Vin(i+colx,j+coly,bi,bj)

            deltaAv(i,j,bi,bj) = deltaAv(i,j,bi,bj) +
     &         A_vud(i,j,bi,bj,colx,coly)*
     &         cg_Uin(i+colx,j+coly,bi,bj)+
     &         A_vvd(i,j,bi,bj,colx,coly)*
     &         cg_Vin(i+colx,j+coly,bi,bj)
           ENDDO
          ENDDO

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(deltaAu, myThid)
      _EXCH_XY_RL(deltaAv, myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-oly,sNy+oly
         DO i=1-olx,sNx+olx
          deltaAuMdBu(i,j,bi,bj) = cg_bud(i,j,bi,bj)-deltaAu(i,j,bi,bj)
          deltaAvMdBv(i,j,bi,bj) = cg_bvd(i,j,bi,bj)-deltaAv(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL STREAMICE_CG_SOLVE (cg_uind, cg_vind,
     &                                deltaAuMdBu, deltaAvMdBv, 
     &                                a_uu, a_uv, a_vu, a_vv,
     &                                tolerance, iters,
     &                                maxiter, mythid) 

#endif
      END


      SUBROUTINE STREAMICE_CG_SOLVE_B(cg_uin, cg_uinb, cg_vin, cg_vinb,
     &                                cg_bu, cg_bub, cg_bv, cg_bvb, a_uu
     &                                , a_uub, a_uv, a_uvb, a_vu, a_vub
     &                                , a_vv, a_vvb, tolerance, iters,
     &                                maxiter, mythid)

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#endif

      INTEGER myThid
      INTEGER iters
      INTEGER maxIter
      _RL tolerance
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL cg_Uinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

#ifdef ALLOW_STREAMICE
      INTEGER recalc_vel
      recalc_vel = 1
      CALL ADSTREAMICE_CG_SOLVE(
     U                               cg_Uin,    ! velocities - need to be recalc ed
     I                               cg_Uinb,      ! adjoint of vel (input)
     U                               cg_Vin,    ! velocities - need to be recalc ed
     I                               cg_Vinb,      ! adjoint of vel (input)
     I                               cg_Bu,   ! to recalc velocities
     U                               cg_Bub,     ! adjoint of RHS (output)
     I                               cg_Bv,   ! to recalc velocities
     U                               cg_Bvb,     ! adjoint of RHS (output)
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     U                               A_uub,     ! adjoint of matrix coeffs (output)
     I                               A_uv,       ! section of matrix that multiplies v and projects on u
     U                               A_uvb,     ! adjoint of matrix coeffs (output)
     I                               A_vu,       ! section of matrix that multiplies u and projects on v
     U                               A_vub,     ! adjoint of matrix coeffs (output)
     I                               A_vv,       ! section of matrix that multiplies v and projects on v
     U                               A_vvb,     ! adjoint of matrix coeffs (output)
     I                               tolerance,
     I                               maxiter,
     I                               recalc_vel,
     I                               myThid )
#endif
      END

C [llh] Hand-written forward-sweep adjoint code of STREAMICE_CG_SOLVE
C [llh] Adjoint of solving Ax=b,
C [llh]  where A is (Auu, Auv, Avu, Avv), x is (xu,xv), b is (bu,bv)
      SUBROUTINE STREAMICE_CG_SOLVE_FWD(cg_uin,
     &                                  cg_vin,
     &                                  cg_bu,
     &                                  cg_bv,
     &                                  a_uu, a_uv,
     &                                  a_vu, a_vv,
     &                                  tolerance, iters,
     &                                  maxiters, mythid)


      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#endif


      !INPUT/OUTPUT ARGUMENTS
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL tolerance
      INTEGER maxiters
      INTEGER iters
      INTEGER myThid

#ifdef ALLOW_STREAMICE

C     !LOCAL VARIABLES
      INTEGER i, j, bi, bj, colx, coly
      INTEGER conv_flag, tmpiter
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      conv_flag = 0
      CALL STREAMICE_CG_SOLVE(
     &  cg_uin,
     &  cg_vin,
     &  cg_bu,
     &  cg_bv,
     &  A_uu,
     &  A_uv,
     &  A_vu,
     &  A_vv,
     &  tolerance,
     &  iters,
     &  maxiters,
     &  myThid )

      CALL PUSHREAL8ARRAY(cg_vin, (snx+2*olx)*(sny+2*oly)*nsx*nsy)
      CALL PUSHREAL8ARRAY(cg_uin, (snx+2*olx)*(sny+2*oly)*nsx*nsy)
#endif
      END

C [llh] Hand-written backward-sweep adjoint code of STREAMICE_CG_SOLVE
C [llh] Adjoint of solving Ax=b,
C [llh]  where A is (Auu, Auv, Avu, Avv), x is (xu,xv), b is (bu,bv)
#ifdef ALLOW_PETSC
      SUBROUTINE STREAMICE_CG_SOLVE_BWD(
     &                                cg_uin, cg_uinb, cg_vin, cg_vinb,
     &                                cg_bu, cg_bub, cg_bv, cg_bvb, a_uu
     &                                , a_uub, a_uv, a_uvb, a_vu, a_vub
     &                                , a_vv, a_vvb, tolerance
     &                                , toleranceb, iters
     &                                , maxiter, mythid)
#else              
      SUBROUTINE STREAMICE_CG_SOLVE_BWD(
     &                                cg_uin, cg_uinb, cg_vin, cg_vinb,
     &                                cg_bu, cg_bub, cg_bv, cg_bvb, a_uu
     &                                , a_uub, a_uv, a_uvb, a_vu, a_vub
     &                                , a_vv, a_vvb, tolerance, 
     &                                iters, maxiter, 
     &                                mythid)
#endif              

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#endif

      INTEGER myThid
      INTEGER iters
      INTEGER maxIter
      _RL tolerance
#ifdef ALLOW_PETSC
      _RL toleranceb
#endif      
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL cg_Uinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL
     & A_uub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

#ifdef ALLOW_STREAMICE

      CALL POPREAL8ARRAY(cg_Uin, (snx+2*olx)*(sny+2*oly)*nsx*nsy)
      CALL POPREAL8ARRAY(cg_Vin, (snx+2*olx)*(sny+2*oly)*nsx*nsy)


      CALL ADSTREAMICE_CG_SOLVE(
     U                               cg_Uin,    ! velocities - need to be recalc ed
     I                               cg_Uinb,      ! adjoint of vel (input)
     U                               cg_Vin,    ! velocities - need to be recalc ed
     I                               cg_Vinb,      ! adjoint of vel (input)
     I                               cg_Bu,   ! to recalc velocities
     U                               cg_Bub,     ! adjoint of RHS (output)
     I                               cg_Bv,   ! to recalc velocities
     U                               cg_Bvb,     ! adjoint of RHS (output)
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     U                               A_uub,     ! adjoint of matrix coeffs (output)
     I                               A_uv,       ! section of matrix that multiplies v and projects on u
     U                               A_uvb,     ! adjoint of matrix coeffs (output)
     I                               A_vu,       ! section of matrix that multiplies u and projects on v
     U                               A_vub,     ! adjoint of matrix coeffs (output)
     I                               A_vv,       ! section of matrix that multiplies v and projects on v
     U                               A_vvb,     ! adjoint of matrix coeffs (output)
     I                               tolerance,
     I                               maxiter,
     I                               myThid )
#endif
      END
