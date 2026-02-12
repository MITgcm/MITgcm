#include "TAPENADE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

C--   File stubs_tap_adj.F: Subroutines with "manual" derivatives
C--  to be called by the tapenade adjoint and tangent linear code
C--    *_B: adjoint with checkpointing on
C--    *_D: tlm
C--    *_FWD, *_BWD: adjoint with no checkpointing
C--
C--    Contents:
C--    o  GLOBAL_MAX_R8_B
C--    o  GLOBAL_SUM_TILE_RL_B
C--    o  GLOBAL_SUM_TILE_RL_FWD
C--    o  GLOBAL_SUM_TILE_RL_BWD
C--    o  GLOBAL_SUM_R8_B
C--    o  CG2D_B0
C--    o  ADEXCH_3D_RL
C--    o  ADEXCH_UV_XY_RS
C--    o  ADEXCH_UV_3D_RL
C--    o  ADEXCH_XY_RS
C--    o  ADEXCH_XY_RL
C--    o  STREAMICE_CG_SOLVE_D
C--    o  STREAMICE_CG_SOLVE_B
C--    o  STREAMICE_CG_SOLVE_FWD
C--    o  STREAMICE_CG_SOLVE_BWD

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE GLOBAL_MAX_R8_B(rhsmax, rhsmaxb, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_MAX.h"
      Real*8  rhsmax
      Real*8  rhsmaxb
      INTEGER myThid

      CALL GLOBAL_ADMAX_R8(rhsmaxb, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE GLOBAL_SUM_TILE_RL_B(phiTile, phiTileb,
     &                                sumPhi, sumPhib, myThid)
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
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE GLOBAL_SUM_TILE_RL_FWD(phiTile,
     &                                  sumPhi, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     sumPhi
      INTEGER myThid

      CALL PUSHINTEGER4(myThid)
      CALL GLOBAL_SUM_TILE_RL(phiTile, sumPhi, myThid)
      CALL PUSHREAL8(sumPhi)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE GLOBAL_SUM_TILE_RL_BWD(phiTile, phiTileb,
     &                                  sumPhi, sumPhib, myThid)
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

      CALL POPREAL8(sumPhi)
      CALL POPINTEGER4(myThid)
      CALL GLOBAL_ADSUM_TILE_RL(phiTileb, sumPhib, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!     Adjoint of global_sum_r8 for arguments Arg1=(in;out)
      SUBROUTINE GLOBAL_SUM_R8_B(sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      Real*8 sumPhi
      Real*8 sumPhib
      INTEGER myThid

      CALL GLOBAL_ADSUM_R8(sumPhib, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE CG2D_B0(cg2d_b, cg2d_bb, cg2d_x, cg2d_xb,
     &                   firstResidual, minResidualSq, lastResidual,
     &                   numIters, nIterMin, myThid)
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
      cg2d_bb = 0. _d 0
      CALL CG2D( cg2d_xb, cg2d_bb,
     &           firstResidual, minResidualSq, lastResidual,
     &           numIters, nIterMin, myThid)
! [llh] the upstream cg2d_x is passive:
      cg2d_xb = 0. _d 0
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE ADEXCH_3D_RL( adVar, Nr, myThid )
      IMPLICIT NONE
      _RL    adVar
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE ADEXCH_UV_XY_RS( adU, adV, bool, myThid )
      IMPLICIT NONE
      _RS     adU, adV
      LOGICAL bool
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE ADEXCH_UV_3D_RL( aduVel, advVel, bool, Nr, myThid )
      IMPLICIT NONE
      _RL     aduVel, advVel
      LOGICAL bool
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE ADEXCH_XY_RS( adVar, myThid )
      IMPLICIT NONE
      _RS     adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE ADEXCH_XY_RL( adVar, myThid )
      IMPLICIT NONE
      _RL     adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE STREAMICE_CG_SOLVE_D(
     &                        cg_Uin, cg_Uind, cg_Vin, cg_Vind,
     &                        cg_Bu, cg_Bud, cg_Bv, cg_Bvd,
     &                        A_uu, A_uud, A_uv, A_uvd,
     &                        A_vu, A_vud, A_vv, A_vvd,
     &                        tolerance, iters, maxIter, myThid )
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      _RL cg_Uin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL cg_Uind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vind(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bud (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bvd (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL A_uud(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vud(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_uvd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vvd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL tolerance
      INTEGER iters
      INTEGER maxIter
      INTEGER myThid

C     !LOCAL VARIABLES
#ifdef ALLOW_STREAMICE
      INTEGER i, j, bi, bj
      INTEGER colx, coly
      _RL deltaAu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAuMdBu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaAvMdBv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      ! A x = b
      ! delta A x + A delta x = delta b
      ! A delta x = delta b - delta A x
      ! delta x = A^{-1} (delta b - delta A x)

      CALL STREAMICE_CG_SOLVE( cg_Uin, cg_Vin,
     &                         cg_Bu, cg_Bv,
     &                         A_uu, A_uv, A_vu, A_vv,
     &                         tolerance, iters, maxIter, myThid )

      _EXCH_XY_RL(cg_Uin, myThid)
      _EXCH_XY_RL(cg_Vin, myThid)

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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          deltaAuMdBu(i,j,bi,bj) = cg_Bud(i,j,bi,bj)-deltaAu(i,j,bi,bj)
          deltaAvMdBv(i,j,bi,bj) = cg_Bvd(i,j,bi,bj)-deltaAv(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL STREAMICE_CG_SOLVE( cg_Uind, cg_Vind,
     &                         deltaAuMdBu, deltaAvMdBv,
     &                         A_uu, A_uv, A_vu, A_vv,
     &                         tolerance, iters, maxIter, myThid )

#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      SUBROUTINE STREAMICE_CG_SOLVE_B(
     &                        cg_Uin, cg_Uinb, cg_Vin, cg_Vinb,
     &                        cg_Bu, cg_Bub, cg_Bv, cg_Bvb,
     &                        A_uu, A_uub, A_uv, A_uvb,
     &                        A_vu, A_vub, A_vv, A_vvb,
     &                        tolerance, iters, maxIter, myThid )

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#endif

      INTEGER myThid
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
      _RL tolerance
      INTEGER iters
      INTEGER maxIter

#ifdef ALLOW_STREAMICE
      INTEGER recalc_vel
      recalc_vel = 1
      CALL ADSTREAMICE_CG_SOLVE(
     U  cg_Uin,    ! velocities - need to be recalc ed
     I  cg_Uinb,      ! adjoint of vel (input)
     U  cg_Vin,    ! velocities - need to be recalc ed
     I  cg_Vinb,      ! adjoint of vel (input)
     I  cg_Bu,   ! to recalc velocities
     U  cg_Bub,     ! adjoint of RHS (output)
     I  cg_Bv,   ! to recalc velocities
     U  cg_Bvb,     ! adjoint of RHS (output)
     I  A_uu,       ! section of matrix that multiplies u and projects on u
     U  A_uub,     ! adjoint of matrix coeffs (output)
     I  A_uv,       ! section of matrix that multiplies v and projects on u
     U  A_uvb,     ! adjoint of matrix coeffs (output)
     I  A_vu,       ! section of matrix that multiplies u and projects on v
     U  A_vub,     ! adjoint of matrix coeffs (output)
     I  A_vv,       ! section of matrix that multiplies v and projects on v
     U  A_vvb,     ! adjoint of matrix coeffs (output)
     I  tolerance,
     I  maxIter,
     I  recalc_vel,
     I  myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C [llh] Hand-written forward-sweep adjoint code of STREAMICE_CG_SOLVE
C [llh] Adjoint of solving Ax=b,
C [llh]  where A is (Auu, Auv, Avu, Avv), x is (xu,xv), b is (bu,bv)
      SUBROUTINE STREAMICE_CG_SOLVE_FWD(
     &                        cg_Uin, cg_Vin, cg_Bu, cg_Bv,
     &                        A_uu, A_uv, A_vu, A_vv,
     &                        tolerance, iters, maxiters, myThid )

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
      _RL A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
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
     &  cg_Uin,
     &  cg_Vin,
     &  cg_Bu,
     &  cg_Bv,
     &  A_uu,
     &  A_uv,
     &  A_vu,
     &  A_vv,
     &  tolerance,
     &  iters,
     &  maxiters,
     &  myThid )

      CALL PUSHREAL8ARRAY(cg_Vin, (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy)
      CALL PUSHREAL8ARRAY(cg_Uin, (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy)
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C [llh] Hand-written backward-sweep adjoint code of STREAMICE_CG_SOLVE
C [llh] Adjoint of solving Ax=b,
C [llh]  where A is (Auu, Auv, Avu, Avv), x is (xu,xv), b is (bu,bv)
      SUBROUTINE STREAMICE_CG_SOLVE_BWD(
     &                        cg_Uin, cg_Uinb, cg_Vin, cg_Vinb,
     &                        cg_Bu, cg_Bub, cg_Bv, cg_Bvb,
     &                        A_uu, A_uub, A_uv, A_uvb,
     &                        A_vu, A_vub, A_vv, A_vvb,
#ifdef ALLOW_PETSC
     &                        tolerance, toleranceb,
#else
     &                        tolerance,
#endif
     &                        iters, maxIter, myThid)

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_STREAMICE
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#endif

      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL cg_Uinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vinb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL A_uub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vub (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_uvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL A_vvb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RL tolerance
#ifdef ALLOW_PETSC
      _RL toleranceb
#endif
      INTEGER iters
      INTEGER maxIter
      INTEGER myThid

#ifdef ALLOW_STREAMICE
      CALL POPREAL8ARRAY(cg_Uin, (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy)
      CALL POPREAL8ARRAY(cg_Vin, (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy)

      CALL ADSTREAMICE_CG_SOLVE(
     U  cg_Uin,    ! velocities - need to be recalc ed
     I  cg_Uinb,      ! adjoint of vel (input)
     U  cg_Vin,    ! velocities - need to be recalc ed
     I  cg_Vinb,      ! adjoint of vel (input)
     I  cg_Bu,   ! to recalc velocities
     U  cg_Bub,     ! adjoint of RHS (output)
     I  cg_Bv,   ! to recalc velocities
     U  cg_Bvb,     ! adjoint of RHS (output)
     I  A_uu,       ! section of matrix that multiplies u and projects on u
     U  A_uub,     ! adjoint of matrix coeffs (output)
     I  A_uv,       ! section of matrix that multiplies v and projects on u
     U  A_uvb,     ! adjoint of matrix coeffs (output)
     I  A_vu,       ! section of matrix that multiplies u and projects on v
     U  A_vub,     ! adjoint of matrix coeffs (output)
     I  A_vv,       ! section of matrix that multiplies v and projects on v
     U  A_vvb,     ! adjoint of matrix coeffs (output)
     I  tolerance,
     I  maxIter,
     I  myThid )
#endif
      RETURN
      END
