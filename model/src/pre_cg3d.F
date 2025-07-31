#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "BACKWARD_COMPAT_OPTIONS.h"

CBOP
C     !ROUTINE: PRE_CG3D
C     !INTERFACE:
      SUBROUTINE PRE_CG3D(
     I                     oldFreeSurfTerm,
     I                     cg2d_x,
     U                     cg3d_b, cg3d_x,
     U                     wSurfP2d,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Called from SOLVE_FOR_PRESSURE, before 3-D solver (cg3d):
C     Finish calculation of 3-D RHS after 2-D inversionis done.

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     !FUNCTIONS:
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     oldFreeSurfTerm :: Treat free-surface term in the old way (no exactConserv)
C     cg2d_x          :: Solution vector of the 2-D solver equation A.x=b
C     cg3d_b          :: Right Hand side vector of the 3-D solver equation A.X=B
C     cg3d_x          :: Solution vector of the 3-D solver equation A.X=B
C     wSurfP2d        :: surface vertical velocity after 2-D solver
C     myTime          :: Current time in simulation
C     myIter          :: Current iteration number in simulation
C     myThid          :: My Thread Id number
      LOGICAL oldFreeSurfTerm
      _RL     cg2d_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     cg3d_b  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     cg3d_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     wSurfP2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
      INTEGER i,j,k,bi,bj
      INTEGER ks, kp1
c     CHARACTER*10 sufx
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     locGamma, tmpFac
      _RL     wFacKm, wFacKp
      _RL     uf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef PRESERVE_C69C_OUTPUT
      _RL     surfFac
#endif
CEOP

c     IF ( use3Dsolver ) THEN

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Calculate updated (after 2-D solver) vertical velocity at the surface
         IF ( selectNHfreeSurf.GE.1 ) THEN
           ks = 1
           tmpFac = oneRL / ( implicDiv2DFlow*deltaTFreeSurf )
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              locGamma = drC(1)*recip_Bo(i,j,bi,bj)
     &                 /( deltaTMom*deltaTFreeSurf
     &                   *implicitNHPress*implicDiv2DFlow )
              wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                + recip_Bo(i,j,bi,bj)*cg2d_x(i,j,bi,bj)*tmpFac
     &                 /(oneRL + locGamma)
            ENDDO
           ENDDO
#ifdef PRESERVE_C69C_OUTPUT
         ELSEIF ( solveForDeltaP ) THEN
C- to use solveForDeltaP, should NOT define PRESERVE_C69C_OUTPUT so add a STOP
           STOP 'PRE_CG3D: should not reach this point!'
         ELSEIF ( .NOT.oldFreeSurfTerm ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                            + ( etaN(i,j,bi,bj)-etaH(i,j,bi,bj) )
     &                            / ( implicDiv2DFlow*deltaTFreeSurf )
            ENDDO
           ENDDO
#else /* PRESERVE_C69C_OUTPUT */
         ELSEIF ( exactConserv ) THEN
           tmpFac = oneRL / ( implicDiv2DFlow*deltaTFreeSurf )
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                + recip_Bo(i,j,bi,bj)*cg2d_x(i,j,bi,bj)*tmpFac
            ENDDO
           ENDDO
#endif /* PRESERVE_C69C_OUTPUT */
         ENDIF

#ifdef PRESERVE_C69C_OUTPUT
C--   Add EmPmR contribution to top level cg3d_b or to wSurfP2d:
C      (has been done for cg2d_b ; and addMass was added by CALC_DIV_GHAT)
         IF ( useRealFreshWaterFlux.AND.fluidIsWater ) THEN
          IF ( oldFreeSurfTerm .OR. usingPCoords ) THEN
           IF ( usingPCoords .OR. uniformFreeSurfLev  ) THEN
            IF ( usingZCoords ) THEN
             ks = 1
             tmpFac = freeSurfFac*mass2rUnit*deepFac2F(ks)
            ELSE
             ks = Nr
             tmpFac = freeSurfFac*mass2rUnit*deepFac2F(ks+1)
            ENDIF
            tmpFac = tmpFac*implicDiv2DFlow/deltaTMom
            DO j=1,sNy
             DO i=1,sNx
              cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &          + tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
             ENDDO
            ENDDO
           ELSE
            tmpFac = freeSurfFac*mass2rUnit*implicDiv2DFlow/deltaTMom
            DO j=1,sNy
             DO i=1,sNx
              ks = kSurfC(i,j,bi,bj)
              IF ( ks.LE.Nr )
     &         cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &          + tmpFac*deepFac2F(ks)*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDIF
          ELSEIF ( selectNHfreeSurf.EQ.0 ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                            + mass2rUnit*EmPmR(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF
         ENDIF
#endif /* PRESERVE_C69C_OUTPUT */

C--   Update or Add free-surface contribution to cg3d_b:
#ifdef PRESERVE_C69C_OUTPUT
         surfFac = 0.
#endif
         IF ( .NOT.oldFreeSurfTerm ) THEN
#ifdef PRESERVE_C69C_OUTPUT
           tmpFac = freeSurfFac*implicDiv2DFlow/deltaTMom
#else
           tmpFac = freeSurfFac
#endif
           DO j=1,sNy
            DO i=1,sNx
              ks = kSurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &                      + tmpFac*wSurfP2d(i,j,bi,bj)
     &                              *_rA(i,j,bi,bj)*deepFac2F(ks)
              ENDIF
            ENDDO
           ENDDO
C-    Next: oldFreeSurfTerm=T cases (only supported with implicDiv2DFlow=1):
         ELSEIF ( uniformFreeSurfLev ) THEN
C-       surface is everywhere @ level k=1 (e.g. Z coordinate without ice-shelf)
#ifdef PRESERVE_C69C_OUTPUT
           surfFac = freeSurfFac*deepFac2F(1)
#else
           tmpFac = freeSurfFac*deepFac2F(1)/deltaTFreeSurf
           DO j=1,sNy
            DO i=1,sNx
               cg3d_b(i,j,1,bi,bj) = cg3d_b(i,j,1,bi,bj)
     &                     + tmpFac*etaN(i,j,bi,bj)*_rA(i,j,bi,bj)
            ENDDO
           ENDDO
#endif
         ELSE
C-       no assumption on surface level index (e.g., P coordinate)
#ifdef PRESERVE_C69C_OUTPUT
           tmpFac = freeSurfFac / ( deltaTFreeSurf*deltaTMom )
#else
           tmpFac = freeSurfFac / deltaTFreeSurf
#endif
           DO j=1,sNy
            DO i=1,sNx
              ks = kSurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &                     + tmpFac*etaN(i,j,bi,bj)*_rA(i,j,bi,bj)
     &                      *deepFac2F(ks)
              ENDIF
            ENDDO
           ENDDO
         ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Finish updating cg3d_b: 1) increment in horiz velocity due to new cg2d_x
C                             2) add vertical velocity contribution.
         DO j=1,sNy+1
          DO i=1,sNx+1
           uf(i,j) = -_recip_dxC(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &             * implicSurfPress*implicDiv2DFlow
#else
     &             * implicSurfPress*deltaTMom
#endif
     &             *(cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
#ifdef ALLOW_OBCS
     &             *maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
#endif
           vf(i,j) = -_recip_dyC(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &             * implicSurfPress*implicDiv2DFlow
#else
     &             * implicSurfPress*deltaTMom
#endif
     &             *(cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
#ifdef ALLOW_OBCS
     &             *maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj)
#endif
          ENDDO
         ENDDO

C Note: with implicDiv2DFlow < 1, wVel contribution to cg3d_b is similar to
C       uVel,vVel contribution to cg2d_b when exactConserv=T, since wVel is
C       always recomputed from continuity eq (like eta when exactConserv=T)
         k=1
         kp1 = MIN(k+1,Nr)
#ifdef PRESERVE_C69C_OUTPUT
         wFacKp = implicDiv2DFlow*deepFac2F(kp1)*rhoFacF(kp1)
#else
         wFacKp = deepFac2F(kp1)*rhoFacF(kp1)
#endif
         IF (k.GE.Nr) wFacKp = 0.
         DO j=1,sNy
          DO i=1,sNx
#ifdef PRESERVE_C69C_OUTPUT
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(k)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(k)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(k)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(k)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( surfFac*etaN(i,j,bi,bj)/deltaTFreeSurf
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTMom
#else
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj) + (
     &         ( ( dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &           - dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j) )
     &         + ( dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &           - dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j ) )
     &         )*drF(k)
     &       - wVel(i,j,kp1,bi,bj)*wFacKp
     &          *_rA(i,j,bi,bj)                         )
#endif
          ENDDO
         ENDDO
         DO k=2,Nr
          kp1 = MIN(k+1,Nr)
C-       deepFac & rhoFac cancel with the ones in uf[=del_i(Phi)/dx],vf ;
C        both appear in wVel term, but at 2 different levels
#ifdef PRESERVE_C69C_OUTPUT
          wFacKm = implicDiv2DFlow*deepFac2F( k )*rhoFacF( k )
          wFacKp = implicDiv2DFlow*deepFac2F(kp1)*rhoFacF(kp1)
#else
          wFacKm = deepFac2F( k )*rhoFacF( k )
          wFacKp = deepFac2F(kp1)*rhoFacF(kp1)
#endif
          IF (k.GE.Nr) wFacKp = 0.
          DO j=1,sNy
           DO i=1,sNx
#ifdef PRESERVE_C69C_OUTPUT
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(k)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(k)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(k)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(k)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j, k ,bi,bj)*wFacKm*maskC(i,j,k-1,bi,bj)
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTMom
#else
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj) + (
     &         ( ( dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &           - dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j) )
     &         + ( dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &           - dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j ) )
     &         )*drF(k)
     &       + ( wVel(i,j, k ,bi,bj)*wFacKm*maskC(i,j,k-1,bi,bj)
     &         - wVel(i,j,kp1,bi,bj)*wFacKp
     &         )*_rA(i,j,bi,bj)                         )
#endif
           ENDDO
          ENDDO
         ENDDO

#ifdef ALLOW_OBCS
C- Note: solver matrix is trivial outside OB region (main diagonal only)
C     => no real need to reset RHS (=cg3d_b) & cg3d_x, except that:
C    a) normalisation is fct of Max(RHS), which can be large ouside OB region
C      (would be different if we were solving for increment of phi_nh
C       instead of directly for phi_nh).
C       => need to reset RHS to ensure that interior solution does not depend
C       on ouside OB region.
C    b) provide directly the trivial solution cg3d_x == 0 for outside OB region
C      (=> no residual => no effect on solver convergence and interior solution)
         IF (useOBCS) THEN
          DO k=1,Nr
           DO j=1,sNy
            DO i=1,sNx
              cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &                            *maskInC(i,j,bi,bj)
              cg3d_x(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)
     &                            *maskInC(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
#endif /* ALLOW_OBCS */

C-    end bi,bj loops
        ENDDO
       ENDDO

c     ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
