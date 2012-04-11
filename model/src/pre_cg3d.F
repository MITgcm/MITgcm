C $Header: /u/gcmpack/MITgcm/model/src/pre_cg3d.F,v 1.5 2012/04/11 15:49:38 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PRE_CG3D
C     !INTERFACE:
      SUBROUTINE PRE_CG3D(
     I                     oldFreeSurfTerm,
     I                     cg2d_x,
     U                     cg3d_b,
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

C     === Functions ====
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     oldFreeSurfTerm :: Treat free-surface term in the old way (no exactConserv)
C     cg2d_x          :: Solution vector of the 2-D solver equation a.x=b
C     cg3d_b          :: Right Hand side vector of the 3-D solver equation A.X=B
C     myTime          :: Current time in simulation
C     myIter          :: Current iteration number in simulation
C     myThid          :: My Thread Id number
      LOGICAL oldFreeSurfTerm
      _RL     cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj
      INTEGER ks, kp1
c     CHARACTER*10 sufx
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     tmpFac, tmpSurf
      _RL     wFacKm, wFacKp
      _RL     uf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef NONLIN_FRSURF
      _RL     tmpVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
CEOP

c     IF ( use3Dsolver ) THEN

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Add EmPmR contribution to top level cg3d_b:
C      (has been done for cg2d_b ; and addMass was added by CALC_DIV_GHAT)
        IF ( useRealFreshWaterFlux.AND.fluidIsWater ) THEN
          tmpFac = freeSurfFac*mass2rUnit*implicDiv2DFlow
          ks = 1
          IF ( usingPCoords ) ks = Nr
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &        + tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)/deltaTMom
     &                               *maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF

C--   Update or Add free-surface contribution to cg3d_b:
         IF ( select_rStar.EQ.0 .AND. .NOT.oldFreeSurfTerm ) THEN
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              IF ( selectNHfreeSurf.GE.1 ) THEN
               tmpSurf = deltaTMom*deltaTfreesurf
     &                  *Bo_surf(i,j,bi,bj)*recip_drC(1)
     &                  *implicitNHPress*implicDiv2DFlow
               tmpSurf = ( tmpSurf*( etaN(i,j,bi,bj)-etaH(i,j,bi,bj) )
     &                    +implicDiv2DFlow*deltaTfreesurf
c    &                                    *(wVel(i,j,1,bi,bj)+PmE)
     &                                    *wVel(i,j,1,bi,bj)
     &                   )/(1. _d 0 + tmpSurf )
              ELSE
               tmpSurf = etaN(i,j,bi,bj)-etaH(i,j,bi,bj)
              ENDIF
              ks = kSurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &                  +freeSurfFac*tmpSurf
c    &                  +freeSurfFac*(etaN(i,j,bi,bj)-etaH(i,j,bi,bj))
     &                              *_rA(i,j,bi,bj)*deepFac2F(ks)
     &                              /deltaTMom/deltaTfreesurf
              ENDIF
            ENDDO
           ENDDO
#ifdef NONLIN_FRSURF
         ELSEIF ( select_rStar.NE.0 ) THEN
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              ks = kSurfC(i,j,bi,bj)
              tmpVar(i,j) = freeSurfFac
     &                    *( etaN(i,j,bi,bj) - etaH(i,j,bi,bj) )
     &                    *_rA(i,j,bi,bj)*deepFac2F(ks)
     &                    /deltaTMom/deltaTfreesurf
     &                    *recip_Rcol(i,j,bi,bj)
            ENDDO
           ENDDO
           DO k=1,Nr
            DO j=1,sNy
             DO i=1,sNx
              cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &          + tmpVar(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
#endif /* NONLIN_FRSURF */
         ELSEIF ( uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
           tmpFac = freeSurfFac*deepFac2F(1)
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              ks = kSurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &              +freeSurfFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &                  *_rA(i,j,bi,bj)*deepFac2F(ks)/deltaTmom
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
     &             * implicSurfPress*implicDiv2DFlow
     &             *(cg2d_x(i,j,bi,bj)-cg2d_x(i-1,j,bi,bj))
#ifdef ALLOW_OBCS
     &             *maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
#endif
           vf(i,j) = -_recip_dyC(i,j,bi,bj)
     &             * implicSurfPress*implicDiv2DFlow
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
         wFacKp = implicDiv2DFlow*deepFac2F(kp1)*rhoFacF(kp1)
         IF (k.GE.Nr) wFacKp = 0.
         DO j=1,sNy
          DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(k)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(k)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(k)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(k)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( tmpFac*etaN(i,j,bi,bj)/deltaTfreesurf
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTmom
          ENDDO
         ENDDO
         DO k=2,Nr
          kp1 = MIN(k+1,Nr)
C-       deepFac & rhoFac cancel with the ones in uf[=del_i(Phi)/dx],vf ;
C        both appear in wVel term, but at 2 different levels
          wFacKm = implicDiv2DFlow*deepFac2F( k )*rhoFacF( k )
          wFacKp = implicDiv2DFlow*deepFac2F(kp1)*rhoFacF(kp1)
          IF (k.GE.Nr) wFacKp = 0.
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &       +drF(k)*dyG(i+1,j,bi,bj)*_hFacW(i+1,j,k,bi,bj)*uf(i+1,j)
     &       -drF(k)*dyG( i ,j,bi,bj)*_hFacW( i ,j,k,bi,bj)*uf( i ,j)
     &       +drF(k)*dxG(i,j+1,bi,bj)*_hFacS(i,j+1,k,bi,bj)*vf(i,j+1)
     &       -drF(k)*dxG(i, j ,bi,bj)*_hFacS(i, j ,k,bi,bj)*vf(i, j )
     &       +( wVel(i,j, k ,bi,bj)*wFacKm*maskC(i,j,k-1,bi,bj)
     &         -wVel(i,j,kp1,bi,bj)*wFacKp
     &        )*_rA(i,j,bi,bj)/deltaTmom
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
              phi_nh(i,j,k,bi,bj) = phi_nh(i,j,k,bi,bj)
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
