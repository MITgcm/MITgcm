C $Header: /u/gcmpack/MITgcm/model/src/pre_cg3d.F,v 1.1 2009/12/11 04:31:31 jmc Exp $
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
#ifdef ALLOW_OBCS
#include "OBCS.h"
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
c     INTEGER ioUnit
c     CHARACTER*10 sufx
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     tmpFac
      _RL     wFacKm, wFacKp
      _RL     uf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     vf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#ifdef NONLIN_FRSURF
      _RL     tmpVar(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
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
          tmpFac = freeSurfFac*mass2rUnit
          IF (exactConserv)
     &     tmpFac = freeSurfFac*mass2rUnit*implicDiv2DFlow
          ks = 1
          IF ( usingPCoords ) ks = Nr
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &        + tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)/deltaTMom
           ENDDO
          ENDDO
        ENDIF

C--   Update or Add free-surface contribution to cg3d_b:
c        IF ( select_rStar.EQ.0 .AND. exactConserv ) THEN
         IF ( select_rStar.EQ.0 .AND. .NOT.oldFreeSurfTerm ) THEN
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              ks = ksurfC(i,j,bi,bj)
              IF ( ks.LE.Nr ) THEN
               cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &                  +freeSurfFac*(etaN(i,j,bi,bj)-etaH(i,j,bi,bj))
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
              ks = ksurfC(i,j,bi,bj)
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
         ELSEIF ( usingZCoords ) THEN
C-       Z coordinate: assume surface @ level k=1
           tmpFac = freeSurfFac*deepFac2F(1)
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
           tmpFac = 0.
           DO j=1,sNy
            DO i=1,sNx
              ks = ksurfC(i,j,bi,bj)
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
           vf(i,j) = -_recip_dyC(i,j,bi,bj)
     &             * implicSurfPress*implicDiv2DFlow
     &             *(cg2d_x(i,j,bi,bj)-cg2d_x(i,j-1,bi,bj))
          ENDDO
         ENDDO

#ifdef ALLOW_OBCS
         IF (useOBCS) THEN
          DO i=1,sNx+1
C Northern boundary
           IF (OB_Jn(i,bi,bj).NE.0)
     &      vf(i,OB_Jn(i,bi,bj)) = 0.
C Southern boundary
           IF (OB_Js(i,bi,bj).NE.0)
     &      vf(i,OB_Js(i,bi,bj)+1) = 0.
          ENDDO
          DO j=1,sNy+1
C Eastern boundary
           IF (OB_Ie(j,bi,bj).NE.0)
     &      uf(OB_Ie(j,bi,bj),j) = 0.
C Western boundary
           IF (OB_Iw(j,bi,bj).NE.0)
     &      uf(OB_Iw(j,bi,bj)+1,j) = 0.
          ENDDO
         ENDIF
#endif /* ALLOW_OBCS */

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
         IF (useOBCS) THEN
          DO k=1,Nr
           DO i=1,sNx
C Northern boundary
            IF (OB_Jn(i,bi,bj).NE.0)
     &       cg3d_b(i,OB_Jn(i,bi,bj),k,bi,bj) = 0.
C Southern boundary
            IF (OB_Js(i,bi,bj).NE.0)
     &       cg3d_b(i,OB_Js(i,bi,bj),k,bi,bj) = 0.
           ENDDO
           DO j=1,sNy
C Eastern boundary
            IF (OB_Ie(j,bi,bj).NE.0)
     &       cg3d_b(OB_Ie(j,bi,bj),j,k,bi,bj) = 0.
C Western boundary
            IF (OB_Iw(j,bi,bj).NE.0)
     &       cg3d_b(OB_Iw(j,bi,bj),j,k,bi,bj) = 0.
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
