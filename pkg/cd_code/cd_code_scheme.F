C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_scheme.F,v 1.6 2013/04/08 21:31:29 jmc Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

CBOP
C !ROUTINE: CD_CODE_SCHEME

C !INTERFACE: ==========================================================
      SUBROUTINE CD_CODE_SCHEME(
     I        bi,bj,k, dPhiHydX,dPhiHydY, guFld,gvFld,
     O        guCor,gvCor,
     I        myTime, myIter, myThid)

C !DESCRIPTION:
C The C-D scheme. The less said the better :-)

C !USES: ===============================================================
C     == Global variables ==
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#ifdef ALLOW_CD_CODE
#include "CD_CODE_VARS.h"
#endif
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C     dPhiHydX,Y        :: Gradient (X & Y dir.) of Hydrostatic Potential
C  guFld,gvFld          :: Acceleration (U & V compon.) from the C grid
C  guCor,gvCor          :: Coriolis terms (2 compon.) computed on C grid
C  myTime               :: current time
C  myIter               :: current time-step number
C  myThid               :: thread number

      INTEGER bi,bj,k
      _RL dPhiHydX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    guFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gvFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    guCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gvCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid


C !LOCAL VARIABLES: ====================================================
#ifdef ALLOW_CD_CODE
C  i,j                  :: loop indices
C  pF                   :: pressure gradient
C  vF                   :: work space
C  aF                   :: work space
      INTEGER i,j
      _RL pF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL aF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ab15,ab05
      _RL phxFac, phyFac
C     Define ranges
      INTEGER iMin,iMax, jMin,jMax
      PARAMETER( iMin = 1-OLx+1 , iMax = sNx+OLx-1 )
      PARAMETER( jMin = 1-OLy+1 , jMax = sNy+OLy-1 )
CEOP

C     Adams-Bashforth weighting factors
      IF ( myIter.EQ.0 ) THEN
        ab15   =  1. _d 0
        ab05   = -0. _d 0
      ELSE
        ab15   =  1.5 _d 0 + epsAB_CD
        ab05   = -0.5 _d 0 - epsAB_CD
      ENDIF

C-- stagger time stepping: grad Phi_Hyp is not in gU,gV and needs to be added:
      IF (staggerTimeStep) THEN
        phxFac = pfFacMom
        phyFac = pfFacMom
      ELSE
        phxFac = 0.
        phyFac = 0.
      ENDIF

C-    Initialize output (dummy) arrays:
c     DO j=1-OLy,sNy+OLy
c      DO i=1-OLx,sNx+OLx
c       guCor(i,j) = 0. _d 0
c       gvCor(i,j) = 0. _d 0
c      ENDDO
c     ENDDO

C     Pressure extrapolated forward in time
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
#ifdef CD_CODE_NO_AB_CORIOLIS
C     Keep this just to reproduce old results (to get same truncation)
        pf(i,j) =
     &   ab15*(  etaN(i,j,bi,bj)*Bo_surf(i,j,bi,bj) )
     &  +ab05*(etaNm1(i,j,bi,bj)*Bo_surf(i,j,bi,bj) )
#else /* CD_CODE_NO_AB_CORIOLIS */
        pf(i,j) = Bo_surf(i,j,bi,bj)
     &          *( ab15*etaN(i,j,bi,bj) + ab05*etaNm1(i,j,bi,bj) )
#endif /* CD_CODE_NO_AB_CORIOLIS */
       ENDDO
      ENDDO

C--   Zonal velocity coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
C--   Coriolis with CD scheme allowed
C     grady(p) + gV
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx
        af(i,j) =
     &        (  gvFld(i,j)
     &          -( _recip_dyC(i,j,bi,bj)*(pf(i,j)-pf(i,j-1))
     &            +phyFac*dPhiHydY(i,j) )
     &        )*_maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &         *maskInC(i,j-1,bi,bj)*maskInC(i,j,bi,bj)
#endif
       ENDDO
      ENDDO
C     Average to Vd point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =
     &           ( (af(i,j)+af(i-1,j+1))
     &            +(af(i-1,j)+af(i,j+1)) )*0.25 _d 0
     &           *_maskW(i,j,k,bi,bj)
     &          -( _fCori( i, j,bi,bj)
     &            +_fCori(i-1,j,bi,bj) )*0.5 _d 0
#ifdef CD_CODE_NO_AB_CORIOLIS
     &           *uVel(i,j,k,bi,bj)
#else /* CD_CODE_NO_AB_CORIOLIS */
     &           *( ab15*uVel(i,j,k,bi,bj) + ab05*uNM1(i,j,k,bi,bj) )
#endif /* CD_CODE_NO_AB_CORIOLIS */
       ENDDO
      ENDDO
C     Step forward Vd
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVelD(i,j,k,bi,bj) = vVelD(i,j,k,bi,bj) + deltaTmom*vf(i,j)
       ENDDO
      ENDDO
C     Relax D grid V to C grid V
      DO j=jMin,jMax
       DO i=iMin,iMax
         vVelD(i,j,k,bi,bj) = ( rCD*vVelD(i,j,k,bi,bj)
     &                         +(1. _d 0 - rCD)
     &         *( ab15*(
     &                  (vVel(i,j,k,bi,bj)+vVel(i-1,j+1,k,bi,bj))
     &                 +(vVel(i-1,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))
     &                 )*0.25 _d 0
     &           +ab05*(
     &                  (vNM1(i,j,k,bi,bj)+vNM1(i-1,j+1,k,bi,bj))
     &                 +(vNM1(i-1,j,k,bi,bj)+vNM1(i,j+1,k,bi,bj))
     &                 )*0.25 _d 0
     &          )             )*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Calculate coriolis force on U
      DO j=jMin,jMax
       DO i=iMin,iMax
        guCor(i,j) =
     &              ( _fCori( i, j,bi,bj)
     &               +_fCori(i-1,j,bi,bj) )*0.5 _d 0
     &              *vVelD(i,j,k,bi,bj)*cfFacMom
       ENDDO
      ENDDO

C--   Meridional velocity coriolis term
C     gradx(p)+gU
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
        af(i,j) =
     &        (  guFld(i,j)
     &          -( _recip_dxC(i,j,bi,bj)*(pf(i,j)-pf(i-1,j))
     &            +phxFac*dPhiHydX(i,j) )
     &        )*_maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &         *maskInC(i-1,j,bi,bj)*maskInC(i,j,bi,bj)
#endif
       ENDDO
      ENDDO
C     Average to Ud point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =
     &           ( (af(i,j)+af(i+1,j-1))
     &            +(af(i+1,j)+af(i,j-1)) )*0.25 _d 0
     &           *_maskS(i,j,k,bi,bj)
     &          +( _fCori(i, j, bi,bj)
     &            +_fCori(i,j-1,bi,bj) )*0.5 _d 0
#ifdef CD_CODE_NO_AB_CORIOLIS
     &           *vVel(i,j,k,bi,bj)
#else /* CD_CODE_NO_AB_CORIOLIS */
     &           *( ab15*vVel(i,j,k,bi,bj) + ab05*vNM1(i,j,k,bi,bj) )
#endif /* CD_CODE_NO_AB_CORIOLIS */
       ENDDO
      ENDDO
C     Step forward Ud
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVelD(i,j,k,bi,bj) = uVelD(i,j,k,bi,bj) + deltaTmom*vf(i,j)
       ENDDO
      ENDDO
C     Relax D grid U to C grid U
      DO j=jMin,jMax
       DO i=iMin,iMax
         uVelD(i,j,k,bi,bj) = ( rCD*uVelD(i,j,k,bi,bj)
     &                         +(1. _d 0 - rCD)
     &         *( ab15*(
     &                  (uVel(i,j,k,bi,bj)+uVel(i+1,j-1,k,bi,bj))
     &                 +(uVel(i,j-1,k,bi,bj)+uVel(i+1,j,k,bi,bj))
     &                 )*0.25 _d 0
     &           +ab05*(
     &                  (uNM1(i,j,k,bi,bj)+uNM1(i+1,j-1,k,bi,bj))
     &                 +(uNM1(i,j-1,k,bi,bj)+uNM1(i+1,j,k,bi,bj))
     &                 )*0.25 _d 0
     &          )             )*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Calculate coriolis force on V
      DO j=jMin,jMax
       DO i=iMin,iMax
        gvCor(i,j) =
     &             -( _fCori(i, j, bi,bj)
     &               +_fCori(i,j-1,bi,bj) )*0.5 _d 0
     &              *uVelD(i,j,k,bi,bj)*cfFacMom
       ENDDO
      ENDDO

C--   Save "previous time level" variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uNM1(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
         vNM1(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_CD_CODE */

      RETURN
      END
