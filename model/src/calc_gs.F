C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gs.F,v 1.27 2001/02/07 16:27:29 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_GS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskup,maskC,
     I           KappaRS,
     U           fVerS,
     I           myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_GS                                       |
C     | o Calculate the salt tendency terms.                     |
C     |==========================================================|
C     | A procedure called EXTERNAL_FORCING_S is called from     |
C     | here. These procedures can be used to add per problem    |
C     | E-P  flux source terms.                                  |
C     | Note: Although it is slightly counter-intuitive the      |
C     |       EXTERNAL_FORCING routine is not the place to put   |
C     |       file I/O. Instead files that are required to       |
C     |       calculate the external source terms are generally  |
C     |       read during the model main loop. This makes the    |
C     |       logisitics of multi-processing simpler and also    |
C     |       makes the adjoint generation simpler. It also      |
C     |       allows for I/O to overlap computation where that   |
C     |       is supported by hardware.                          |
C     | Aside from the problem specific term the code here       |
C     | forms the tendency terms due to advection and mixing     |
C     | The baseline implementation here uses a centered         |
C     | difference form for the advection term and a tensorial   |
C     | divergence of a flux form for the diffusive term. The    |
C     | diffusive term is formulated so that isopycnal mixing and|
C     | GM-style subgrid-scale terms can be incorporated b simply|
C     | setting the diffusion tensor terms appropriately.        |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     fVerS   - Flux of salt (S) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  - Land mask used to denote base of the domain.
C     maskC   - Land mask for salt cells (used in TOP_LAYER only)
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL     myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
      LOGICAL TOP_LAYER
      _RL afFacS, dfFacS
      _RL dSdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df4   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverS is set in this subroutine
C--   the kDown is still required
      fVerS(1,1,kDown) = fVerS(1,1,kDown)
#endif
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fZon(i,j)      = 0.0
        fMer(i,j)      = 0.0
        fVerS(i,j,kUp) = 0.0
       ENDDO
      ENDDO

      afFacS = 1. _d 0
      dfFacS = 1. _d 0
      TOP_LAYER = K .EQ. 1

C---  Calculate advective and diffusive fluxes between cells.

#ifdef INCLUDE_T_DIFFUSION_CODE
C     o Zonal tracer gradient
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        dSdx(i,j) = _recip_dxC(i,j,bi,bj)*
     &  (salt(i,j,k,bi,bj)-salt(i-1,j,k,bi,bj))
       ENDDO
      ENDDO
C     o Meridional tracer gradient
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        dSdy(i,j) = _recip_dyC(i,j,bi,bj)*
     &  (salt(i,j,k,bi,bj)-salt(i,j-1,k,bi,bj))
       ENDDO
      ENDDO

C--   del^2 of S, needed for bi-harmonic (del^4) term
      IF (diffK4S .NE. 0.) THEN
       DO j=1-Oly+1,sNy+Oly-1
        DO i=1-Olx+1,sNx+Olx-1
         df4(i,j)= _recip_hFacC(i,j,k,bi,bj)
     &             *recip_drF(k)/_rA(i,j,bi,bj)
     &            *(
     &             +( xA(i+1,j)*dSdx(i+1,j)-xA(i,j)*dSdx(i,j) )
     &             +( yA(i,j+1)*dSdy(i,j+1)-yA(i,j)*dSdy(i,j) )
     &             )
        ENDDO
       ENDDO
      ENDIF
#endif

C--   Zonal flux (fZon is at west face of "salt" cell)
C     Advective component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = 
     &   uTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     o Diffusive component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -diffKhS*xA(i,j)*dSdx(i,j)
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_XTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     xA,salt,
     U     df,
     I     myThid)
#endif
C     o Add the bi-harmonic contribution
      IF (diffK4S .NE. 0.) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j) + xA(i,j)*
     &    diffK4S*(df4(i,j)-df4(i-1,j))*_recip_dxC(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF
C     Net zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = afFacS*af(i,j) + dfFacS*df(i,j)
       ENDDO
      ENDDO

C--   Meridional flux (fMer is at south face of "salt" cell)
C     Advective component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
C       Advective component of meridional flux
        af(i,j) =
     &   vTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i,j-1,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Diffusive component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -diffKhS*yA(i,j)*dSdy(i,j)
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_YTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     yA,salt,
     U     df,
     I     myThid)
#endif
C     o Add the bi-harmonic contribution
      IF (diffK4S .NE. 0.) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j) + yA(i,j)*
     &    diffK4S*(df4(i,j)-df4(i,j-1))*_recip_dyC(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C     Net meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = afFacS*af(i,j) + dfFacS*df(i,j)
       ENDDO
      ENDDO

C--   Vertical flux (fVerS) above
C     Advective component of vertical flux
C     Note: For K=1 then KM1=1 this gives a barZ(T) = T
C     (this plays the role of the free-surface correction)
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   rTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i,j,kM1,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     o Diffusive component of vertical flux
C     Note: For K=1 then KM1=1 and this gives a dS/dr = 0 upper
C           boundary condition.
      IF (implicitDiffusion) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = - _rA(i,j,bi,bj)*(
     &    KappaRS(i,j,k)*recip_drC(k)
     &    *(salt(i,j,kM1,bi,bj)-salt(i,j,k,bi,bj))*rkFac
     &    )
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_RTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     maskUp,salt,
     U     df,
     I     myThid)
#endif

#ifdef ALLOW_KPP
C--   Add non-local KPP transport term (ghat) to diffusive salt flux.
      IF (useKPP) CALL KPP_TRANSPORT_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     maskC,KappaRS,
     U     df )
#endif

C     Net vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerS(i,j,kUp) = ( afFacS*af(i,j)+  dfFacS*df(i,j) )*maskUp(i,j)
       ENDDO
      ENDDO
      IF ( TOP_LAYER ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerS(i,j,kUp) = afFacS*af(i,j)*freeSurfFac
        ENDDO
       ENDDO
      ENDIF

C--   Tendency is minus divergence of the fluxes.
C     Note. Tendency terms will only be correct for range
C           i=iMin+1:iMax-1, j=jMin+1:jMax-1. Edge points
C           will contain valid floating point numbers but 
C           they are not algorithmically correct. These points
C           are not used.
      DO j=jMin,jMax-1
       DO i=iMin,iMax-1
C     DO j=1-2,OLy+2
C      DO i=1-2,OLx+2
#define _recip_VolS1(i,j,k,bi,bj) _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
#define _recip_VolS2(i,j,k,bi,bj) /_rA(i,j,bi,bj)
        gS(i,j,k,bi,bj)=
     &   -_recip_VolS1(i,j,k,bi,bj)
     &    _recip_VolS2(i,j,k,bi,bj)
     &   *(
     &    +( fZon(i+1,j)-fZon(i,j) )
     &    +( fMer(i,j+1)-fMer(i,j) )
     &    +( fVerS(i,j,kUp)-fVerS(i,j,kDown) )*rkFac
     &    )
       ENDDO
      ENDDO

C--   External forcing term(s)
      CALL EXTERNAL_FORCING_S(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     maskC,
     I     myCurrentTime,myThid)

      RETURN
      END
