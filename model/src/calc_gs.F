C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gs.F,v 1.18 1999/05/18 18:01:12 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_GS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskup,maskC,
     I           K13,K23,KappaRS,KapGM,
     U           af,df,fZon,fMer,fVerS,
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
#ifdef ALLOW_KPP
#include "KPPMIX.h"
#endif

C     == Routine arguments ==
C     fZon    - Work array for flux of temperature in the east-west 
C               direction at the west face of a cell.
C     fMer    - Work array for flux of temperature in the north-south 
C               direction at the south face of a cell.
C     fVerS   - Flux of salt (S) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  - Land mask used to denote base of the domain.
C     maskC   - Land mask for salt cells (used in TOP_LAYER only)
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - Vertical volume transport through cell face
C     af      - Advective flux component work array
C     df      - Diffusive flux component work array
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K13   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL K23   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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

      afFacS = 1. _d 0
      dfFacS = 1. _d 0
      TOP_LAYER = K .EQ. 1

C---  Calculate advective and diffusive fluxes between cells.

C--   Zonal flux (fZon is at west face of "salt" cell)
C     Advective component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = 
     &   uTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Zonal tracer gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dSdx(i,j) = _recip_dxC(i,j,bi,bj)*
     &  (salt(i,j,k,bi,bj)-salt(i-1,j,k,bi,bj))
       ENDDO
      ENDDO
C     Diffusive component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -(diffKhS+0.5*(KapGM(i,j)+KapGM(i-1,j)))*
     &            xA(i,j)*dSdx(i,j)
       ENDDO
      ENDDO
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
C     Zonal tracer gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dSdy(i,j) = _recip_dyC(i,j,bi,bj)*
     &  (salt(i,j,k,bi,bj)-salt(i,j-1,k,bi,bj))
       ENDDO
      ENDDO
C     Diffusive component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -(diffKhS+0.5*(KapGM(i,j)+KapGM(i,j-1)))*
     &            yA(i,j)*dSdy(i,j)
       ENDDO
      ENDDO
C     Net meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = afFacS*af(i,j) + dfFacS*df(i,j)
       ENDDO
      ENDDO

C--   Interpolate terms for Redi/GM scheme
      DO j=jMin,jMax
       DO i=iMin,iMax
        dSdx(i,j) = 0.5*( 
     &   +0.5*(_maskW(i+1,j,k,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (salt(i+1,j,k,bi,bj)-salt(i,j,k,bi,bj))
     &        +_maskW(i,j,k,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (salt(i,j,k,bi,bj)-salt(i-1,j,k,bi,bj)))
     &   +0.5*(_maskW(i+1,j,km1,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (salt(i+1,j,km1,bi,bj)-salt(i,j,km1,bi,bj))
     &        +_maskW(i,j,km1,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (salt(i,j,km1,bi,bj)-salt(i-1,j,km1,bi,bj)))
     &       )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        dSdy(i,j) = 0.5*(
     &   +0.5*(_maskS(i,j,k,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (salt(i,j,k,bi,bj)-salt(i,j-1,k,bi,bj))
     &        +_maskS(i,j+1,k,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (salt(i,j+1,k,bi,bj)-salt(i,j,k,bi,bj)))
     &   +0.5*(_maskS(i,j,km1,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (salt(i,j,km1,bi,bj)-salt(i,j-1,km1,bi,bj))
     &        +_maskS(i,j+1,km1,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (salt(i,j+1,km1,bi,bj)-salt(i,j,km1,bi,bj)))
     &       )
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
C     Diffusive component of vertical flux
C     Note: For K=1 then KM1=1 this gives a dS/dz = 0 upper
C           boundary condition.
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = _rA(i,j,bi,bj)*(
     &   -KapGM(i,j)*K13(i,j,k)*dSdx(i,j)
     &   -KapGM(i,j)*K23(i,j,k)*dSdy(i,j)
     &   )
       ENDDO
      ENDDO
      IF (.NOT.implicitDiffusion) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j) + _rA(i,j,bi,bj)*(
     &    -KappaRS(i,j,k)*recip_drC(k)
     &    *(salt(i,j,kM1,bi,bj)-salt(i,j,k,bi,bj))*rkFac
     &    )
        ENDDO
       ENDDO
      ENDIF
#ifdef ALLOW_KPP
      IF (usingKPPmixing) THEN
C--   Add non local transport coefficient (ghat term) to right-hand-side
C     The nonlocal transport term is noNrero only for scalars in unstable
C     (convective) forcing conditions.
       IF ( TOP_LAYER ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j) - _rA(i,j,bi,bj) *
     &              EmPmR(i,j,bi,bj) * delZ(1) *
     &              ( KappaRS(i,j,k)   * KPPghat(i,j,k,bi,bj)   )
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j) - _rA(i,j,bi,bj) *
     &              EmPmR(i,j,bi,bj) * delZ(1) *
     &              ( KappaRS(i,j,k)   * KPPghat(i,j,k,bi,bj)
     &              - KappaRS(i,j,k-1) * KPPghat(i,j,k-1,bi,bj) )
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_KPP */

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
      DO j=jMin,jMax
       DO i=iMin,iMax
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

#ifdef INCLUDE_LAT_CIRC_FFT_FILTER_CODE
C--
      CALL FILTER_LATCIRCS_FFT_APPLY( gS, 1, sNy, k, k, bi, bj, 1, myThid)
#endif

      RETURN
      END
