C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gt.F,v 1.7 1998/05/28 15:09:29 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_GT( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,wTrans,maskup,
     I           K13,K23,K33,KapGM,
     U           af,df,fZon,fMer,fVerT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_GT                                       |
C     | o Calculate the temperature tendency terms.              |
C     |==========================================================|
C     | A procedure called EXTERNAL_FORCING_T is called from     |
C     | here. These procedures can be used to add per problem    |
C     | heat flux source terms.                                  |
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

C     == Routine arguments ==
C     fZon    - Work array for flux of temperature in the east-west 
C               direction at the west face of a cell.
C     fMer    - Work array for flux of temperature in the north-south 
C               direction at the south face of a cell.
C     fVerT   - Flux of temperature (T) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  - Land mask used to denote base of the domain.
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     wTrans  - Vertical volume transport through cell face
C     af      - Advective flux component work array
C     df      - Diffusive flux component work array
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K13   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K23   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K33   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL KapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
      _RL afFacT, dfFacT
      _RL dTdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      afFacT = 1. _d 0
      dfFacT = 1. _d 0

C---  Calculate advective and diffusive fluxes between cells.

C--   Zonal flux (fZon is at west face of "theta" cell)
C     Advective component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = 
     &   uTrans(i,j)*(theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Zonal tracer gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdx(i,j) = _rdxC(i,j,bi,bj)*
     &  (theta(i,j,k,bi,bj)-theta(i-1,j,k,bi,bj))
       ENDDO
      ENDDO
C     Diffusive component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -(diffKhT+0.5*(KapGM(i,j)+KapGM(i-1,j)))*
     &            xA(i,j)*dTdx(i,j)
       ENDDO
      ENDDO
C     Net zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = afFacT*af(i,j) + dfFacT*df(i,j)
       ENDDO
      ENDDO

C--   Meridional flux (fMer is at south face of "theta" cell)
C     Advective component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
C       Advective component of meridional flux
        af(i,j) =
     &   vTrans(i,j)*(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Zonal tracer gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdy(i,j) = _rdyC(i,j,bi,bj)*
     &  (theta(i,j,k,bi,bj)-theta(i,j-1,k,bi,bj))
       ENDDO
      ENDDO
C     Diffusive component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = -(diffKhT+0.5*(KapGM(i,j)+KapGM(i,j-1)))*
     &            yA(i,j)*dTdy(i,j)
       ENDDO
      ENDDO
C     Net meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = afFacT*af(i,j) + dfFacT*df(i,j)
       ENDDO
      ENDDO

C--   Interpolate terms for Redi/GM scheme
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdx(i,j) = 0.5*( 
     &   +0.5*(maskW(i+1,j,k,bi,bj)*_rdxC(i+1,j,bi,bj)*
     &           (theta(i+1,j,k,bi,bj)-theta(i,j,k,bi,bj))
     &        +maskW(i,j,k,bi,bj)*_rdxC(i,j,bi,bj)*
     &           (theta(i,j,k,bi,bj)-theta(i-1,j,k,bi,bj)))
     &   +0.5*(maskW(i+1,j,km1,bi,bj)*_rdxC(i+1,j,bi,bj)*
     &           (theta(i+1,j,km1,bi,bj)-theta(i,j,km1,bi,bj))
     &        +maskW(i,j,km1,bi,bj)*_rdxC(i,j,bi,bj)*
     &           (theta(i,j,km1,bi,bj)-theta(i-1,j,km1,bi,bj)))
     &       )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdy(i,j) = 0.5*(
     &   +0.5*(maskS(i,j,k,bi,bj)*_rdyC(i,j,bi,bj)*
     &           (theta(i,j,k,bi,bj)-theta(i,j-1,k,bi,bj))
     &        +maskS(i,j+1,k,bi,bj)*_rdyC(i,j+1,bi,bj)*
     &           (theta(i,j+1,k,bi,bj)-theta(i,j,k,bi,bj)))
     &   +0.5*(maskS(i,j,km1,bi,bj)*_rdyC(i,j,bi,bj)*
     &           (theta(i,j,km1,bi,bj)-theta(i,j-1,km1,bi,bj))
     &        +maskS(i,j+1,km1,bi,bj)*_rdyC(i,j+1,bi,bj)*
     &           (theta(i,j+1,km1,bi,bj)-theta(i,j,km1,bi,bj)))
     &       )
       ENDDO
      ENDDO

C--   Vertical flux (fVerT) above
C     Advective component of vertical flux
C     Note: For K=1 then KM1=1 this gives a barZ(T) = T
C     (this plays the role of the free-surface correction)
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   wTrans(i,j)*(theta(i,j,k,bi,bj)+theta(i,j,kM1,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Diffusive component of vertical flux
C     Note: For K=1 then KM1=1 this gives a dT/dz = 0 upper
C           boundary condition.
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = zA(i,j,bi,bj)*(
     &   -(diffKzT+KapGM(i,j)*K33(i,j,k))*rdzC(k)
     &   *(theta(i,j,kM1,bi,bj)-theta(i,j,k,bi,bj))
     &   -KapGM(i,j)*K13(i,j,k)*dTdx(i,j)
     &   -KapGM(i,j)*K23(i,j,k)*dTdy(i,j)
     &   )
       ENDDO
      ENDDO
C     Net vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerT(i,j,kUp) = (afFacT*af(i,j) + dfFacT*df(i,j))*maskUp(i,j)
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes.
C     Note. Tendency terms will only be correct for range
C           i=iMin+1:iMax-1, j=jMin+1:jMax-1. Edge points
C           will contain valid floating point numbers but 
C           they are not algorithmically correct. These points
C           are not used.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gT(i,j,k,bi,bj)=
     &   -_rhFacC(i,j,k,bi,bj)*rdzF(k)*_rdxF(i,j,bi,bj)*_rdyF(i,j,bi,bj)
     &   *(
     &    +( fZon(i+1,j)-fZon(i,j) )
     &    +( fMer(i,j+1)-fMer(i,j) )
     &    +( fVerT(i,j,kUp)-fVerT(i,j,kDown) )
     &    )
       ENDDO
      ENDDO

C--   External thermal forcing term(s)

      RETURN
      END
