C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_gs.F,v 1.6 1998/05/30 02:10:16 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_GS( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,wTrans,maskup,
     U           af,df,fZon,fMer, fVerS,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_GS                                       |
C     | o Calculate the salinity tendency terms.                 |
C     |==========================================================|
C     | A procedure called EXTERNAL_FORCING_S is called from     |
C     | here. These procedures can be used to add per problem    |
C     | fresh water flux source terms.                           |
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
C     fVerS   - Flux of salinity (S) in the vertical 
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
C     myThid - Instance number for this innvocation of CALC_GS
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j,k
      INTEGER afFacS, dfFacS

      afFacS = 1. _d 0
      dfFacS = 1. _d 0

C---
C---  Calculate advective and diffusive fluxes between cells.
C---

C--   Zonal flux (fZon is at west face of "salt" cell)
C     Advective component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = 
     &   uTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Diffusive component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = 
     &   -diffKhS*xA(i,j)*_rdxC(i,j,bi,bj)
     &   *(salt(i,j,k,bi,bj)-salt(i-1,j,k,bi,bj))
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
C     Diffusive component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) =
     &   -diffKhS*yA(i,j)*_rdyC(i,j,bi,bj)
     &   *(salt(i,j,k,bi,bj)-salt(i,j-1,k,bi,bj))
       ENDDO
      ENDDO
C     Net meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = afFacS*af(i,j) + dfFacS*df(i,j)
       ENDDO
      ENDDO

C--   Vertical flux (fVerS) above
C     Note: For K=1 then KM1=1 this gives a dS/dz = 0 upper
C           boundary condition.
C     Advective component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   wTrans(i,j)*(salt(i,j,k,bi,bj)+salt(i,j,kM1,bi,bj))*0.5 _d 0
       ENDDO
      ENDDO
C     Diffusive component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) =
     &   -diffKzS*_zA(i,j,bi,bj)*rdzC(k)
     &   *(salt(i,j,kM1,bi,bj)-salt(i,j,k,bi,bj))
       ENDDO
      ENDDO
C     Net vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerS(i,j,kUp) = (afFacS*af(i,j) + dfFacS*df(i,j))*maskUp(i,j)
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
        gS(i,j,k,bi,bj)=
     &   -_rhFacC(i,j,k,bi,bj)*rdzF(k)*_rdxF(i,j,bi,bj)*_rdyF(i,j,bi,bj)
     &   *(
     &    +( fZon(i+1,j)-fZon(i,j) )
     &    +( fMer(i,j+1)-fMer(i,j) )
     &    +( fVerS(i,j,kUp)-fVerS(i,j,kDown) )
     &    )
       ENDDO
      ENDDO

C--   External haline forcing term(s)

      RETURN
      END
