C $Header: /u/gcmpack/MITgcm/model/src/ini_cartesian_grid.F,v 1.17 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CARTESIAN_GRID
C     !INTERFACE:
      SUBROUTINE INI_CARTESIAN_GRID( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_CARTESIAN_GRID                             
C     | o Initialise model coordinate system                      
C     *==========================================================*
C     | The grid arrays, initialised here, are used throughout 
C     | the code in evaluating gradients, integrals and spatial 
C     | avarages. This routine   
C     | is called separately by each thread and initialises only   
C     | the region of the domain it is "responsible" for.         
C     | Notes:                                                    
C     | Two examples are included. One illustrates the            
C     | initialisation of a cartesian grid (this routine). 
C     | The other shows the   
C     | inialisation of a spherical polar grid. Other orthonormal 
C     | grids can be fitted into this design. In this case        
C     | custom metric terms also need adding to account for the   
C     | projections of velocity vectors onto these grids.         
C     | The structure used here also makes it possible to         
C     | implement less regular grid mappings. In particular       
C     | o Schemes which leave out blocks of the domain that are   
C     |   all land could be supported.                            
C     | o Multi-level schemes such as icosohedral or cubic        
C     |   grid projections onto a sphere can also be fitted       
C     |   within the strategy we use.                             
C     |   Both of the above also require modifying the support    
C     |   routines that map computational blocks to simulation    
C     |   domain blocks.                                          
C     | Under the cartesian grid mode primitive distances in X    
C     | and Y are in metres. Disktance in Z are in m or Pa        
C     | depending on the vertical gridding mode.                  
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CARTESIAN_GRID
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER iG, jG, bi, bj, I,  J
      _RL xG0, yG0
C     "Long" real for temporary coordinate calculation
C      NOTICE the extended range of indices!!
      _RL xGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)
      _RL yGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)
C     These functions return the "global" index with valid values beyond
C     halo regions
      INTEGER iGl,jGl
      iGl(I,bi) = 1+mod(myXGlobalLo-1+(bi-1)*sNx+I+Olx*Nx-1,Nx)
      jGl(J,bj) = 1+mod(myYGlobalLo-1+(bj-1)*sNy+J+Oly*Ny-1,Ny)
CEOP

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--     "Global" index (place holder)
        jG = myYGlobalLo + (bj-1)*sNy
        iG = myXGlobalLo + (bi-1)*sNx

C--   First find coordinate of tile corner (meaning outer corner of halo)
        xG0 = 0.
C       Find the X-coordinate of the outer grid-line of the "real" tile
        DO i=1, iG-1
         xG0 = xG0 + delX(i)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO i=1, Olx
         xG0 = xG0 - delX( 1+mod(Olx*Nx-1+iG-i,Nx) )
        ENDDO
C       Find the Y-coordinate of the outer grid-line of the "real" tile
        yG0 = 0.
        DO j=1, jG-1
         yG0 = yG0 + delY(j)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO j=1, Oly
         yG0 = yG0 - delY( 1+mod(Oly*Ny-1+jG-j,Ny) )
        ENDDO

C--     Calculate coordinates of cell corners for N+1 grid-lines
        DO J=1-Oly,sNy+Oly +1
         xGloc(1-Olx,J) = xG0
         DO I=1-Olx,sNx+Olx
c         xGloc(I+1,J) = xGloc(I,J) + delX(1+mod(Nx-1+iG-1+i,Nx))
          xGloc(I+1,J) = xGloc(I,J) + delX( iGl(I,bi) )
         ENDDO
        ENDDO
        DO I=1-Olx,sNx+Olx +1
         yGloc(I,1-Oly) = yG0
         DO J=1-Oly,sNy+Oly
c         yGloc(I,J+1) = yGloc(I,J) + delY(1+mod(Ny-1+jG-1+j,Ny))
          yGloc(I,J+1) = yGloc(I,J) + delY( jGl(J,bj) )
         ENDDO
        ENDDO

C--     Make a permanent copy of [xGloc,yGloc] in [xG,yG]
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          xG(I,J,bi,bj) = xGloc(I,J)
          yG(I,J,bi,bj) = yGloc(I,J)
         ENDDO
        ENDDO

C--     Calculate [xC,yC], coordinates of cell centers
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by averaging
          xC(I,J,bi,bj) = 0.25*(
     &     xGloc(I,J)+xGloc(I+1,J)+xGloc(I,J+1)+xGloc(I+1,J+1) )
          yC(I,J,bi,bj) = 0.25*(
     &     yGloc(I,J)+yGloc(I+1,J)+yGloc(I,J+1)+yGloc(I+1,J+1) )
         ENDDO
        ENDDO

C--     Calculate [dxF,dyF], lengths between cell faces (through center)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          dXF(I,J,bi,bj) = delX( iGl(I,bi) )
          dYF(I,J,bi,bj) = delY( jGl(J,bj) )
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          dXG(I,J,bi,bj) = delX( iGl(I,bi) )
          dYG(I,J,bi,bj) = delY( jGl(J,bj) )
         ENDDO
        ENDDO

C--     The following arrays are not defined in some parts of the halo
C       region. We set them to zero here for safety. If they are ever
C       referred to, especially in the denominator then it is a mistake!
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          dXC(I,J,bi,bj) = 0.
          dYC(I,J,bi,bj) = 0.
          dXV(I,J,bi,bj) = 0.
          dYU(I,J,bi,bj) = 0.
          rAw(I,J,bi,bj) = 0.
          rAs(I,J,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Calculate [dxC], zonal length between cell centers
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx+1,sNx+Olx ! NOTE range
          dXC(I,J,bi,bj) = 0.5D0*(dXF(I,J,bi,bj)+dXF(I-1,J,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx,sNx+Olx
          dYC(I,J,bi,bj) = 0.5*(dYF(I,J,bi,bj)+dYF(I,J-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging (method I)
          dXV(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I-1,J,bi,bj))
          dYU(I,J,bi,bj) = 0.5*(dYG(I,J,bi,bj)+dYG(I,J-1,bi,bj))
C         by averaging (method II)
c         dXV(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I-1,J,bi,bj))
c         dYU(I,J,bi,bj) = 0.5*(dYC(I,J,bi,bj)+dYC(I-1,J,bi,bj))
         ENDDO
        ENDDO

C     Calculate vertical face area 
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          rA (I,J,bi,bj) = dxF(I,J,bi,bj)*dyF(I,J,bi,bj)
          rAw(I,J,bi,bj) = dxC(I,J,bi,bj)*dyG(I,J,bi,bj)
          rAs(I,J,bi,bj) = dxG(I,J,bi,bj)*dyC(I,J,bi,bj)
          rAz(I,J,bi,bj) = dxV(I,J,bi,bj)*dyU(I,J,bi,bj)
          tanPhiAtU(I,J,bi,bj) = 0.
          tanPhiAtV(I,J,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Cosine(lat) scaling
        DO J=1-OLy,sNy+OLy
         cosFacU(J,bi,bj)=1.
         cosFacV(J,bi,bj)=1.
         sqcosFacU(J,bi,bj)=1.
         sqcosFacV(J,bi,bj)=1.
        ENDDO

       ENDDO ! bi
      ENDDO ! bj

      RETURN
      END
