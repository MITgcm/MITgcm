C $Id: pfind.F,v 1.2 1998/06/23 13:36:42 adcroft Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
# define _D(a) 
C=====================================================================
C|||    Procedure name: PFIND                                      |||
C|||          Function: Diagnose new pressure field such that      |||
C|||                    del^2(PS) = RHS2D - del^2(PH)              |||
C|||          Comments:                                            |||
C=====================================================================
CStartOfInterface
      SUBROUTINE PFIND( RHS2D, rPrmBz, divH,
     O            PH , PS )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     |    ==== Global data ==========                               |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "AJAINF.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
C     /--------------------------------------------------------------\
C     |    ==== Routine arguments ====                               |
C     | RHS2D   - div(GuHat) - 1/dt*div(UHat)                        |
C     | rprmBz  - Density anomaly wrt inert reference (kg/m**3)      |
C     | divH    - Vertically integrated divergent flow (m**3/s)      |
C     | PH      - Hydrostatic "pressure" (m).                        |
C     | PS      - Surface "pressure" (m).                            |
C     \--------------------------------------------------------------/
      REAL rPrmBz(Nx,Ny,Nz)
      REAL RHS2D (Nx,Ny   )
      REAL divH  (Nx,Ny   )
      REAL PH    (Nx,Ny,Nz)
      REAL PS    (Nx,Ny   )
CEndOfInterface
C     /--------------------------------------------------------------\
C     |    ==== Local variables ======                               |
C     | PHxhat  - Vertically integrated d/dx PH.                     |
C     | PHyhat  - Vertically integrated d/dy PH.                     |
C     | divHMx  - Maximum vert. intergrated divergence (s**-1).      |
C     | MAXIT   - Limit on number of CG iterations.                  |
C     | rMax    - Max. absolute value of elliptic eqn. rhs.          |
C     | toler   - tolerance for CG convergence.                      |
C     | freqCheck - Interval at which CG convergence is tested.      |
C     | rStart2   - Starting CG residual.                            |
C     | rFinal2   - Converged CG residual.                           |
C     | numIt2    - No. of CG iterations to converge.                |
C     \--------------------------------------------------------------/
      REAL PHxhat (Nx+1,Ny+1)
      REAL PHyhat (Nx+1,Ny+1)
      REAL    divHMx
      INTEGER MAXIT
      REAL    rMax
      REAL    toler
      INTEGER freqCheck
      INTEGER numIt2
      REAL    rStart2
      REAL    rFinal2
C     /--------------------------------------------------------------\
C     | Loop counters                                                |
C     \--------------------------------------------------------------/
      INTEGER I, J, K

C     _D(( ' S/R PFIND: MAXVAL(RHS2d) = ',MAXVAL(RHS2d)))
c     WRITE(0,*) ' S/R PFIND: MAXVAL(RHS2d) = ',MAXVAL(RHS2d)

      DO J=1,Ny
       DO I=1,Nx
C       /-------------------------------------------------------------\
C       | RHS2d <- RHS2d - (Dx{PHxHat}+Dy{PHyHat})/RONIL              |
C       |                + fsFac*ZA*PS/DT**2                          |
C       \-------------------------------------------------------------/
        RHS2d(I,J) = RHS2d(I,J)
     &   -ZA(_I3(1,I,J))*freeSurfFac*PS(I,J)/DELT/DELT
        RHS2d(I,J) = RHS2d(I,J)*PMASK(I,J,1)/aNorm2d
       ENDDO
      ENDDO
      divHMx = MAXVAL(divH*rPvolHat)
      IF ( divHMx .GT. divHMax ) THEN
       IF ( toler2d .GT. 1.E-15) toler2d = toler2d * 0.9
      ELSEIF ( divHMx .LT. divHMin ) THEN
       IF ( toler2d .LT. 1.    ) toler2d = toler2d * 1.1
      ENDIF
c     WRITE(0,*) ' S/R PFIND MAX(DIVxy) = ',divHMx
c     WRITE(0,*) ' S/R PFIND normalisation factor  = ',1.D0/aNorm2d
C     RHS2d = RHS2d
      rMax = MAXVAL(ABS(RHS2d))
      _D(( ' S/R PFIND: aNorm2d = ',aNorm2d))
      _D(( ' S/R PFIND: rMax = ',rMax))
      PSNM1 = PS
      IF ( rMax .EQ. 0. ) THEN
       PS = 0.
      ELSE
       RHS2d = RHS2d/rMax
       PS    = PS/rMax
       MAXIT = MAX2DIT
       TOLER = TOLER2D
       FREQCHECK = MAX(freqCheckToler2d,1)
C      /--------------------------------------------------------------\
C      | del2{PS} = RHS2d                                             |
C      \--------------------------------------------------------------/
C     OPEN (11,FILE='rhs',FORM='unformatted')
C     WRITE(11) RHS2d
C     CLOSE(11)

       CALL CG2D_OLD(
C      CALL CG2D    (
     I             RHS2d, maxIt, freqCheck, toler,
     U             PS,
     O             numIt2, rStart2, rFinal2
     &            )
       PS = PS*rMax
       IF ( numIt2 .GE. maxIt ) THEN
        WRITE(0,*) 'WARNING: 2d solver used maximum iterations.'
        WRITE(0,*) ' Requested tolerance = ',toler
        WRITE(0,*) '  Tolerance achieved = ',sqrt(rFinal2)
        WRITE(0,*) '       2d iterations = ',numIt2
        WRITE(0,*) '      Model timestep = ',nIter
        WRITE(0,*) '          Model time = ',currentTime
       ENDIF
      ENDIF
      _D(( ' S/R PFIND: MAXVAL(PS) = ',MAXVAL(PS) ))
      _D(( ' S/R PFIND: MINVAL(PS) = ',MINVAL(PS) ))
CcnhDebugStarts
c     WRITE(0,*) '  S/R PFIND: nIts2, resid2 ', numIt2, sqrt(rFinal2)
CcnhDebugEnds
C
      RETURN
      END
