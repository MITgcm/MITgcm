C $Id: update_w.F,v 1.1 1998/05/25 20:21:06 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C/-------------------------------------------------------------------\
C|||  Procedure: UPDATE_W                                          |||
C|||===============================================================|||
C|||        Function: Step forward W.                              |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE UPDATE_W( 
     I             U, V, PS,         
     O             W )
       IMPLICIT NONE
C     /--------------------------------------------------------------\
C     | Global data                                                  |
C     |==============================================================|
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "AJAINF.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OLDG.h"
#include "MASKS.h"
C     /--------------------------------------------------------------\
C     | Routine arguments                                            |
C     |==============================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                 |
C     | PS      - Surface pressure (m).                              |
C     \--------------------------------------------------------------/
      REAL    U (Nx,Ny,Nz)
      REAL    V (Nx,Ny,Nz)
      REAL    PS(Nx,Ny   )
      REAL    W (Nx,Ny,Nz)
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I,J,K   - Loop counters.                                     |
C     | tmp     - Work array.                                        |
C     \--------------------------------------------------------------/
      REAL    tmp(0:Nx+1,0:Ny+1)
      INTEGER I
      INTEGER J
      INTEGER K
C
      W = 0.   
C
      DO K = Nz, 1, -1
       tmp(1:Nx,1:Ny) = U(_I3(K,:,:))*XA(_I3(K,:,:))
       tmp(Nx+1,1:Ny) = tmp(1,1:Ny)
       DO J=1,Ny
        DO I=1,Nx
         W(_I3(K,I,J)) = ( tmp(I+1,J)-tmp(I,J) )
        ENDDO
       ENDDO
       tmp(1:Nx,1:Ny) = V(_I3(K,:,:))*YA(_I3(K,:,:))
       tmp(1:Nx,Ny+1) = tmp(1:Nx,1)
       DO J=1,Ny
        DO I=1,Nx
         W(_I3(K,I,J)) = W(_I3(K,I,J))+( tmp(I,J+1)-tmp(I,J) )
        ENDDO
       ENDDO
       IF ( K .NE. Nz ) THEN
        DO J=1,Ny
         DO I=1,Nx
          W(_I3(K,I,J)) = W(_I3(K,I,J)) + W(_I3(K+1,I,J))*ZA(_I3(K+1,I,J))
         ENDDO
        ENDDO
       ENDIF
       DO J=1,Ny
        DO I=1,Nx
         IF ( ZA(_I3(K,I,J)) .NE. 0. ) THEN
          W(_I3(K,I,J)) = W(_I3(K,I,J))/ZA(_I3(K,I,J))
         ELSE
          W(_I3(K,I,J)) = 0.
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     Rigid lid
      IF ( freeSurfFac .EQ. 0. ) W(_I3(1,:,:)) = 0.
C
C     _D(( ' S/R UPDATE_W: MAXVAL(W) = ',MAXVAL(W) ))
      RETURN
      END
