C $Id: update_uv.F,v 1.1 1998/05/25 20:21:06 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C/-------------------------------------------------------------------\
C|||  Procedure: UPDATE_UV                                         |||
C|||===============================================================|||
C|||        Function: Step forward U and V using calculated        |||
C|||                  tendency and pressure terms.                 |||
C|||        Comments:                                              |||
C\-------------------------------------------------------------------/
CStartofinterface
      SUBROUTINE UPDATE_UV(
     I             PH, PS, GU, GV, K,
     U             U, V )
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
C     | U, V    - X,Y,Z Velocity ( m/s, m/s ).                       |
C     | PH      - Hydrostatic pressure perturbation (m).             |
C     | PS      - Surface pressure (m).                              |
C     | GU      - d/dt U (m/s/s).                                    |
C     | GU      - d/dt V (m/s/s).                                    |
C     \--------------------------------------------------------------/
      REAL    U (Nx,Ny,Nz)
      REAL    V (Nx,Ny,Nz)
      REAL    PH(Nx,Ny,Nz)
      REAL    PS(Nx,Ny   )
      REAL    GU(Nx,Ny,Nz)
      REAL    GV(Nx,Ny,Nz)
      INTEGER K
CEndofinterface
C     /--------------------------------------------------------------\
C     | Local variables                                              |
C     |==============================================================|
C     | I,J     - Loop counters.                                     |
C     | tmp     - Work array.                                        |
C     \--------------------------------------------------------------/
      REAL    tmp(0:Nx,0:Ny)
      INTEGER I
      INTEGER J
C   
      DO J=1,Ny
       DO I=1,Nx
        tmp(I,J)=G*PS(I,J)+0.*G*PH(_I3(K,I,J))
       ENDDO
      ENDDO
      DO J=1,Ny
       tmp(0,J)=tmp(Nx,J)
      ENDDO
      DO I=1,Nx
       tmp(I,0)=tmp(I,Ny)
      ENDDO
C
      DO J=1,Ny
       DO I=1,Nx
        UNM1(_I3(K,I,J))=U(_I3(K,I,J))
        VNM1(_I3(K,I,J))=V(_I3(K,I,J))
        U(_I3(K,I,J)) = U(_I3(K,I,J))+
     &  DELT*( GU(_I3(K,I,J))-pDdxOp(_I3(K,I,J))*(tmp(I,J)-tmp(I-1,J)) )
        V(_I3(K,I,J)) = V(_I3(K,I,J))+
     &  DELT*( GV(_I3(K,I,J))-pDdyOp(_I3(K,I,J))*(tmp(I,J)-tmp(I,J-1)) )
       ENDDO
      ENDDO
C
      RETURN
      END
