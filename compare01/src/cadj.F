C $Id: cadj.F,v 1.2 1998/06/15 05:13:54 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C===================================================================
C     Procedure name: CADJ                                         |
C           Function: Mix statically unstable adjacent layers.     |
C           Comments:                                              |
C===================================================================
CStartofinterface
      SUBROUTINE CADJ  (
     I                  K,
     U                  T, S)
      IMPLICIT NONE
C============== Global data ==========================================
#include "SIZE.h"
#include "AJAINF.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OPERATORS.h"
#include "OLDG.h"
#include "MASKS.h"
C============= Routine arguments =====================================
C     /--------------------------------------------------------------\
C     | K        - Upper level in pair of layers to mix.             |
C     | T        - Potential temperature (oC).                       |
C     | S        - Salinity  (ppt).                                  |
C     \--------------------------------------------------------------/
      REAL    T        (Nx,Ny,Nz)
      REAL    S        (Nx,Ny,Nz)
      INTEGER K
CEndofinterface
C============= Local variables  ======================================
C     Loop counters:
      INTEGER I, J
      REAL TPU, SPU, RHOPU(Nx,Ny,Nz)
      REAL TPL, SPL, RHOPL(Nx,Ny,Nz)
      REAL TMIX
      REAL SMIX
      REAL RHOMIX
C     Flags
      LOGICAL BOTTOM_LAYER
C
CcnhDebugStarts
C     Richard turned off convective adjustment!
C     RETURN
CcnhDebugEnds
C
      BOTTOM_LAYER = K .EQ. Nz
C     /--------------------------------------------------------------\
C     | Mix if sig(K) > sig(K+1)                                     |
C     |==============================================================|
C     | sig is referenced to layer K when testing the stability.     |
C     \--------------------------------------------------------------/
      IF ( .NOT. BOTTOM_LAYER ) THEN
       CALL UPDATE_RHO(
     I              T, S,   K,'LINEAR','BELOW',
     O              RHOPU )
       CALL UPDATE_RHO(
     I              T, S, K+1,'LINEAR','LOCAL',
     O              RHOPL )
       DO J=1,Ny
        DO I=1,Nx
         IF ( RHOPU(_I3(K,I,J)).GT.RHOPL(_I3(K+1,I,J)) .AND. 
     &        PMASK(_I3(K+1,I,J)) .NE. 0. ) THEN
          TMIX=( T(_I3(K, I, J) )*DELPS(K)+T(_I3(K+1, I, J) )*DELPS(K+1) )/
     &         ( DELPS(K)+DELPS(K+1) )
          T(_I3(K  , I, J) )=TMIX
          T(_I3(K+1, I, J) )=TMIX
          SMIX=( S(_I3(K, I, J) )*DELPS(K)+S(_I3(K+1, I, J) )*DELPS(K+1) )/
     &         ( DELPS(K)+DELPS(K+1) )
          S(_I3(K  , I, J) )=SMIX
          S(_I3(K+1, I, J) )=SMIX
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C
      RETURN
      END
