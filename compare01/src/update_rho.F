C $Id: update_rho.F,v 1.1 1998/05/25 20:21:06 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C===================================================================
C     Procedure name: UPDATE_RHO                                   |
C           Function: Update density field using equation of state.|
C           Comments:                                              |
C===================================================================
CStartofinterface
      SUBROUTINE UPDATE_RHO(
     I                  T, S, K, eqn, ref, 
     O                  rhoPrime )
      IMPLICIT NONE
C============== Global data ==========================================
#include "SIZE.h"
#include "AJAINF.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OPERATORS.h"
#include "OLDG.h"
#include "MASKS.h"
#include "POLYEOS.h"
C============= Routine arguments =====================================
C     /--------------------------------------------------------------\
C     | T        - Potential temperature (oC).                       |
C     | S        - Salinity  (ppt).                                  |
C     | rhoPrime - Density perturbation wrt initial stable resting   |
C     |            state. The code here needs to return in situ      |
C     |            densities from an input of potential temperature. |
C     | K        - Level to evaluate                                 |
C     | ref      - Density is referenced as determined by "ref".     |
C     | eqn      - Density is calculated using equation defined by   |
C     |            "eqn".                                            |
C     \--------------------------------------------------------------/
      REAL    T        (Nx,Ny,Nz)
      REAL    S        (Nx,Ny,Nz)
      REAL    rhoPrime (Nx,Ny,Nz)
      INTEGER K
      CHARACTER*(*) ref
      CHARACTER*(*) eqn 
CEndofinterface
C============= Local variables  ======================================
      REAL sP, tP
      REAL  tD
      REAL  sD
C     Loop counters:
      INTEGER I, J
C     /--------------------------------------------------------------\
C     | Caculate in-situ density from potential temperature          |
C     | and salinity.                                                |
C     \--------------------------------------------------------------/
      IF     ( eqn .EQ. 'LINEAR' ) THEN
C      /--------------------------------------------------------------\
C      | Linear equation of state.                                    |
C      \--------------------------------------------------------------/
       IF ( ref     .EQ. 'LOCAL' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-THS(K)
          SP = S(_I3(K,I,J))-SSPPT(K)
          rhoPrime(_I3(K,I,J)) = ronil*(sBeta*sP-tAlpha*tP)
         ENDDO
        ENDDO
       ELSEIF ( ref .EQ. 'ABOVE' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-THS(K-1)
          SP = S(_I3(K,I,J))-SSPPT(K-1)
          rhoPrime(_I3(K,I,J)) = ronil*(sBeta*sP-tAlpha*tP)
         ENDDO
        ENDDO
       ELSEIF ( ref .EQ. 'BELOW' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-THS(K+1)
          SP = S(_I3(K,I,J))-SSPPT(K+1)
          rhoPrime(_I3(K,I,J)) = ronil*(sBeta*sP-tAlpha*tP)
         ENDDO
        ENDDO
       ELSE
        WRITE(0,*) ' Unknown reference point for LINEAR EOS'
C       STOP
       ENDIF
      ELSEIF ( eqn .EQ. 'POLY3' ) THEN
C      /--------------------------------------------------------------\
C      | 3rd order polynomial approximation.                          |
C      \--------------------------------------------------------------/
       IF     ( ref     .EQ. 'LOCAL' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-tRef(K)
          SP = (S(_I3(K,I,J))-35.)/1000.-sRef(K)
          rhoPrime(_I3(K,I,J))=1000.*(
     &     C(K,1)*tP   +C(K,2)*sP
     &    +C(K,3)*tP**2+C(K,4)*tP*sP   +C(K,5)*sP**2
     &    +C(K,6)*tP**3+C(K,7)*tP*sP**2+C(K,8)*sP*tP**2+C(K,9)*sP**3 )
          rhoPrime(_I3(K,I,J))=rhoPrime(_I3(K,I,J))-rPrmInit(K)
         ENDDO
        ENDDO
       ELSEIF ( ref     .EQ. 'ABOVE' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-tRef(K-1)
          SP = (S(_I3(K,I,J))-35.)/1000.-sRef(K-1)
          rhoPrime(_I3(K,I,J))=1000.*(
     &     C(K-1,1)*tP   +C(K-1,2)*sP
     &    +C(K-1,3)*tP**2+C(K-1,4)*tP*sP   +C(K-1,5)*sP**2
     &    +C(K-1,6)*tP**3+C(K-1,7)*tP*sP**2+C(K-1,8)*sP*tP**2+C(K-1,9)*sP**3 )
          rhoPrime(_I3(K,I,J))=rhoPrime(_I3(K,I,J))-rPrmInit(K-1)
         ENDDO
        ENDDO
       ELSEIF ( ref     .EQ. 'ABS_SURFACE' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-tRef(1)
          SP = (S(_I3(K,I,J))-35.)/1000.-sRef(1)
          rhoPrime(_I3(K,I,J))=(
     &     C(1,1)*tP   +C(1,2)*sP
     &    +C(1,3)*tP**2+C(1,4)*tP*sP   +C(1,5)*sP**2
     &    +C(1,6)*tP**3+C(1,7)*tP*sP**2+C(1,8)*sP*tP**2+C(1,9)*sP**3 )
          rhoPrime(_I3(K,I,J))=( rhoPrime(_I3(K,I,J))+sig0(1)+1. )*1000.
         ENDDO
        ENDDO
       ELSEIF ( ref     .EQ. 'ABS_LOCAL' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-tRef(K)
          SP = (S(_I3(K,I,J))-35.)/1000.-sRef(K)
          rhoPrime(_I3(K,I,J))=(
     &     C(K,1)*tP   +C(K,2)*sP
     &    +C(K,3)*tP**2+C(K,4)*tP*sP   +C(K,5)*sP**2
     &    +C(K,6)*tP**3+C(K,7)*tP*sP**2+C(K,8)*sP*tP**2+C(K,9)*sP**3 )
          rhoPrime(_I3(K,I,J))=( rhoPrime(_I3(K,I,J))+sig0(K)+1. )*1000.
         ENDDO
        ENDDO
       ELSEIF ( ref     .EQ. 'BELOW' ) THEN
        DO J=1,Ny
         DO I=1,Nx
          TP = T(_I3(K,I,J))-tRef(K+1)
          SP = (S(_I3(K,I,J))-35.)/1000.-sRef(K+1)
          rhoPrime(_I3(K,I,J))=1000.*(
     &     C(K+1,1)*tP   +C(K+1,2)*sP
     &    +C(K+1,3)*tP**2+C(K+1,4)*tP*sP   +C(K+1,5)*sP**2
     &    +C(K+1,6)*tP**3+C(K+1,7)*tP*sP**2+C(K+1,8)*sP*tP**2+C(K+1,9)*sP**3 )
          rhoPrime(_I3(K,I,J))=rhoPrime(_I3(K,I,J))-rPrmInit(K+1)
         ENDDO
        ENDDO
       ELSE
        WRITE(0,*) ' Unknown reference point for POLY3 EOS'
        STOP
       ENDIF
      ELSE
       WRITE(0,*) ' Unknown EOS'
       STOP
      ENDIF
C
      RETURN
      END
