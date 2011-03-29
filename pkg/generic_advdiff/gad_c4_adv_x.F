C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_x.F,v 1.7 2011/03/29 15:50:30 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C4_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C4_ADV_X(
     I           bi,bj,k,
     I           uTrans, maskLocW,
     I           tracer,
     O           uT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated zonal flux due to advection of a tracer
C using centered fourth-order interpolation:
C \begin{equation*}
C F^x_{adv} = U \overline{ \theta - \frac{1}{6} \delta_{ii} \theta }^i
C \end{equation*}
C Near boundaries, the scheme reduces to a second if the flow is away
C from the boundary and to third order if the flow is towards
C the boundary.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj        :: tile indices
C  k            :: vertical level
C  uTrans       :: zonal volume transport
C  maskLocW     :: mask (either 0 or 1) at grid-cell western edge
C  tracer       :: tracer field
C  myThid       :: my thread Id number
      INTEGER bi,bj,k
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT           :: zonal advective flux
      _RL uT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Rjm,Rj,Rjp           :: differences at i-1,i,i+1
C  Rjjm,Rjjp            :: second differences at i-1,i
      INTEGER i,j
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp
CEOP

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+2,sNx+Olx-1
        Rjp = (tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj  = (tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm = (tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)
        Rjjp=(Rjp-Rj)
        Rjjm=(Rj-Rjm)
        uT(i,j) =
     &   uTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i-1,j)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
     &  +ABS( uTrans(i,j) )*0.5 _d 0*oneSixth*( Rjjp-Rjjm )
     &    *( 1. _d 0 - maskW(i-1,j,k,bi,bj)*maskW(i+1,j,k,bi,bj) )
c    &    *( 1. _d 0 - maskLocW(i-1,j)*maskLocW(i+1,j) )
       ENDDO
      ENDDO

      RETURN
      END
