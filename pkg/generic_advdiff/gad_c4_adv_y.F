C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_y.F,v 1.4 2001/09/21 13:11:43 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C4_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C4_ADV_Y( 
     I           bi,bj,k,
     I           vTrans,
     I           tracer,
     O           vT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional flux due to advection of a tracer
C using centered fourth-order interpolation:
C \begin{equation*}
C F^y_{adv} = V \overline{ \theta - \frac{1}{6} \delta_{jj} \theta }^j
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
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vTrans               :: meridional volume transport
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                   :: zonal advective flux
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Rjm,Rj,Rjp           :: differences at j-1,j,j+1
C  Rjjm,Rjjp            :: second differences at j-1,j
      INTEGER i,j
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp
CEOP

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
       vT(i,2-Oly)=0.
       vT(i,sNy+Oly)=0.
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i,j))
c    &     *maskS(i,j+1,k,bi,bj)
        Rj =(tracer(i,j)-tracer(i,j-1))
c    &     *maskS(i,j,k,bi,bj)
        Rjm=(tracer(i,j-1)-tracer(i,j-2))
c    &     *maskS(i,j-1,k,bi,bj)
C- jmc: mask not needed here above if Rjjp & Rjjm & vTrans are masked
        Rjjp=(Rjp-Rj)*maskS(i,j+1,k,bi,bj)
        Rjjm=(Rj-Rjm)*maskS(i,j-1,k,bi,bj)
        vT(i,j) =
     &   vTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i,j-1)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
     &  +ABS( vTrans(i,j) )*0.5 _d 0*oneSixth*( Rjjp-Rjjm )
     &    *( 1. _d 0 - maskS(i,j-1,k,bi,bj)*maskS(i,j+1,k,bi,bj) )
       ENDDO
      ENDDO

      RETURN
      END
