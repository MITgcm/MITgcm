C $Id: inc_tracer.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C     Procedure name: INC_TRACER
C           Function: Controls steppping forward tracer field.
C            Written: 10th January 1994, by Chris Hill M.I.T.
C            Updated: Fall 1995, Moto Nakamura, M.I.T., mixing schemes
C            Updated: Fall 1995, Curtis Heisey, M.I.T., generalized routine
C           Comments:
CStartofinterface
      SUBROUTINE INC_TRACER (
     I    U, V, W,
     U    Q, QT, QTNM1,
     I    Kredi,K13,K23,K31,K32,K33,Ustar,Vstar,Wstar,
     I    surfProf,tracerStepping,tracerDiffusion,tracerBhDiffusion,
     I    tracerAdvection,tracerForcing,a4TracerXY,a2TracerZ )
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "MASKS.h"
C     ======== Routine arguments ======================
      REAL U          (_I3(Nz,Nx,Ny))
      REAL V          (_I3(Nz,Nx,Ny))
      REAL W          (_I3(Nz,Nx,Ny))
      REAL Q  (_I3(Nz,Nx,Ny))
      REAL QT    (_I3(Nz,Nx,Ny))
      REAL QTNM1 (_I3(Nz,Nx,Ny))
      REAL Kredi (_I3(Nz,Nx,Ny))
      REAL K13 (_I3(Nz,Nx,Ny))
      REAL K23 (_I3(Nz,Nx,Ny))
      REAL K31 (_I3(Nz,Nx,Ny))
      REAL K32 (_I3(Nz,Nx,Ny))
      REAL K33 (_I3(Nz,Nx,Ny))
      REAL Ustar (_I3(Nz,Nx,Ny))
      REAL Vstar (_I3(Nz,Nx,Ny))
      REAL Wstar (_I3(Nz,Nx,Ny))
      REAL surfProf(Nx,Ny,2)
      LOGICAL tracerStepping
      LOGICAL tracerDiffusion
      LOGICAL tracerBhDiffusion
      LOGICAL tracerAdvection
      LOGICAL tracerForcing
      REAL a4TracerXY
      REAL a2TracerZ
CEndofinterface
C     Local variables
C     qXM1 - Q shifted -1 in X.
C     qYM1 - Q shifted -1 in Y.
      REAL qXM1      (_I3(Nz,Nx,Ny))
      REAL qYM1      (_I3(Nz,Nx,Ny))
      REAL qZM1      (_I3(Nz,Nx,Ny))
      REAL fluxWest  (_I3(Nz,Nx,Ny))
      REAL fluxSouth (_I3(Nz,Nx,Ny))
      REAL fluxUpper (_I3(Nz,Nx,Ny))
      REAL dqDx (_I3(Nz,Nx,Ny))
      REAL dqDy (_I3(Nz,Nx,Ny))
      REAL dqDz (_I3(Nz,Nx,Ny))
      REAL dqDzBz(_I3(Nz,Nx,Ny))
      integer K

      qXM1 = Cshift(Q,DIM=_I3X,SHIFT=-1)
      qYM1 = Cshift(Q,DIM=_I3Y,SHIFT=-1)
      qZM1 = Cshift(Q,DIM=_I3Z,SHIFT=-1)
      qT        = 0.
      fluxWest  = 0.
      fluxSouth = 0.
      fluxUpper = 0.
      IF ( tracerDiffusion ) THEN
C       Include del**2 3d dissipation of Q, -1.*div[k2.grad(Q)],
C       and/or del**4 lateral dissipation of Q, divh[k4.gradh(divh[gradh(Q))].
        dQdx = (Q-qXM1)*pDdxOp
        dQdy = (Q-qYM1)*pDdyOp
        do K=2,Nz
         dQdz(_I3(K,:,:))=(G*RONIL)*rDZatW(K)*(qZM1(_I3(K,:,:))-Q(_I3(K,:,:)))
        enddo
! Extrapolate downward from above at W=0 surfaces
        dQdz=WMASK*dQdz+(1.-WMASK)*CSHIFT(dQdz,DIM=_I3Z,SHIFT=-1)
! Extrapolate upward from below at Z=0
        dQdz(_I3(1,:,:))=dQdz(_I3(2,:,:))
! Interpolate to tracer points
        dQdzBz=0.5*(dqdz+CSHIFT(dQdz,DIM=_I3Z,SHIFT=+1))*PMASK
        dQdzBz(_I3(Nz,:,:))=dQdz(_I3(Nz-1,:,:))

!??     fluxWest = XA*(Q-qXM1)*pDdxOp*umask
!??     fluxSouth = YA*(Q-qYM1)*pDdyOp*vmask
!??     IF ( tracerBhDiffusion ) fluxUpper=rPVol*PMASK*(
!??  &  Cshift(fluxWest,DIM=_I3X,SHIFT=+1)
!??  &         -fluxWest+Cshift(fluxSouth,DIM=_I3Y,SHIFT=+1)-fluxSouth)
        IF ( tracerBhDiffusion ) THEN
         fluxWest = XA*(Q-qXM1)*pDdxOp*umask
         fluxSouth = YA*(Q-qYM1)*pDdyOp*vmask
         fluxUpper=rPVol*PMASK*( Cshift(fluxWest,DIM=_I3X,SHIFT=+1)
     &         -fluxWest+Cshift(fluxSouth,DIM=_I3Y,SHIFT=+1)-fluxSouth)
        ENDIF

!moto   fluxWest = umask*difcx*XA*((1.+epslFac*slpxu*slpxu+slpyu*
!moto& slpyu)*dqDx - (1.-epslFac)*slpxu*slpyu*(dqDy+
!moto& Cshift(dqDy,DIM=_I3X,SHIFT=-1)+Cshift(dqDy,DIM=_I3Y,SHIFT=1)+Cshift(Cshift(dqDy,DIM=_I3X,SHIFT=-1),DIM=_I3Y,SHIFT=1))/4. + 
!moto& (1.-epslFac)*slpxu*(dqDz+Cshift(dqDz,DIM=_I3X,SHIFT=-1)+Cshift(dqDz,DIM=_I3Z,SHIFT=1)+
!moto& Cshift(Cshift(dqDz,DIM=_I3X,SHIFT=-1),DIM=_I3Z,SHIFT=1))/4.)/
!moto& (1.+slpxu*slpxu+slpyu*slpyu)

        fluxWest = -UMASK*XA  *0.5*(Kredi+CSHIFT(Kredi,DIM=_I3X,SHIFT=-1))
     &        *( dQDx + K13*0.5*(dQdzBz+Cshift(dQdzBz,DIM=_I3X,SHIFT=-1)) )

!moto   fluxSouth = vmask*difcy*YA*((epslFac-1.)*slpxv*slpyv*(dqDx+
!moto& Cshift(dqDx,DIM=_I3Y,SHIFT=-1)+Cshift(dqDx,DIM=_I3X,SHIFT=1)+Cshift(Cshift(dqDx,DIM=_I3X,SHIFT=1),DIM=_I3Y,SHIFT=-1)
!moto& )/4. + (1.+slpxv*slpxv+epslFac*slpyv*slpyv)*dqDy + (1.-epslFac)*
!moto& slpyv*(dqDz+Cshift(dqDz,DIM=_I3Y,SHIFT=-1)+Cshift(dqDz,DIM=_I3Z,SHIFT=1)+
!moto& Cshift(Cshift(dqDz,DIM=_I3Y,SHIFT=-1),DIM=_I3Z,SHIFT=1))/4.)/(1.+slpxv*slpxv+slpyv*slpyv)

        fluxSouth = -VMASK*YA  *0.5*(Kredi+CSHIFT(Kredi,DIM=_I3Y,SHIFT=-1))
     &        *( dQdy + K23*0.5*(dQdzBz+Cshift(dQdzBz,DIM=_I3Y,SHIFT=-1)) )

        IF ( tracerBhDiffusion ) THEN
          fluxWest = fluxWest  +
     &    a4TracerXY*XA*(fluxUpper-Cshift(fluxUpper,DIM=_I3X,SHIFT=-1))*pDdxOp
          fluxSouth = fluxSouth+
     &    a4TracerXY*YA*(fluxUpper-Cshift(fluxUpper,DIM=_I3Y,SHIFT=-1))*pDdyOp
        ENDIF

! Interpolate d/dx Q and d/dy Q to W points
        dQdx=0.5d0*(dQdx+CSHIFT(dQdx,DIM=_I3X,SHIFT=+1))*PMASK	! Interp to T
        dQdx=0.5d0*(dQdx+CSHIFT(dQdx,DIM=_I3Z,SHIFT=-1))*WMASK	! Interp to W
        dQdy=0.5d0*(dQdy+CSHIFT(dQdy,DIM=_I3Y,SHIFT=+1))*PMASK	! Interp to T
        dQdy=0.5d0*(dQdy+CSHIFT(dQdy,DIM=_I3Z,SHIFT=-1))*WMASK	! Interp to W

c note difh is positive, while difcx and difcy are negative.  (see 
c getkhkv.F)  to adjust to the sign convention of diffusion terms, 
c its multiplied by "-" here.
!moto   fluxUpper = -wmask*difh/(1.+slpxw*slpxw+slpyw*slpyw)
!moto   fluxUpper = fluxUpper*ZA*((1.-epslFac)*slpxw*(dqDx+
!moto& Cshift(dqDx,DIM=_I3X,SHIFT=1)+Cshift(dqDx,DIM=_I3Z,SHIFT=-1)+Cshift(Cshift(dqDx,DIM=_I3Z,SHIFT=-1),DIM=_I3X,SHIFT=1))/4. +
!moto& (1.-epslFac)*slpyw*(dqDy+Cshift(dqDy,DIM=_I3Z,SHIFT=-1)+Cshift(dqDy,DIM=_I3Y,SHIFT=1)+
!moto& Cshift(Cshift(dqDy,DIM=_I3Z,SHIFT=-1),DIM=_I3Y,SHIFT=1))/4. + 
!moto& (slpxw*slpxw+slpyw*slpyw+epslFac)*dqDz)
        fluxUpper = -ZA*0.5*(Kredi+CSHIFT(Kredi,DIM=_I3Z,SHIFT=-1))
     &        *( K31*dQdx + K32*dQdy + K33*dQdz )

!                                    ^
!                                    |
!                                    |
!                        this sign seems to be wrong?

! Add in vertical diffusion (Av d^2/dz^2)
! fluxUpper=-wmask*g*ronil*fluxUpper-a2TracerZ*wmask*ZA*(Q-qZM1))*pDdzOP
        fluxUpper = -WMASK*( (G*RONIL)*fluxUpper-a2TracerZ*ZA*(qZM1-Q)*pDdzOP )
        fluxUpper(_I3(1,:,:)) = 0.
c factor g*ronil comes in to convert units of fluxUpper, since
c Kv is in units of m**s-1 and gradients in vedtical are 1/Z.  
c negative sign comes from different dT/dz calculation. 
      ENDIF
      IF ( tracerAdvection ) THEN
C      Include transport of temperature by flow field.
       fluxWest  = fluxWest +XA*(qXM1*0.5+Q*0.5)*(U+Ustar)
       fluxSouth = fluxSouth+YA*(qYM1*0.5+Q*0.5)*(V+Vstar)
       fluxUpper = fluxUpper+ZA*(qZM1*0.5+Q*0.5)*(W+Wstar)
      ENDIF
      IF ( tracerDiffusion .OR. tracerAdvection ) THEN
        qT = qT - (
     &              +Cshift(fluxWest,DIM=_I3X,SHIFT=+1)-fluxWest
     &              +Cshift(fluxSouth,DIM=_I3Y,SHIFT=+1)-fluxSouth
     &              -fluxUpper+Cshift(fluxUpper,DIM=_I3Z,SHIFT=+1)
     &            )*rPVol*PMASK
      ENDIF
      IF(tracerForcing)THEN
C     Include external forcing ( e.g. surface heat fluxes, boundary fluxes )
C      Switch in the appropriate tracer forcing module 
       qT(_I3(1:2,:,:)) = qT(_I3(1:2,:,:)) 
     & - 1./(30.*oneDay)*(Q(_I3(1:2,:,:))-surfProf)
      ENDIF
C
      Q = Q + ( (1.5+abEps)*qT-(0.5+abEps)*qTNM1 )*delT*asyncfac
C

      RETURN
      END
