C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_visc.F,v 1.24 2006/11/23 09:49:36 mlosch Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"


      SUBROUTINE MOM_CALC_VISC(
     I        bi,bj,k,
     O        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     O        harmonic,biharmonic,useVariableViscosity,
     I        hDiv,vort3,tension,strain,KE,hFacZ,
     I        myThid)

      IMPLICIT NONE
C
C     Calculate horizontal viscosities (L is typical grid width)
C     harmonic viscosity=
C       viscAh (or viscAhD on div pts and viscAhZ on zeta pts)
C       +0.25*L**2*viscAhGrid/deltaT
C       +sqrt((viscC2leith/pi)**6*grad(Vort3)**2
C             +(viscC2leithD/pi)**6*grad(hDiv)**2)*L**3
C       +(viscC2smag/pi)**2*L**2*sqrt(Tension**2+Strain**2)
C
C     biharmonic viscosity=
C       viscA4 (or viscA4D on div pts and viscA4Z on zeta pts)
C       +0.25*0.125*L**4*viscA4Grid/deltaT (approx)
C       +0.125*L**5*sqrt((viscC4leith/pi)**6*grad(Vort3)**2
C                        +(viscC4leithD/pi)**6*grad(hDiv)**2)
C       +0.125*L**4*(viscC4smag/pi)**2*sqrt(Tension**2+Strain**2)
C
C     Note that often 0.125*L**2 is the scale between harmonic and
C     biharmonic (see Griffies and Hallberg (2000))
C     This allows the same value of the coefficient to be used
C     for roughly similar results with biharmonic and harmonic
C
C     LIMITERS -- limit min and max values of viscosities
C     viscAhRemax is min value for grid point harmonic Reynolds num
C      harmonic viscosity>sqrt(2*KE)*L/viscAhRemax
C
C     viscA4Remax is min value for grid point biharmonic Reynolds num
C      biharmonic viscosity>sqrt(2*KE)*L**3/8/viscA4Remax
C
C     viscAhgridmax is CFL stability limiter for harmonic viscosity
C      harmonic viscosity<0.25*viscAhgridmax*L**2/deltaT
C
C     viscA4gridmax is CFL stability limiter for biharmonic viscosity
C      biharmonic viscosity<viscA4gridmax*L**4/32/deltaT (approx)
C
C     viscAhgridmin and viscA4gridmin are lower limits for viscosity:
C      harmonic viscosity>0.25*viscAhgridmax*L**2/deltaT
C      biharmonic viscosity>viscA4gridmax*L**4/32/deltaT (approx)
C
C     RECOMMENDED VALUES
C     viscC2Leith=1-3
C     viscC2LeithD=1-3
C     viscC4Leith=1-3
C     viscC4LeithD=1.5-3
C     viscC2smag=2.2-4 (Griffies and Hallberg,2000) 
C               0.2-0.9 (Smagorinsky,1993)
C     viscC4smag=2.2-4 (Griffies and Hallberg,2000) 
C     viscAhRemax>=1, (<2 suppresses a computational mode)
C     viscA4Remax>=1, (<2 suppresses a computational mode)
C     viscAhgridmax=1
C     viscA4gridmax=1
C     viscAhgrid<1
C     viscA4grid<1
C     viscAhgridmin<<1
C     viscA4gridmin<<1

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
      LOGICAL harmonic,biharmonic,useVariableViscosity

C     == Local variables ==
      INTEGER I,J
      INTEGER kp1
      _RL smag2fac, smag4fac
      _RL leith2fac, leith4fac
      _RL leithD2fac, leithD4fac
      _RL viscAhRe_max, viscA4Re_max
      _RL Alin,grdVrt,grdDiv, keZpt
      _RL recip_dt,L2,L3,L4,L5,L2rdt,L4rdt
      _RL Uscl,U4scl
      _RL divDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_ZMax(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DMax(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZMax(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DMax(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_ZMin(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DMin(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZMin(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DMin(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_ZLth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DLth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZLth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DLth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_ZLthD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DLthD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZLthD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DLthD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_ZSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL calcLeith,calcSmag

      useVariableViscosity=
     &      (viscAhGrid.NE.0.)
     &  .OR.(viscA4Grid.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)
     &  .OR.(viscC2smag.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

      harmonic=
     &      (viscAh.NE.0.)
     &  .OR.(viscAhD.NE.0.)
     &  .OR.(viscAhZ.NE.0.)
     &  .OR.(viscAhGrid.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC2smag.NE.0.)

      IF ((harmonic).and.(viscAhremax.ne.0.)) THEN
        viscAhre_max=sqrt(2. _d 0)/viscAhRemax
      ELSE
        viscAhre_max=0. _d 0
      ENDIF

      biharmonic=
     &      (viscA4.NE.0.)
     &  .OR.(viscA4D.NE.0.)
     &  .OR.(viscA4Z.NE.0.)
     &  .OR.(viscA4Grid.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

      IF ((biharmonic).and.(viscA4remax.ne.0.)) THEN
        viscA4re_max=0.125 _d 0*sqrt(2. _d 0)/viscA4Remax
      ELSE
        viscA4re_max=0. _d 0
      ENDIF

      calcleith=
     &      (viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)

      calcsmag=
     &      (viscC2smag.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

      IF (deltaTmom.NE.0.) THEN
       recip_dt=1. _d 0/deltaTmom
      ELSE
       recip_dt=0. _d 0
      ENDIF

      IF (calcsmag) THEN
        smag2fac=(viscC2smag/pi)**2
        smag4fac=0.125 _d 0*(viscC4smag/pi)**2
      ELSE
        smag2fac=0. _d 0
        smag4fac=0. _d 0
      ENDIF

      IF (calcleith) THEN
        IF (useFullLeith) THEN
         leith2fac =(viscC2leith /pi)**6
         leithD2fac=(viscC2leithD/pi)**6
         leith4fac =0.015625 _d 0*(viscC4leith /pi)**6
         leithD4fac=0.015625 _d 0*(viscC4leithD/pi)**6
        ELSE
         leith2fac =(viscC2leith /pi)**3
         leithD2fac=(viscC2leithD/pi)**3
         leith4fac =0.125 _d 0*(viscC4leith /pi)**3
         leithD4fac=0.125 _d 0*(viscC4leithD/pi)**3
        ENDIF
      ELSE
        leith2fac=0. _d 0
        leith4fac=0. _d 0
        leithD2fac=0. _d 0
        leithD4fac=0. _d 0
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( calcLeith .OR. calcSmag ) THEN
       STOP 'calcLeith or calcSmag not implemented for ADJOINT'
      ENDIF
#endif
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          viscAh_D(i,j)=viscAhD
          viscAh_Z(i,j)=viscAhZ
          viscA4_D(i,j)=viscA4D
          viscA4_Z(i,j)=viscA4Z
c
          visca4_zsmg(i,j) = 0. _d 0
          viscah_zsmg(i,j) = 0. _d 0
c
          viscAh_Dlth(i,j) = 0. _d 0
          viscA4_Dlth(i,j) = 0. _d 0
          viscAh_DlthD(i,j)= 0. _d 0
          viscA4_DlthD(i,j)= 0. _d 0
c
          viscAh_DSmg(i,j) = 0. _d 0
          viscA4_DSmg(i,j) = 0. _d 0
c
          viscAh_ZLth(i,j) = 0. _d 0
          viscA4_ZLth(i,j) = 0. _d 0
          viscAh_ZLthD(i,j)= 0. _d 0
          viscA4_ZLthD(i,j)= 0. _d 0
        ENDDO
      ENDDO

C     - Viscosity
      IF (useVariableViscosity) THEN

C-     Initialise to zero gradient of vorticity & divergence:
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          divDx(i,j) = 0.
          divDy(i,j) = 0.
          vrtDx(i,j) = 0.
          vrtDy(i,j) = 0.
        ENDDO
       ENDDO

       IF (calcleith) THEN
C      horizontal gradient of horizontal divergence:

C-       gradient in x direction:
#ifndef ALLOW_AUTODIFF_TAMC
         IF (useCubedSphereExchange) THEN
C        to compute d/dx(hDiv), fill corners with appropriate values:
           CALL FILL_CS_CORNER_TR_RL( .TRUE., hDiv, bi,bj, myThid )
         ENDIF
#endif
         DO j=2-Oly,sNy+Oly-1
          DO i=2-Olx,sNx+Olx-1
            divDx(i,j) = (hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj)
          ENDDO
         ENDDO

C-       gradient in y direction:
#ifndef ALLOW_AUTODIFF_TAMC
         IF (useCubedSphereExchange) THEN
C        to compute d/dy(hDiv), fill corners with appropriate values:
           CALL FILL_CS_CORNER_TR_RL(.FALSE., hDiv, bi,bj, myThid )
         ENDIF
#endif
         DO j=2-Oly,sNy+Oly-1
          DO i=2-Olx,sNx+Olx-1
            divDy(i,j) = (hDiv(i,j)-hDiv(i,j-1))*recip_DYC(i,j,bi,bj)
          ENDDO
         ENDDO

C      horizontal gradient of vertical vorticity:
C-       gradient in x direction:
         DO j=2-Oly,sNy+Oly
          DO i=2-Olx,sNx+Olx-1
            vrtDx(i,j) = (vort3(i+1,j)-vort3(i,j))
     &                  *recip_DXG(i,j,bi,bj)
     &                  *maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
C-       gradient in y direction:
         DO j=2-Oly,sNy+Oly-1
          DO i=2-Olx,sNx+Olx
            vrtDy(i,j) = (vort3(i,j+1)-vort3(i,j))
     &                  *recip_DYG(i,j,bi,bj)
     &                  *maskW(i,j,k,bi,bj)
          ENDDO
         ENDDO

       ENDIF

       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
CCCCCCCCCCCCCCC Divergence Point CalculationsCCCCCCCCCCCCCCCCCCCC

C These are (powers of) length scales 
         IF (useAreaViscLength) THEN
          L2=rA(i,j,bi,bj)
         ELSE
          L2=2. _d 0/((recip_DXF(I,J,bi,bj)**2+recip_DYF(I,J,bi,bj)**2))
         ENDIF
         L3=(L2**1.5)
         L4=(L2**2)
         L5=(L2**2.5)

         L2rdt=0.25 _d 0*recip_dt*L2

         IF (useAreaViscLength) THEN
          L4rdt=0.125 _d 0*recip_dt*rA(i,j,bi,bj)**2
         ELSE
          L4rdt=recip_dt/( 6. _d 0*(recip_DXF(I,J,bi,bj)**4
     &                            +recip_DYF(I,J,bi,bj)**4)
     &                   +8. _d 0*((recip_DXF(I,J,bi,bj)
     &                             *recip_DYF(I,J,bi,bj))**2) )
         ENDIF

C Velocity Reynolds Scale
         IF ( viscAhRe_max.GT.0. .AND. KE(i,j).GT.0. ) THEN
           Uscl=sqrt(KE(i,j)*L2)*viscAhRe_max
         ELSE
           Uscl=0.
         ENDIF
         IF ( viscA4Re_max.GT.0. .AND. KE(i,j).GT.0. ) THEN
           U4scl=sqrt(KE(i,j))*L3*viscA4Re_max
         ELSE
           U4scl=0.
         ENDIF

#ifndef ALLOW_AUTODIFF_TAMC
         IF (useFullLeith.and.calcleith) THEN
C This is the vector magnitude of the vorticity gradient squared
          grdVrt=0.25 _d 0*( (vrtDx(i,j+1)*vrtDx(i,j+1)
     &                        + vrtDx(i,j)*vrtDx(i,j) )
     &                     + (vrtDy(i+1,j)*vrtDy(i+1,j)
     &                        + vrtDy(i,j)*vrtDy(i,j) )  )

C This is the vector magnitude of grad (div.v) squared
C Using it in Leith serves to damp instabilities in w.
          grdDiv=0.25 _d 0*( (divDx(i+1,j)*divDx(i+1,j)
     &                        + divDx(i,j)*divDx(i,j) )
     &                     + (divDy(i,j+1)*divDy(i,j+1)
     &                        + divDy(i,j)*divDy(i,j) )  )

          viscAh_DLth(i,j)=
     &     sqrt(leith2fac*grdVrt+leithD2fac*grdDiv)*L3
          viscA4_DLth(i,j)=
     &     sqrt(leith4fac*grdVrt+leithD4fac*grdDiv)*L5
          viscAh_DLthd(i,j)=
     &     sqrt(leithD2fac*grdDiv)*L3
          viscA4_DLthd(i,j)=
     &     sqrt(leithD4fac*grdDiv)*L5
         ELSEIF (calcleith) THEN
C but this approximation will work on cube
c (and differs by as much as 4X)
          grdVrt=max( abs(vrtDx(i,j+1)), abs(vrtDx(i,j)) ) 
          grdVrt=max( grdVrt, abs(vrtDy(i+1,j)) )
          grdVrt=max( grdVrt, abs(vrtDy(i,j))   )

c This approximation is good to the same order as above...
          grdDiv=max( abs(divDx(i+1,j)), abs(divDx(i,j)) ) 
          grdDiv=max( grdDiv, abs(divDy(i,j+1)) )
          grdDiv=max( grdDiv, abs(divDy(i,j))   )

          viscAh_Dlth(i,j)=(leith2fac*grdVrt+(leithD2fac*grdDiv))*L3
          viscA4_Dlth(i,j)=(leith4fac*grdVrt+(leithD4fac*grdDiv))*L5
          viscAh_DlthD(i,j)=((leithD2fac*grdDiv))*L3
          viscA4_DlthD(i,j)=((leithD4fac*grdDiv))*L5
         ELSE
          viscAh_Dlth(i,j)=0. _d 0
          viscA4_Dlth(i,j)=0. _d 0
          viscAh_DlthD(i,j)=0. _d 0
          viscA4_DlthD(i,j)=0. _d 0
         ENDIF

         IF (calcsmag) THEN
          viscAh_DSmg(i,j)=L2
     &       *sqrt(tension(i,j)**2
     &       +0.25 _d 0*(strain(i+1, j )**2+strain( i ,j+1)**2
     &                  +strain(i  , j )**2+strain(i+1,j+1)**2))
          viscA4_DSmg(i,j)=smag4fac*L2*viscAh_DSmg(i,j)
          viscAh_DSmg(i,j)=smag2fac*viscAh_DSmg(i,j)
         ELSE
          viscAh_DSmg(i,j)=0. _d 0
          viscA4_DSmg(i,j)=0. _d 0
         ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C  Harmonic on Div.u points
         Alin=viscAhD+viscAhGrid*L2rdt
     &          +viscAh_DLth(i,j)+viscAh_DSmg(i,j)
         viscAh_DMin(i,j)=max(viscAhGridMin*L2rdt,Uscl)
         viscAh_D(i,j)=max(viscAh_DMin(i,j),Alin)
         viscAh_DMax(i,j)=min(viscAhGridMax*L2rdt,viscAhMax)
         viscAh_D(i,j)=min(viscAh_DMax(i,j),viscAh_D(i,j))

C  BiHarmonic on Div.u points
         Alin=viscA4D+viscA4Grid*L4rdt
     &          +viscA4_DLth(i,j)+viscA4_DSmg(i,j)
         viscA4_DMin(i,j)=max(viscA4GridMin*L4rdt,U4scl)
         viscA4_D(i,j)=max(viscA4_DMin(i,j),Alin)
         viscA4_DMax(i,j)=min(viscA4GridMax*L4rdt,viscA4Max)
         viscA4_D(i,j)=min(viscA4_DMax(i,j),viscA4_D(i,j))

#ifdef ALLOW_NONHYDROSTATIC 
C     /* Pass Viscosities to calc_gw, if constant, not necessary */

         kp1  = MIN(k+1,Nr)

         if (k .eq. 1) then
          viscAh_W(i,j,kp1,bi,bj)=0.5*viscAh_D(i,j)
          viscA4_W(i,j,kp1,bi,bj)=0.5*viscA4_D(i,j)

          viscAh_W(i,j,k,bi,bj)=viscAh_D(i,j) /* These values dont get used */
          viscA4_W(i,j,k,bi,bj)=viscA4_D(i,j)
         else
C Note that previous call of this function has already added half.
          viscAh_W(i,j,kp1,bi,bj)=0.5*viscAh_D(i,j)
          viscA4_W(i,j,kp1,bi,bj)=0.5*viscA4_D(i,j)

          viscAh_W(i,j,k,bi,bj)=viscAh_W(i,j,k,bi,bj)+0.5*viscAh_D(i,j)
          viscA4_W(i,j,k,bi,bj)=viscA4_W(i,j,k,bi,bj)+0.5*viscA4_D(i,j)
         endif
#endif /* ALLOW_NONHYDROSTATIC */

CCCCCCCCCCCCC Vorticity Point CalculationsCCCCCCCCCCCCCCCCCC
C These are (powers of) length scales 
         IF (useAreaViscLength) THEN
          L2=rAz(i,j,bi,bj)
         ELSE
          L2=2. _d 0/((recip_DXV(I,J,bi,bj)**2+recip_DYU(I,J,bi,bj)**2))
         ENDIF

         L3=(L2**1.5)
         L4=(L2**2)
         L5=(L2**2.5)

         L2rdt=0.25 _d 0*recip_dt*L2
         IF (useAreaViscLength) THEN
          L4rdt=0.125 _d 0*recip_dt*rAz(i,j,bi,bj)**2
         ELSE
          L4rdt=recip_dt/
     &     ( 6. _d 0*(recip_DXV(I,J,bi,bj)**4+recip_DYU(I,J,bi,bj)**4)
     &      +8. _d 0*((recip_DXV(I,J,bi,bj)*recip_DYU(I,J,bi,bj))**2))
         ENDIF

C Velocity Reynolds Scale (Pb here at CS-grid corners !)
         IF ( viscAhRe_max.GT.0. .OR. viscA4Re_max.GT.0. ) THEN
           keZpt=0.25 _d 0*( (KE(i,j)+KE(i-1,j-1))
     &                      +(KE(i-1,j)+KE(i,j-1)) )
           IF ( keZpt.GT.0. ) THEN
             Uscl = sqrt(keZpt*L2)*viscAhRe_max
             U4scl= sqrt(keZpt)*L3*viscA4Re_max
           ELSE
             Uscl =0.
             U4scl=0.
           ENDIF
         ELSE
           Uscl =0.
           U4scl=0.
         ENDIF

#ifndef ALLOW_AUTODIFF_TAMC
C This is the vector magnitude of the vorticity gradient squared
         IF (useFullLeith.and.calcleith) THEN
          grdVrt=0.25 _d 0*( (vrtDx(i-1,j)*vrtDx(i-1,j)
     &                        + vrtDx(i,j)*vrtDx(i,j) )
     &                     + (vrtDy(i,j-1)*vrtDy(i,j-1)
     &                        + vrtDy(i,j)*vrtDy(i,j) )  )

C This is the vector magnitude of grad(div.v) squared
          grdDiv=0.25 _d 0*( (divDx(i,j-1)*divDx(i,j-1)
     &                        + divDx(i,j)*divDx(i,j) )
     &                     + (divDy(i-1,j)*divDy(i-1,j)
     &                        + divDy(i,j)*divDy(i,j) )  )

          viscAh_ZLth(i,j)=
     &     sqrt(leith2fac*grdVrt+leithD2fac*grdDiv)*L3
          viscA4_ZLth(i,j)=
     &     sqrt(leith4fac*grdVrt+leithD4fac*grdDiv)*L5
          viscAh_ZLthD(i,j)=
     &     sqrt(leithD2fac*grdDiv)*L3
          viscA4_ZLthD(i,j)=
     &     sqrt(leithD4fac*grdDiv)*L5

         ELSEIF (calcleith) THEN
C but this approximation will work on cube (and differs by 4X)
          grdVrt=max( abs(vrtDx(i-1,j)), abs(vrtDx(i,j)) ) 
          grdVrt=max( grdVrt, abs(vrtDy(i,j-1)) )
          grdVrt=max( grdVrt, abs(vrtDy(i,j))   )

          grdDiv=max( abs(divDx(i,j)), abs(divDx(i,j-1)) ) 
          grdDiv=max( grdDiv, abs(divDy(i,j))   )
          grdDiv=max( grdDiv, abs(divDy(i-1,j)) )

          viscAh_ZLth(i,j)=(leith2fac*grdVrt+(leithD2fac*grdDiv))*L3
          viscA4_ZLth(i,j)=(leith4fac*grdVrt+(leithD4fac*grdDiv))*L5
          viscAh_ZLthD(i,j)=(leithD2fac*grdDiv)*L3
          viscA4_ZLthD(i,j)=(leithD4fac*grdDiv)*L5
         ELSE
          viscAh_ZLth(i,j)=0. _d 0
          viscA4_ZLth(i,j)=0. _d 0
          viscAh_ZLthD(i,j)=0. _d 0
          viscA4_ZLthD(i,j)=0. _d 0
         ENDIF

         IF (calcsmag) THEN
          viscAh_ZSmg(i,j)=L2
     &      *sqrt(strain(i,j)**2
     &        +0.25 _d 0*(tension( i , j )**2+tension( i ,j-1)**2
     &                   +tension(i-1, j )**2+tension(i-1,j-1)**2))
          viscA4_ZSmg(i,j)=smag4fac*L2*viscAh_ZSmg(i,j)
          viscAh_ZSmg(i,j)=smag2fac*viscAh_ZSmg(i,j)
         ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C  Harmonic on Zeta points
         Alin=viscAhZ+viscAhGrid*L2rdt
     &           +viscAh_ZLth(i,j)+viscAh_ZSmg(i,j)
         viscAh_ZMin(i,j)=max(viscAhGridMin*L2rdt,Uscl)
         viscAh_Z(i,j)=max(viscAh_ZMin(i,j),Alin)
         viscAh_ZMax(i,j)=min(viscAhGridMax*L2rdt,viscAhMax)
         viscAh_Z(i,j)=min(viscAh_ZMax(i,j),viscAh_Z(i,j))

C  BiHarmonic on Zeta points
         Alin=viscA4Z+viscA4Grid*L4rdt
     &           +viscA4_ZLth(i,j)+viscA4_ZSmg(i,j)
         viscA4_ZMin(i,j)=max(viscA4GridMin*L4rdt,U4scl)
         viscA4_Z(i,j)=max(viscA4_ZMin(i,j),Alin)
         viscA4_ZMax(i,j)=min(viscA4GridMax*L4rdt,viscA4Max)
         viscA4_Z(i,j)=min(viscA4_ZMax(i,j),viscA4_Z(i,j))
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         viscAh_D(i,j)=viscAhD
         viscAh_Z(i,j)=viscAhZ
         viscA4_D(i,j)=viscA4D
         viscA4_Z(i,j)=viscA4Z
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
       CALL DIAGNOSTICS_FILL(viscAh_D,'VISCAHD ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_D,'VISCA4D ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_Z,'VISCAHZ ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_Z,'VISCA4Z ',k,1,2,bi,bj,myThid)
#ifdef ALLOW_NONHYDROSTATIC
       CALL DIAGNOSTICS_FILL(viscAh_W,'VISCAHW ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_W,'VISCA4W ',k,1,2,bi,bj,myThid)
#endif

       CALL DIAGNOSTICS_FILL(viscAh_DMax,'VAHDMAX ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DMax,'VA4DMAX ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZMax,'VAHZMAX ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZMax,'VA4ZMAX ',k,1,2,bi,bj,myThid)

       CALL DIAGNOSTICS_FILL(viscAh_DMin,'VAHDMIN ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DMin,'VA4DMIN ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZMin,'VAHZMIN ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZMin,'VA4ZMIN ',k,1,2,bi,bj,myThid)

       CALL DIAGNOSTICS_FILL(viscAh_DLth,'VAHDLTH ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DLth,'VA4DLTH ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZLth,'VAHZLTH ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZLth,'VA4ZLTH ',k,1,2,bi,bj,myThid)

       CALL DIAGNOSTICS_FILL(viscAh_DLthD,'VAHDLTHD'
     &   ,k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DLthD,'VA4DLTHD'
     &   ,k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZLthD,'VAHZLTHD'
     &   ,k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZLthD,'VA4ZLTHD'
     &   ,k,1,2,bi,bj,myThid)

       CALL DIAGNOSTICS_FILL(viscAh_DSmg,'VAHDSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DSmg,'VA4DSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZSmg,'VAHZSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZSmg,'VA4ZSMAG',k,1,2,bi,bj,myThid)
      ENDIF
#endif

      RETURN
      END

