#include "MOM_COMMON_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_CALC_VISC

C     !INTERFACE:
      SUBROUTINE MOM_CALC_VISC(
     I        bi,bj,k,
     O        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     I        hDiv,vort3,tension,strain,stretching,KE,hFacZ,
     I        myThid)

C     !DESCRIPTION:
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
C     viscAhReMax is min value for grid point harmonic Reynolds num
C      harmonic viscosity>sqrt(2*KE)*L/viscAhReMax
C
C     viscA4ReMax is min value for grid point biharmonic Reynolds num
C      biharmonic viscosity>sqrt(2*KE)*L**3/8/viscA4ReMax
C
C     viscAhgridmax is CFL stability limiter for harmonic viscosity
C      harmonic viscosity<0.25*viscAhgridmax*L**2/deltaT
C
C     viscA4gridmax is CFL stability limiter for biharmonic viscosity
C      biharmonic viscosity<viscA4gridmax*L**4/32/deltaT (approx)
C
C     viscAhgridmin and viscA4gridmin are lower limits for viscosity:
C       harmonic viscosity>0.25*viscAhgridmin*L**2/deltaT
C       biharmonic viscosity>viscA4gridmin*L**4/32/deltaT (approx)

C     RECOMMENDED VALUES
C     viscC2Leith=1-3
C     viscC2LeithD=1-3
C     viscC2LeithQG=1
C     viscC4Leith=1-3
C     viscC4LeithD=1.5-3
C     viscC2smag=2.2-4 (Griffies and Hallberg,2000)
C               0.2-0.9 (Smagorinsky,1993)
C     viscC4smag=2.2-4 (Griffies and Hallberg,2000)
C     viscAhReMax>=1, (<2 suppresses a computational mode)
C     viscA4ReMax>=1, (<2 suppresses a computational mode)
C     viscAhgridmax=1
C     viscA4gridmax=1
C     viscAhgrid<1
C     viscA4grid<1
C     viscAhgridmin<<1
C     viscA4gridmin<<1

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MOM_VISC.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     myThid               :: my thread Id number
      INTEGER bi,bj,k
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
#ifdef ALLOW_NONHYDROSTATIC
      _RL shiftAh, shiftA4
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey, lockey_1, lockey_2
#endif
      _RL smag2fac, smag4fac
      _RL leith2fac, leith4fac
      _RL leithD2fac, leithD4fac
      _RL leithQG2fac
      _RL viscAhRe_max, viscA4Re_max
      _RL Alin, grdVrt, grdDiv, keZpt
      _RL deepFac3, deepFac4
      _RL L2, L3, L5, L2rdt, L4rdt, recip_dt
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
#ifdef ALLOW_LEITH_QG
      _RL viscAh_ZLthQG(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DLthQG(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sqargQG
#endif
      _RL viscAh_ZSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_DSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_ZSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_DSmg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sqargAh, sqargA4, sqargAhD, sqargA4D, sqargSmag
      LOGICAL calcLeith, calcSmag, calcLeithQG

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ikey = (act1 + 1) + act2*max1
     &                  + act3*max1*max2
     &                  + act4*max1*max2*max3
      lockey_1 = (ikey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set flags which are used in this S/R and elsewhere :
C     useVariableVisc, useHarmonicVisc and useBiharmonicVisc
C     are now set early on (in S/R SET_PARAMS)

c     IF ( useVariableVisc ) THEN
C---- variable viscosity :

       recip_dt = 1. _d 0
       IF ( deltaTMom.NE.0. ) recip_dt = 1. _d 0/deltaTMom
       deepFac3 = deepFac2C(k)*deepFacC(k)
       deepFac4 = deepFac2C(k)*deepFac2C(k)

       IF ( useHarmonicVisc .AND. viscAhReMax.NE.0. ) THEN
        viscAhRe_max=SQRT(2. _d 0)/viscAhReMax
       ELSE
        viscAhRe_max=0. _d 0
       ENDIF

       IF ( useBiharmonicVisc .AND. viscA4ReMax.NE.0. ) THEN
        viscA4Re_max=0.125 _d 0*SQRT(2. _d 0)/viscA4ReMax
       ELSE
        viscA4Re_max=0. _d 0
       ENDIF

       calcLeithQG = (viscC2LeithQG.NE.zeroRL)
       calcLeith=
     &      (viscC2leith.NE.0.)
     &  .OR.(viscC2leithD.NE.0.)
     &  .OR.(viscC4leith.NE.0.)
     &  .OR.(viscC4leithD.NE.0.)
     &  .OR. calcLeithQG

       calcSmag=
     &      (viscC2smag.NE.0.)
     &  .OR.(viscC4smag.NE.0.)

       IF (calcSmag) THEN
        smag2fac=(viscC2smag/pi)**2
        smag4fac=0.125 _d 0*(viscC4smag/pi)**2
       ELSE
        smag2fac=0. _d 0
        smag4fac=0. _d 0
       ENDIF

       IF (calcLeith) THEN
        IF (useFullLeith) THEN
C       Uses correct calculation for gradients, but might not work on cube sphere
         leith2fac =(viscC2leith /pi)**6
         leithD2fac=(viscC2leithD/pi)**6
         leithQG2fac = (viscC2LeithQG/pi)**6
         leith4fac =0.015625 _d 0*(viscC4leith /pi)**6
         leithD4fac=0.015625 _d 0*(viscC4leithD/pi)**6
        ELSE
C       Uses approximate gradients, but works on cube sphere. No reason to use this
C        unless `useFullLeith` fails for your setup.
         leith2fac =(viscC2leith /pi)**3
         leithD2fac=(viscC2leithD/pi)**3
         leithQG2fac = (viscC2LeithQG/pi)**3
         leith4fac =0.125 _d 0*(viscC4leith /pi)**3
         leithD4fac=0.125 _d 0*(viscC4leithD/pi)**3
        ENDIF
       ELSE
        leith2fac=0. _d 0
        leith4fac=0. _d 0
        leithQG2fac=0. _d 0
        leithD2fac=0. _d 0
        leithD4fac=0. _d 0
       ENDIF

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
C-    viscosity arrays have been initialised everywhere before calling this S/R
c         viscAh_D(i,j) = viscAhD
c         viscAh_Z(i,j) = viscAhZ
c         viscA4_D(i,j) = viscA4D
c         viscA4_Z(i,j) = viscA4Z

          viscAh_DLth(i,j) = 0. _d 0
          viscAh_ZLth(i,j) = 0. _d 0
          viscA4_DLth(i,j) = 0. _d 0
          viscA4_ZLth(i,j) = 0. _d 0
          viscAh_DLthD(i,j)= 0. _d 0
          viscAh_ZLthD(i,j)= 0. _d 0
          viscA4_DLthD(i,j)= 0. _d 0
          viscA4_ZLthD(i,j)= 0. _d 0
#ifdef ALLOW_LEITH_QG
          viscAh_DLthQG(i,j) = 0. _d 0
          viscAh_ZLthQG(i,j) = 0. _d 0
#endif

          viscAh_DSmg(i,j) = 0. _d 0
          viscAh_ZSmg(i,j) = 0. _d 0
          viscA4_DSmg(i,j) = 0. _d 0
          viscA4_ZSmg(i,j) = 0. _d 0
        ENDDO
       ENDDO

C-    Initialise to zero gradient of vorticity and divergence:
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          divDx(i,j) = 0.
          divDy(i,j) = 0.
          vrtDx(i,j) = 0.
          vrtDy(i,j) = 0.
        ENDDO
       ENDDO

       IF ( calcLeith ) THEN
C--   horizontal gradient of horizontal divergence:
C-       gradient in x direction:
         IF (useCubedSphereExchange) THEN
C        to compute d/dx(hDiv), fill corners with appropriate values:
           CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                hDiv, bi,bj, myThid )
         ENDIF
         DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1
            divDx(i,j) = (hDiv(i,j)-hDiv(i-1,j))
     &                  *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          ENDDO
         ENDDO

C-       gradient in y direction:
         IF (useCubedSphereExchange) THEN
C        to compute d/dy(hDiv), fill corners with appropriate values:
           CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                hDiv, bi,bj, myThid )
         ENDIF
         DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1
            divDy(i,j) = (hDiv(i,j)-hDiv(i,j-1))
     &                  *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          ENDDO
         ENDDO

C--   horizontal gradient of vertical vorticity:
C-       gradient in x direction:
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx-1
            vrtDx(i,j) = (vort3(i+1,j)-vort3(i,j))
     &                  *recip_dxG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInS(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
C-       gradient in y direction:
         DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx
            vrtDy(i,j) = (vort3(i,j+1)-vort3(i,j))
     &                  *recip_dyG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInW(i,j,bi,bj)
#endif
          ENDDO
         ENDDO

#ifdef ALLOW_LEITH_QG
        IF ( calcLeithQG ) THEN
C      horizontal gradient of vorticity and vortex stretching:
C        In the case of using QG Leith, we want to add a term
C        before calculating vector magnitude, so add to the
C        values just calculated.
C        gradient in x direction:
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx-1
C        Average d/dx of stretching onto V-points to match vrtDX
            vrtDx(i,j) = vrtDx(i,j)
     &                 + halfRL*halfRL*
     &                   ( (stretching(i+1,j)-stretching(i,j))
     &                     *recip_dxC(i+1,j,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i,j)-stretching(i-1,j))
     &                     *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i+1,j-1)-stretching(i,j-1))
     &                     *recip_dxC(i,j-1,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i,j-1)-stretching(i-1,j-1))
     &                     *recip_dxC(i-1,j-1,bi,bj)*recip_deepFacC(k)
     &                   )*maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                    *maskInS(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
C-       gradient in y direction:
         DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx
C        Average d/dy of stretching onto U-points to match vrtDy
            vrtDy(i,j) = vrtDy(i,j)
     &                 + halfRL*halfRL*
     &                   ( (stretching(i,j+1)-stretching(i,j))
     &                     *recip_dyC(i,j+1,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i,j)-stretching(i,j-1))
     &                     *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i-1,j+1)-stretching(i-1,j))
     &                     *recip_dyC(i-1,j+1,bi,bj)*recip_deepFacC(k)
     &                   + (stretching(i-1,j)-stretching(i-1,j-1))
     &                     *recip_dyC(i-1,j,bi,bj)*recip_deepFacC(k)
     &                   )*maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                    *maskInW(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
C      end if calcLeithQG
        ENDIF
#endif /* ALLOW_LEITH_QG */

C--   end if calcLeith
       ENDIF

       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
CCCCCCCCCCCCCCC Divergence Point CalculationsCCCCCCCCCCCCCCCCCCCC

#ifdef ALLOW_AUTODIFF_TAMC
# ifndef AUTODIFF_DISABLE_LEITH
           lockey_2 = i+OLx + (sNx+2*OLx)*(j+OLy-1)
     &                      + (sNx+2*OLx)*(sNy+2*OLy)*(lockey_1-1)
CADJ STORE viscA4_ZSmg(i,j)
CADJ &     = comlev1_mom_ijk_loop , key=lockey_2, byte=isbyte
CADJ STORE viscAh_ZSmg(i,j)
CADJ &     = comlev1_mom_ijk_loop , key=lockey_2, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C These are (powers of) length scales
         L2 = L2_D(i,j,bi,bj)*deepFac2C(k)
         L2rdt = 0.25 _d 0*recip_dt*L2
         L3 = L3_D(i,j,bi,bj)*deepFac3
         L4rdt = L4rdt_D(i,j,bi,bj)*deepFac4
         L5 = (L2*L3)

#ifndef AUTODIFF_DISABLE_REYNOLDS_SCALE
C Velocity Reynolds Scale
         IF ( viscAhRe_max.GT.0. .AND. KE(i,j).GT.0. ) THEN
           Uscl=SQRT(KE(i,j)*L2)*viscAhRe_max
         ELSE
           Uscl=0.
         ENDIF
         IF ( viscA4Re_max.GT.0. .AND. KE(i,j).GT.0. ) THEN
           U4scl=SQRT(KE(i,j))*L3*viscA4Re_max
         ELSE
           U4scl=0.
         ENDIF
#endif /* ndef AUTODIFF_DISABLE_REYNOLDS_SCALE */

#ifndef AUTODIFF_DISABLE_LEITH
         IF (useFullLeith.AND.calcLeith) THEN
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

          sqargAh  = leith2fac*grdVrt+leithD2fac*grdDiv
          sqargA4  = leith4fac*grdVrt+leithD4fac*grdDiv
          sqargAhD = leithD2fac*grdDiv
          sqargA4D = leithD4fac*grdDiv
#ifdef ALLOW_LEITH_QG
          sqargQG  = leithQG2fac*(grdVrt+grdDiv)
#endif

#ifdef ALLOW_AUTODIFF
C Avoid derivative of SQRT(0)
          IF (sqargAh .GT.0. _d 0) sqargAh = SQRT(sqargAh)
          IF (sqargA4 .GT.0. _d 0) sqargA4 = SQRT(sqargA4)
          IF (sqargAhD .GT.0. _d 0) sqargAhD = SQRT(sqargAhD)
          IF (sqargA4D .GT.0. _d 0) sqargA4D = SQRT(sqargA4D)
# ifdef ALLOW_LEITH_QG
          IF (sqargQG .GT.0. _d 0) sqargQG = SQRT(sqargQG)
# endif
#else /* ALLOW_AUTODIFF */
          sqargAh = SQRT(sqargAh)
          sqargA4 = SQRT(sqargA4)
          sqargAhD = SQRT(sqargAhD)
          sqargA4D = SQRT(sqargA4D)
# ifdef ALLOW_LEITH_QG
          sqargQG = SQRT(sqargQG)
# endif
#endif /* ALLOW_AUTODIFF */
          viscAh_DLth(i,j) = sqargAh  * L3
          viscA4_DLth(i,j) = sqargA4  * L5
          viscAh_DLthd(i,j)= sqargAhD * L3
          viscA4_DLthd(i,j)= sqargA4D * L5
#ifdef ALLOW_LEITH_QG
          viscAh_DLthQG(i,j)=sqargQG  * L3
#endif

         ELSEIF (calcLeith) THEN
C but this approximation will work on cube (and differs by as much as 4X)
          grdVrt=MAX( ABS(vrtDx(i,j+1)), ABS(vrtDx(i,j)) )
          grdVrt=MAX( grdVrt, ABS(vrtDy(i+1,j)) )
          grdVrt=MAX( grdVrt, ABS(vrtDy(i,j))   )

C This approximation is good to the same order as above...
          grdDiv=MAX( ABS(divDx(i+1,j)), ABS(divDx(i,j)) )
          grdDiv=MAX( grdDiv, ABS(divDy(i,j+1)) )
          grdDiv=MAX( grdDiv, ABS(divDy(i,j))   )

          viscAh_DLth(i,j)=(leith2fac*grdVrt+(leithD2fac*grdDiv))*L3
          viscA4_DLth(i,j)=(leith4fac*grdVrt+(leithD4fac*grdDiv))*L5
          viscAh_DLthD(i,j)=((leithD2fac*grdDiv))*L3
          viscA4_DLthD(i,j)=((leithD4fac*grdDiv))*L5
#ifdef ALLOW_LEITH_QG
          viscAh_DLthQG(i,j)=leithQG2fac*(grdVrt + grdDiv)*L3
#endif

         ELSE
          viscAh_DLth(i,j)=0. _d 0
          viscA4_DLth(i,j)=0. _d 0
          viscAh_DLthD(i,j)=0. _d 0
          viscA4_DLthD(i,j)=0. _d 0
#ifdef ALLOW_LEITH_QG
          viscAh_DLthQG(i,j)=0. _d 0
#endif
         ENDIF

         IF (calcSmag) THEN
          sqargSmag = tension(i,j)**2
     &       +0.25 _d 0*(strain(i+1, j )**2+strain( i ,j+1)**2
     &                  +strain(i  , j )**2+strain(i+1,j+1)**2)
#ifdef ALLOW_AUTODIFF
C Avoid derivative of SQRT(0)
          IF (sqargSmag.GT.0. _d 0) sqargSmag = SQRT(sqargSmag)
#else
          sqargSmag = SQRT(sqargSmag)
#endif
          viscAh_DSmg(i,j)=L2*sqargSmag
          viscA4_DSmg(i,j)=smag4fac*L2*viscAh_DSmg(i,j)
          viscAh_DSmg(i,j)=smag2fac*viscAh_DSmg(i,j)
         ELSE
          viscAh_DSmg(i,j)=0. _d 0
          viscA4_DSmg(i,j)=0. _d 0
         ENDIF
#endif /* AUTODIFF_DISABLE_LEITH */

C  Harmonic on Div.u points
         Alin=viscAhD+viscAhGrid*L2rdt
     &         + viscAh_DLth(i,j)+viscAh_DSmg(i,j)
#ifdef ALLOW_LEITH_QG
     &         + viscAh_DLthQG(i,j)
#endif
#ifdef ALLOW_3D_VISCAH
     &         + viscAhDfld(i,j,k,bi,bj)
# ifdef AUTODIFF_ALLOW_VISCFACADJ
     &          *viscFacAdj
# endif /* AUTODIFF_ALLOW_VISCFACADJ */
#endif /* ALLOW_3D_VISCAH */
         viscAh_DMin(i,j)=MAX(viscAhGridMin*L2rdt,Uscl)
         viscAh_D(i,j)=MAX(viscAh_DMin(i,j),Alin)
         viscAh_DMax(i,j)=MIN(viscAhGridMax*L2rdt,viscAhMax)
         viscAh_D(i,j)=MIN(viscAh_DMax(i,j),viscAh_D(i,j))

C  BiHarmonic on Div.u points
         Alin=viscA4D+viscA4Grid*L4rdt
     &         + viscA4_DLth(i,j)+viscA4_DSmg(i,j)
#ifdef ALLOW_3D_VISCA4
     &         + viscA4Dfld(i,j,k,bi,bj)
# ifdef AUTODIFF_ALLOW_VISCFACADJ
     &          *viscFacAdj
# endif /* AUTODIFF_ALLOW_VISCFACADJ */
#endif /* ALLOW_3D_VISCA4 */
         viscA4_DMin(i,j)=MAX(viscA4GridMin*L4rdt,U4scl)
         viscA4_D(i,j)=MAX(viscA4_DMin(i,j),Alin)
         viscA4_DMax(i,j)=MIN(viscA4GridMax*L4rdt,viscA4Max)
         viscA4_D(i,j)=MIN(viscA4_DMax(i,j),viscA4_D(i,j))

CCCCCCCCCCCCC Vorticity Point CalculationsCCCCCCCCCCCCCCCCCC
C These are (powers of) length scales
         L2 = L2_Z(i,j,bi,bj)*deepFac2C(k)
         L2rdt = 0.25 _d 0*recip_dt*L2
         L3 = L3_Z(i,j,bi,bj)*deepFac3
         L4rdt = L4rdt_Z(i,j,bi,bj)*deepFac4
         L5 = (L2*L3)

#ifndef AUTODIFF_DISABLE_REYNOLDS_SCALE
C Velocity Reynolds Scale (Pb here at CS-grid corners !)
         IF ( viscAhRe_max.GT.0. .OR. viscA4Re_max.GT.0. ) THEN
           keZpt=0.25 _d 0*( (KE(i,j)+KE(i-1,j-1))
     &                      +(KE(i-1,j)+KE(i,j-1)) )
           IF ( keZpt.GT.0. ) THEN
             Uscl = SQRT(keZpt*L2)*viscAhRe_max
             U4scl= SQRT(keZpt)*L3*viscA4Re_max
           ELSE
             Uscl =0.
             U4scl=0.
           ENDIF
         ELSE
           Uscl =0.
           U4scl=0.
         ENDIF
#endif /* ndef AUTODIFF_DISABLE_REYNOLDS_SCALE */

#ifndef AUTODIFF_DISABLE_LEITH
C This is the vector magnitude of the vorticity gradient squared
         IF (useFullLeith.AND.calcLeith) THEN
          grdVrt=0.25 _d 0*( (vrtDx(i-1,j)*vrtDx(i-1,j)
     &                        + vrtDx(i,j)*vrtDx(i,j) )
     &                     + (vrtDy(i,j-1)*vrtDy(i,j-1)
     &                        + vrtDy(i,j)*vrtDy(i,j) )  )

C This is the vector magnitude of grad(div.v) squared
          grdDiv=0.25 _d 0*( (divDx(i,j-1)*divDx(i,j-1)
     &                        + divDx(i,j)*divDx(i,j) )
     &                     + (divDy(i-1,j)*divDy(i-1,j)
     &                        + divDy(i,j)*divDy(i,j) )  )

          sqargAh  = leith2fac*grdVrt+leithD2fac*grdDiv
          sqargA4  = leith4fac*grdVrt+leithD4fac*grdDiv
          sqargAhD = leithD2fac*grdDiv
          sqargA4D = leithD4fac*grdDiv
#ifdef ALLOW_LEITH_QG
          sqargQG  = leithQG2fac*(grdVrt+grdDiv)
#endif
#ifdef ALLOW_AUTODIFF
C Avoid derivative of SQRT(0)
          IF (sqargAh .GT.0. _d 0) sqargAh = SQRT(sqargAh)
          IF (sqargA4 .GT.0. _d 0) sqargA4 = SQRT(sqargA4)
          IF (sqargAhD .GT.0. _d 0) sqargAhD = SQRT(sqargAhD)
          IF (sqargA4D .GT.0. _d 0) sqargA4D = SQRT(sqargA4D)
# ifdef ALLOW_LEITH_QG
          IF (sqargQG .GT.0. _d 0) sqargQG = SQRT(sqargQG)
# endif
#else /* ALLOW_AUTODIFF */
          sqargAh = SQRT(sqargAh)
          sqargA4 = SQRT(sqargA4)
          sqargAhD = SQRT(sqargAhD)
          sqargA4D = SQRT(sqargA4D)
# ifdef ALLOW_LEITH_QG
          sqargQG = SQRT(sqargQG)
# endif
#endif /* ALLOW_AUTODIFF */
          viscAh_ZLth(i,j) = sqargAh  * L3
          viscA4_ZLth(i,j) = sqargA4  * L5
          viscAh_ZLthd(i,j)= sqargAhD * L3
          viscA4_ZLthd(i,j)= sqargA4D * L5
#ifdef ALLOW_LEITH_QG
          viscAh_ZLthQG(i,j)=sqargQG  * L3
#endif

         ELSEIF (calcLeith) THEN
C but this approximation will work on cube (and differs by 4X)
          grdVrt=MAX( ABS(vrtDx(i-1,j)), ABS(vrtDx(i,j)) )
          grdVrt=MAX( grdVrt, ABS(vrtDy(i,j-1)) )
          grdVrt=MAX( grdVrt, ABS(vrtDy(i,j))   )

          grdDiv=MAX( ABS(divDx(i,j)), ABS(divDx(i,j-1)) )
          grdDiv=MAX( grdDiv, ABS(divDy(i,j))   )
          grdDiv=MAX( grdDiv, ABS(divDy(i-1,j)) )

          viscAh_ZLth(i,j)=(leith2fac*grdVrt+(leithD2fac*grdDiv))*L3
          viscA4_ZLth(i,j)=(leith4fac*grdVrt+(leithD4fac*grdDiv))*L5
          viscAh_ZLthD(i,j)=(leithD2fac*grdDiv)*L3
          viscA4_ZLthD(i,j)=(leithD4fac*grdDiv)*L5
#ifdef ALLOW_LEITH_QG
          viscAh_ZLthQG(i,j)=leithQG2fac*(grdVrt + grdDiv)*L3
#endif
         ELSE
          viscAh_ZLth(i,j)=0. _d 0
          viscA4_ZLth(i,j)=0. _d 0
          viscAh_ZLthD(i,j)=0. _d 0
          viscA4_ZLthD(i,j)=0. _d 0
#ifdef ALLOW_LEITH_QG
          viscAh_ZLthQG(i,j)=0. _d 0
#endif
         ENDIF

         IF (calcSmag) THEN
          sqargSmag = strain(i,j)**2
     &        +0.25 _d 0*(tension( i , j )**2+tension( i ,j-1)**2
     &                   +tension(i-1, j )**2+tension(i-1,j-1)**2)
#ifdef ALLOW_AUTODIFF
C Avoid derivative of SQRT(0)
          IF (sqargSmag.GT.0. _d 0) sqargSmag = SQRT(sqargSmag)
#else
          sqargSmag = SQRT(sqargSmag)
#endif
          viscAh_ZSmg(i,j)=L2*sqargSmag
          viscA4_ZSmg(i,j)=smag4fac*L2*viscAh_ZSmg(i,j)
          viscAh_ZSmg(i,j)=smag2fac*viscAh_ZSmg(i,j)
         ENDIF
#endif /* AUTODIFF_DISABLE_LEITH */

C  Harmonic on Zeta points
         Alin=viscAhZ+viscAhGrid*L2rdt
     &         + viscAh_ZLth(i,j)+viscAh_ZSmg(i,j)
#ifdef ALLOW_LEITH_QG
     &         + viscAh_ZLthQG(i,j)
#endif
#ifdef ALLOW_3D_VISCAH
     &         + viscAhZfld(i,j,k,bi,bj)
# ifdef AUTODIFF_ALLOW_VISCFACADJ
     &          *viscFacAdj
# endif /* AUTODIFF_ALLOW_VISCFACADJ */
#endif
         viscAh_ZMin(i,j)=MAX(viscAhGridMin*L2rdt,Uscl)
         viscAh_Z(i,j)=MAX(viscAh_ZMin(i,j),Alin)
         viscAh_ZMax(i,j)=MIN(viscAhGridMax*L2rdt,viscAhMax)
         viscAh_Z(i,j)=MIN(viscAh_ZMax(i,j),viscAh_Z(i,j))

C  BiHarmonic on Zeta points
         Alin=viscA4Z+viscA4Grid*L4rdt
     &         + viscA4_ZLth(i,j)+viscA4_ZSmg(i,j)
#ifdef ALLOW_3D_VISCA4
     &         + viscA4Zfld(i,j,k,bi,bj)
# ifdef AUTODIFF_ALLOW_VISCFACADJ
     &          *viscFacAdj
# endif /* AUTODIFF_ALLOW_VISCFACADJ */
#endif
         viscA4_ZMin(i,j)=MAX(viscA4GridMin*L4rdt,U4scl)
         viscA4_Z(i,j)=MAX(viscA4_ZMin(i,j),Alin)
         viscA4_ZMax(i,j)=MIN(viscA4GridMax*L4rdt,viscA4Max)
         viscA4_Z(i,j)=MIN(viscA4_ZMax(i,j),viscA4_Z(i,j))
        ENDDO
       ENDDO

#ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
C--   Pass Viscosities to calc_gw (if constant, not necessary)

        IF ( k.LT.Nr ) THEN
C     Prepare for next level (next call)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            viscAh_W(i,j,k+1,bi,bj) = halfRL*viscAh_D(i,j)
            viscA4_W(i,j,k+1,bi,bj) = halfRL*viscA4_D(i,j)
          ENDDO
         ENDDO
        ENDIF

        shiftAh = viscAhW - viscAhD
        shiftA4 = viscA4W - viscA4D
        IF ( k.EQ.1 ) THEN
C     These values dont get used
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            viscAh_W(i,j,k,bi,bj) = shiftAh + viscAh_D(i,j)
            viscA4_W(i,j,k,bi,bj) = shiftA4 + viscA4_D(i,j)
          ENDDO
         ENDDO
        ELSE
C     Note that previous call of this function has already added half.
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            viscAh_W(i,j,k,bi,bj) = shiftAh + viscAh_W(i,j,k,bi,bj)
     &                                      + halfRL*viscAh_D(i,j)
            viscA4_W(i,j,k,bi,bj) = shiftA4 + viscA4_W(i,j,k,bi,bj)
     &                                      + halfRL*viscA4_D(i,j)
          ENDDO
         ENDDO
        ENDIF

       ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

c     ELSE
C---- use constant viscosity (useVariableVisc=F):
c      DO j=1-OLy,sNy+OLy
c       DO i=1-OLx,sNx+OLx
c        viscAh_D(i,j) = viscAhD
c        viscAh_Z(i,j) = viscAhZ
c        viscA4_D(i,j) = viscA4D
c        viscA4_Z(i,j) = viscA4Z
c       ENDDO
c      ENDDO
C---- variable/constant viscosity : end if/else block
c     ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
       CALL DIAGNOSTICS_FILL(viscAh_D,'VISCAHD ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_D,'VISCA4D ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_Z,'VISCAHZ ',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_Z,'VISCA4Z ',k,1,2,bi,bj,myThid)

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

       CALL DIAGNOSTICS_FILL(viscAh_DLthD,'VAHDLTHD',
     &                       k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DLthD,'VA4DLTHD',
     &                       k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZLthD,'VAHZLTHD',
     &                       k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZLthD,'VA4ZLTHD',
     &                       k,1,2,bi,bj,myThid)
#ifdef ALLOW_LEITH_QG
       CALL DIAGNOSTICS_FILL(viscAh_DLthQG,'VAHDLTHQ',
     &                       k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZLthQG,'VAHZLTHQ',
     &                       k,1,2,bi,bj,myThid)
#endif
       CALL DIAGNOSTICS_FILL(viscAh_DSmg,'VAHDSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_DSmg,'VA4DSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscAh_ZSmg,'VAHZSMAG',k,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(viscA4_ZSmg,'VA4ZSMAG',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
