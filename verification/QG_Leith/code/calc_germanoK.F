      SUBROUTINE CALC_GERMANOK(
     I        bi,bj,uFld,vFld,k,
     I        thetaG,saltG, GermanoK_layer)
      IMPLICIT NONE
C
C     This is going to be Germano (1991) diffusivity. I think this
C     only needs to be calculated at divergence points. I am going
C     to calculate strain, tension, vort4, KE(?), and hFacZ in
C     this code as part of the Germano setup.


C     == Global variables ==
#include "CPP_OPTIONS.h"
#include "MOM_VISC.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL GermanoK_layer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LAMBDA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL GermanoKh(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL GermanoK_save(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL thetaG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL saltG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

    
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL recip_dt
      _RL grdVrt,grdDiv
      _RL L2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL L3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FL3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL U_scale(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Ro_g2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Bu_g(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching_hold(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == "Filter" variables ==

      _RL f_eps

      _RL V(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL u1u1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL u1u2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL u2u2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL S11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL S12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL S22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VS11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VS12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VS22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL L_11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL L_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL L_22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL M_11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL M_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL M_22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL FuFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FvFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fu1u1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fu1u2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fu2u2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FS11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FS12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FS22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FVS11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FVS12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FVS22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


      _RL leith_qg_fac  

      leith_qg_fac = (leith_qg_coeff / PI)**3.0



C       ------------------------------------------------------------
C       ----       SET DEFAULTS OF ALL LOCAL FIELDS TO 0        ----
C       ------------------------------------------------------------


       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx      

         V(i,j) = 0.
         u1u1(i,j) = 0.
         u1u2(i,j) = 0.
         u2u2(i,j) = 0.
         S11(i,j) = 0.
         S12(i,j) = 0.
         S22(i,j) = 0.
         VS11(i,j) = 0.
         VS12(i,j) = 0.
         VS22(i,j) = 0.
         Nsq(i,j) = 0.

        ENDDO
       ENDDO


C       ------------------------------------------------------------
C       ----       CALCULATE VORTEX STRETCHING TERM             ----
C       ------------------------------------------------------------



        CALL MOM_CALC_QG_STRETCHING(bi,bj,k,thetaG,saltG,
     &    stretching,Nsq,myThid)



C       ------------------------------------------------------------
C       ----                      HDIV                          ----
C       ------------------------------------------------------------
C       It seems like we always use hDivScheme=2, so this is the one
C       I am using in this code.
       DO J=1-Oly,sNy+Oly-1
        DO I=1-Olx,sNx+Olx-1
C       This discretization takes into account the fractional areas
C       due to the lopping. Whether we should do this is not clear!
         hDiv(I,J)=
     &    ( ( uFld(i+1, j )*dyg(i+1, j ,bi,bj)*hFacW(i+1, j ,K,bi,bj)
     &       -uFld( i , j )*dyg( i , j ,bi,bj)*hFacW( i , j ,K,bi,bj) )
     &     +( vFld( i ,j+1)*dxg( i ,j+1,bi,bj)*hFacS( i ,j+1,K,bi,bj)
     &       -vFld( i , j )*dxg( i , j ,bi,bj)*hFacS( i , j ,K,bi,bj) )
     &    )*recip_rA(I,J,bi,bj)
     &     *_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
 

C       ------------------------------------------------------------
C       ----                   VORT3                            ----
C       ------------------------------------------------------------
C      Relative vorticity is calculated at vorticity points.
       DO J=2-Oly,sNy+Oly
        DO I=2-Olx,sNx+Olx
         vort3(I,J)=
     &      recip_rAz(I,J,bi,bj)*(
     &      ( vFld(I,J)*dyC(I,J,bi,bj)
     &       -vFld(I-1,J)*dyC(I-1,J,bi,bj) )
     &     -( uFld(I,J)*dxC(I,J,bi,bj)
     &       -uFld(I,J-1)*dxC(I,J-1,bi,bj) )
     &                           )

        ENDDO
       ENDDO


C Here I will put a cap on the stretching term.

       DO J=2-OLy,sNy+OLy-1
        DO I=2-OLx,sNx+OLx-1
         U_scale(i,j) = 0.5* SQRT(
     &           ( uFld( i , j )*uFld( i , j )
     &            +uFld(i+1, j )*uFld(i+1, j ) )
     &         + ( vFld( i , j )*vFld( i , j )
     &            +vFld( i ,j+1)*vFld( i ,j+1) )
     &                  )

         Ro_g2(i,j) = U_scale(i,j)**2 * recip_rA(i,j,bi,bj) / (f0**2)
         Bu_g(i,j) = Nsq(i,j) * drf(k) /
     &    ((f0**2)* SQRT(recip_rA(i,j,bi,bj)) )

         stretching_hold(i,j) = MIN( ABS(stretching(i,j)),
     &    ABS(vort3(i,j) / MAX(Bu_g(i,j),Ro_g2(i,j)) ) )

         stretching(i,j) = SIGN(stretching_hold(i,j), stretching(i,j))

        ENDDO
       ENDDO





C       ------------------------------------------------------------
C       ----               STRAIN RATE                          ----
C       ------------------------------------------------------------

       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
C       This discretization takes into account the fractional areas
C       due to the lopping. Whether we should do this is not clear!
         S11(i,j)=
     &     ( uFld(i+1, j )*dyg(i+1, j ,bi,bj)*hFacW(i+1, j ,K,bi,bj)
     &       -uFld( i , j )*dyg( i , j ,bi,bj)*hFacW( i , j ,K,bi,bj) )
     &    *recip_rA(I,J,bi,bj)
     &     *_recip_hFacC(i,j,k,bi,bj)
         S22(i,j)=
     &     ( vFld( i ,j+1)*dxg( i ,j+1,bi,bj)*hFacS( i ,j+1,K,bi,bj)
     &       -vFld( i , j )*dxg( i , j ,bi,bj)*hFacS( i , j ,K,bi,bj) )
     &     *recip_rA(I,J,bi,bj)
     &     *_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO

       DO J=2-Oly,sNy+Oly
        DO I=2-Olx,sNx+Olx
         S12(I,J)= 0.5 _d 0 *
     &      recip_rAz(I,J,bi,bj)*(
     &      ( vFld(I,J)*dyC(I,J,bi,bj)
     &       -vFld(I-1,J)*dyC(I-1,J,bi,bj) )
     &     +( uFld(I,J)*dxC(I,J,bi,bj)
     &       -uFld(I,J-1)*dxC(I,J-1,bi,bj) )
     &                           )
        ENDDO
       ENDDO


C       ------------------------------------------------------------
C       ----        GRADIENTS OF VORTICITY AND DIVERGENCE       ----
C       ------------------------------------------------------------
C-     Initialise to zero gradient of vorticity & divergence:
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          divDx(i,j) = 0.
          divDy(i,j) = 0.
          vrtDx(i,j) = 0.
          vrtDy(i,j) = 0.
        ENDDO
       ENDDO

C      horizontal gradient of horizontal divergence:
C-       gradient in x direction:
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
          divDx(i,j) = (hDiv(i,j)-hDiv(i-1,j))*recip_DXC(i,j,bi,bj)
        ENDDO
       ENDDO

C-       gradient in y direction:
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
     &         + (stretching(i-1,j)-stretching(i+1,j))
     &                  *0.5*recip_DXG(i,j,bi,bj)
     &                  *maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
C-       gradient in y direction:
         DO j=2-Oly,sNy+Oly-1
          DO i=2-Olx,sNx+Olx
            vrtDy(i,j) = (vort3(i,j+1)-vort3(i,j))
     &                  *recip_DYG(i,j,bi,bj)
     &                  *maskW(i,j,k,bi,bj)
     &         + (stretching(i,j+1)-stretching(i,j))
     &                  *0.5*recip_DYG(i,j,bi,bj)
     &                  *maskW(i,j,k,bi,bj)
          ENDDO
         ENDDO

C       ------------------------------------------------------------
C       ----                   NOW CALCULATE V                  ----
C       ------------------------------------------------------------

       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

C I am going try modified Leith form for starters

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

C          grdDiv=0.25 _d 0*( (divDx(i-1,j)*divDx(i-1,j)
C     &                        + divDx(i,j)*divDx(i,j) )
C     &                     + (divDy(i,j-1)*divDy(i,j-1)
C     &                        + divDy(i,j)*divDy(i,j) )  )

          V(i,j)=
     &     SQRT(grdVrt+grdDiv)

        ENDDO
       ENDDO


       DO i=2-Olx,sNx+Olx-1
          V(i,sNy) = V(i,1)
       ENDDO


CC V has problems on the right boundary due to the horizontal gradient of the 
CC stretching term.  I will temporarily fix this by setting the right boundary value
CC of V equal to the left boundary value of V


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C               NOW DECIDE IF WE ARE USING DYNAMICAL LEITH OR NOT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       IF (dyn_leith_qg) THEN

C       ------------------------------------------------------------
C       ----                  LEONARD TENSOR 1                  ----
C       ------------------------------------------------------------


C This basically is u_i times the tracer
C Make sure to interpolate u and v to the divergence points!

       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
        
         u1u1(i,j) = 0.25 _d 0 * (uFld(i,j)+uFld(i+1,j))**2. 
         u1u2(i,j) = 0.25 _d 0 * (vFld(i,j)+vFld(i,j+1)) *
     &      (uFld(i,j)+uFld(i+1,j))
         u2u2(i,j) = 0.25 _d 0 * (vFld(i,j)+vFld(i,j+1))**2.

        ENDDO
       ENDDO


C       ------------------------------------------------------------
C       ----                V * STRAIN RATE                     ----
C       ------------------------------------------------------------


       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

         VS11(i,j)= V(i,j) * S11(i,j)
         VS12(i,j)= V(i,j) * S12(i,j)
         VS22(i,j) = V(i,j) * S22(i,j)

        ENDDO
       ENDDO



C       ------------------------------------------------------------
C       --   COPY THE ABOVE FIELDS INTO THEIR "FILTER" VERSIONS   --
C       ------------------------------------------------------------
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
        
         FuFld(i,j) = 0.5 _d 0 * (uFld(i,j)+uFld(i+1,j)) 
         FvFld(i,j) = 0.5 _d 0 * (vFld(i,j)+vFld(i,j+1))
         FV(i,j) = V(i,j)
         Fu1u1(i,j) = u1u1(i,j)
         Fu1u2(i,j) = u1u2(i,j)
         Fu2u2(i,j) = u2u2(i,j)
         FS11(i,j)=S11(i,j)
         FS12(i,j)=S12(i,j)
         FS22(i,j)=S22(i,j)
         FVS11(i,j)=VS11(i,j)
         FVS12(i,j)=VS12(i,j)
         FVS22(i,j)=VS22(i,j)
        ENDDO
       ENDDO
  

C       ------------------------------------------------------------
C       ----       PASS THE "FILTER FIELDS" ON AND FFT          ----
C       ------------------------------------------------------------

C I have calculated every field necessary to do this, so now I will
C call the top-hat filter code and do the test filter.

C The effective filter width of the convolution of two top hat
C filters is sqrt(5) times the grid spacing. FL3 needs to be
C multiplied by the filter width cubed, so 5^(3/2) 

 
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         L2(i,j) = rA(i,j,bi,bj)
         L3(i,j) = rA(i,j,bi,bj)**1.5 _d 0
         FL3(i,j) = L3(i,j) * (SQRT(6.0)**3.0)
        ENDDO
       ENDDO

       f_eps = filter_width


       CALL CALC_GERMANO_DISC(FuFld,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FvFld,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FV,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(Fu1u1,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(Fu1u2,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(Fu2u2,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FS11,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FS12,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FS22,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FVS11,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FVS12,bi,bj,k,f_eps,myThid)
       CALL CALC_GERMANO_DISC(FVS22,bi,bj,k,f_eps,myThid)

C       ------------------------------------------------------------
C       ----                  CONSTRUCT L AND M                 ----
C       ------------------------------------------------------------

C 

       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx

         L_11(i,j) = Fu1u1(i,j) - (FuFld(i,j) * FuFld(i,j))
         L_12(i,j) = Fu1u2(i,j) - (FuFld(i,j) * FvFld(i,j))
         L_22(i,j) = Fu2u2(i,j) - (FvFld(i,j) * FvFld(i,j))

         M_11(i,j) = (FL3(i,j) * FV(i,j) * FS11(i,j)) -
     &           (L3(i,j) * FVS11(i,j))
         M_12(i,j) = (FL3(i,j) * FV(i,j) * FS12(i,j)) -
     &           (L3(i,j) * FVS12(i,j))
         M_22(i,j) = (FL3(i,j) * FV(i,j) * FS22(i,j)) -
     &           (L3(i,j) * FVS22(i,j))
        ENDDO
       ENDDO

C       ------------------------------------------------------------
C       ----                CONSTRUCT GERMANOK                  ----
C       ------------------------------------------------------------

C Here is the field that will hold all the \kappa's that are 
C calculated in this code.

C Initialize it to zero just to be safe (no screw-ups)

       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx

         GermanoK_layer(i,j)= 0.
         GermanoKh(i,j) = 0.   

        ENDDO
       ENDDO

C Now calculate it for real. I am not going to calculate the very
C edges of the box, because I think I'll get a division by zero error
C there. It shouldn't matter anyways, since that is the overlap
C region and is there only for derivatives.

       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

         LAMBDA(i,j) = PI*(((L_11(i,j)*M_11(i,j)) +
     &            2*(L_12(i,j)* M_12(i,j)) + (L_22(i,j)* M_22(i,j)))
     &         / ((M_11(i,j)*M_11(i,j))+ 2*(M_12(i,j)*M_12(i,j))+
     &                    (M_22(i,j)*M_22(i,j))))**0.33


         GermanoK_layer(i,j)= (((L_11(i,j)*M_11(i,j)) +
     &            2*(L_12(i,j)* M_12(i,j)) + (L_22(i,j)* M_22(i,j))) 
     &         / ((M_11(i,j)*M_11(i,j))+ 2*(M_12(i,j)*M_12(i,j))+
     &                    (M_22(i,j)*M_22(i,j)))) * L3(i,j) * V(i,j)

        ENDDO
       ENDDO


C The locally defined Germano scheme (above) is unstable!  It is known
C to give negative viscosities (diffusivities). A crude way to get by
C this limitation is to assume homogeneous turbulence in at least one
C direction and do a spatial averaging. I will try this first by 
C assuming that it is homogeneous in both x and y and I'll do an
C averaging over a 3x3 box (arbitrary choice). 


       DO j=1,sNy
        DO i=1,sNx
         GermanoKh(i,j)= (GermanoK_layer(i+1,j) +
     &    GermanoK_layer(i-1,j) + GermanoK_layer(i,j) + 
     &    GermanoK_layer(i,j-1) + GermanoK_layer(i,j+1)) / 5.0 _d 0
        ENDDO
       ENDDO

      

       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
C clip to zero if it is negative
         GermanoK_layer(i,j) = MAX(GermanoKh(i,j), 0. _d 0)

C Make sure it doesn't violate stability condition
C I'm not sure exactly what the stability criteria are, but from what I
C gather it goes something like dt < dx^2 / (c * kappa), where c is a
C constant depending on the numerical scheme (I've seen it equal to 16 somewhere)
C 16 is the strictest condition I could find, so I'll go with that
         GermanoK_layer(i,j) = MIN(GermanoK_layer(i,j),
     &    L2(i,j) / (16.0 _d 0 * deltaT))          
        ENDDO
       ENDDO

       ENDIF
C END OF DYNAMICAL LEITH LOOP


       IF (leith_qg) THEN

       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         L3(i,j) = rA(i,j,bi,bj)**1.5 _d 0
         L2(i,j) = rA(i,j,bi,bj)
         GermanoK_layer(i,j) = leith_qg_fac * L3(i,j) * V(i,j)
        ENDDO
       ENDDO

C Smooth the field

       DO j=1,sNy
        DO i=1,sNx
         GermanoKh(i,j)= (GermanoK_layer(i+1,j) +
     &    GermanoK_layer(i-1,j) + GermanoK_layer(i,j) +
     &    GermanoK_layer(i,j-1) + GermanoK_layer(i,j+1)) / 5.0 _d 0
        ENDDO
       ENDDO


       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         GermanoK_layer(i,j) = GermanoKh(i,j)
         GermanoK_layer(i,j) = MIN(GermanoK_layer(i,j),
     &    L2(i,j) / (16.0 * deltaT))
        ENDDO
       ENDDO


       ENDIF

C END OF NON-DYNAMICAL LEITH LOOP



#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
C----  For Germano, now add some user diagnostics
C       CALL DIAGNOSTICS_FILL(GermanoK_layer,'UDIAG1  ',k,1,2,
C     &    bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(u1u1,
C     &   'UDIAG2  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(FV,'UDIAG3  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(FuFld,'UDIAG4  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(Fuc,'UDIAG5  ',k,1,2,bi,bj,myThid)
C      
C       CALL DIAGNOSTICS_FILL(L_1,'UDIAG6  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(L_2,'UDIAG7  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(M_1,'UDIAG8  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(M_2,'UDIAG9  ',k,1,2,bi,bj,myThid)
C       CALL DIAGNOSTICS_FILL(FTracer,'UDIAG9  ',k,1,2,bi,bj,myThid)
      ENDIF
#endif

      RETURN
      END
