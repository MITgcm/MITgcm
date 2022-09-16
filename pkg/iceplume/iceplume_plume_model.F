#include "ICEPLUME_OPTIONS.h"

CBOP
C     !ROUTINE: ICEPLUME_PLUME_MODEL
C     !INTERFACE:
      SUBROUTINE ICEPLUME_PLUME_MODEL (mythid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ICEPLUME_PLUME_MODEL:
C     | o Uses ODEPACK to solve one of 3 models describing subglacial
C     |   runoff driven plumes
C     | 
C     *===========================================================
c	\ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"
#include "ICEPLUME_OPTIONS.h"


C     !INPUT/OUTPUT PARAMETERS:

C     ==Local variables==

C     Plume models
      EXTERNAL JENKINS, HALFCONE, DETACHED_CONE, JEX

C     For ODEPACK
      INTEGER  IOPT, IOUT, ISTATE, ITASK, ITOL, IWORK(20), LIW, LRW,
     &         MF, NEQ
      _RL T, TOUT

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      _RL  RWORK(148), Y(9)
#else
      _RL  RWORK(116), Y(7)
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

C     Y is input/output vector for DLSODE
C       Y(1) = plume thickness/radius
C       Y(2) = plume velocity
C       Y(3) = plume temperature
C       Y(4) = plume salinity
C       Y(5) = plume area
C       Y(6) = area integrated melt
C       Y(7) = angle of plume (detatched plume only)
C       Y(8) = distance of plume from ice (detatched plume only)
 

C     Other local variables
      INTEGER  k, mythid
      DOUBLE PRECISION RHO
      DOUBLE PRECISION temperature, salinity, depth
      DOUBLE PRECISION tambient, sambient
      DOUBLE PRECISION rho_plume, rho_ambient
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_ICEPLUME

C For ODEPACK solver. See ODEPACK documentation for definitions.
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      NEQ = 8
      LRW = 148
      LIW = 148
#else
      NEQ = 6
      LRW = 116
      LIW = 116
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
      ITOL = 1
      ITASK = 1
      ISTATE = 1
      IOPT = 0
      MF = 10
      IWORK(7) = 2 ! To limit number of times repeat error messages are printed

C Initial conditions
      Y(2) = w_sg          ! initial vertical velocity
      Y(3) = T_sg          ! initial temperature
      Y(4) = S_sg          ! initial salinity
      Y(5) = 0.0           ! integrated contact area
      Y(6) = 0.0           ! integrated melt rate
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      Y(7) = theta_sg          ! initial angle -- make nearly horizontal
      Y(8) = 0.0
      Y(9) = delta_y
      IF (useDetachPlume) THEN
      Y(1) = 0.5 * PI*r_sg**2 ! initial area
                              ! value set for consistency with half-cone
      ELSE
      Y(1) = r_sg
      ENDIF
#else
      Y(1) = r_sg          ! intial plume thickness
      Y(7) = delta_y
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */


C Prepare profiles

      DO K = 1,Nr+1
       zProf(k) = rF(k)
       zProfAbs(k) = abs(rF(k))
       rProfPlume(k) = 0.0
       wProfPlume(k) = 0.0
       tProfPlume(k) = 0.0
       sProfPlume(k) = 0.0
       aProfPlume(k) = 0.0
       mIntProfPlume(k) = 0.0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
       thetaProfPlume(k) = 0.0
       distanceProfPlume(k) = 0.0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
      ENDDO

C Start at bottom of ice face
       T = iceDepth

C Next point at which to retrieve values
       TOUT = zProf(iceDepthK-1)

C Clear cells below depth of ice face
      DO k = Nr+1,iceDepthK+1
       rProfPlume(k) = 0.D0
       wProfPlume(k) = 0.D0
       tProfPlume(k) = 0.D0
       sProfPlume(k) = 0.D0
       aProfPlume(k)  = 0.D0
       mIntProfPlume(k) = 0.D0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
       thetaProfPlume(k)    = 0.D0
       distanceProfPlume(k) = 0.D0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
      ENDDO
   
C Set intial conditions
      rProfPlume(iceDepthK) = Y(1)
      wProfPlume(iceDepthK) = Y(2)
      tProfPlume(iceDepthK) = Y(3)
      sProfPlume(iceDepthK) = Y(4)
      aProfPlume(iceDepthK) = Y(5)
      mIntProfPlume(iceDepthK) = Y(6)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      thetaProfPlume(iceDepthK) = Y(7)
      distanceProfPlume(iceDepthK) = Y(8)
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */


C Move up through water column from lowest layer

      DO IOUT = iceDepthK-1,1,-1
      
C Check to make sure plume hasn't reached neutral buoyancy in a lower layer
       IF ( ISTATE .GT. -1 ) THEN 

C Decide which plume model to use
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
        Y(9) = delta_y
#else
        Y(7) = delta_y
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

        IF ( useSheetPlume ) THEN

         CALL DLSODE (JENKINS,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,
     &               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)

        ELSEIF ( useConePlume ) THEN

         CALL DLSODE (HALFCONE,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,
     &               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME

        ELSEIF ( useDetachPlume ) THEN

         CALL DLSODE (DETACHED_CONE,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,
     &        ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)

#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

        ENDIF

C Test to see if neutral buoyancy has now been reached.
C If solver returns ISTATE = -1, then it has been unable to meet required tolerances
C at this level. This generally occurs because plume has reached neutral buoyancy and
C run out of momentum, and so is no longer rising. At this point, we therefore end
C the call to the plume model.
C Our aim is to catch the plume at the point of neutral buoyancy. We therefore perform
C a manual comparrison of ambient and plume density. If plume density >= ambient density
C we assign ISTATE = -1, again ending the call to the plume model.

        temperature = Y(3)
        salinity = Y(4)
        depth = T

C Calculate plume density
        rho_plume = RHO(temperature,salinity,depth)

        IF (IOUT.EQ.1) THEN
         tambient = tprof(1)
         sambient = sprof(1)
        ELSE
         tambient = .5*(tprof(IOUT-1)+tprof(IOUT))
         sambient = .5*(sprof(IOUT-1)+sprof(IOUT))
        ENDIF

C Calculate ambient density
        rho_ambient = RHO(tambient,sambient,depth)

        IF ( rho_plume .GT. rho_ambient ) ISTATE = -1

C If ISTATE is now < 0, then plume has reached neutral buoyancy 
        IF ( ISTATE .LT. 0 ) THEN

C If we have reached neutral buoyancy then there is no volume flux out of this cell,
C so plume area and velocity equal zero.
C Other values are kept for use in determining plume outflow properties.

         Y(1) = 0.D0
         Y(2) = 0.D0

        ELSE

C If the plume has not reached neutral buoyancy, then we assign a depth at which to calculate the next value and loop round to call the plume model again.
C Make sure we're not at the surface

         IF ( iout .NE. 1 ) THEN

C Define present depth

          T=TOUT

C Define next depth

          TOUT = zProf(IOUT-1)

         ENDIF
        ENDIF

C This section is entered once the plume has reached neutral buoyancy
       ELSE

C Once plume has reached neutral buoyancy, no plume values
        Y(1) = 0.0
        Y(2) = 0.0
        Y(3) = 0.0
        Y(4) = 0.0
        Y(5) = 0.0
        Y(6) = 0.0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
        Y(7) = 0.0
        Y(8) = 0.0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

       ENDIF

C Save results
       
       rProfPlume(iout) = Y(1)
       wProfPlume(iout) = Y(2)
       tProfPlume(iout) = Y(3)
       sProfPlume(iout) = Y(4)
       aProfPlume(iout) = Y(5)
       mIntProfPlume(iout) = Y(6)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
       thetaProfPlume(iout) = Y(7)
       distanceProfPlume(iout) = Y(8)

#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

      ENDDO

#endif /* ALLOW_ICEPLUME */

      RETURN
      END


C---------------------------------------------------

      SUBROUTINE  JENKINS (NEQ, T, Y, YDOT)

#include "ICEPLUME_OPTIONS.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"

      INTEGER  NEQ
      DOUBLE PRECISION T, Y(7), YDOT(6)
      _RL Tambient, Sambient, rho_0, rho_1, Tin
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO

#ifdef ALLOW_ICEPLUME

C     Y(1) = r
C     Y(2) = w
C     Y(3) = T
C     Y(4) = S

      
C Interpolate from imposed ambient profiles

      IF ( abs(T) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(T) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(T),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(T),Sambient)
      ENDIF

	  Tin = T
	  
C Plume density
      rho_1   = RHO(Y(3),Y(4),Tin)
C Ambient density
      rho_0 = RHO(Tambient,Sambient,Tin)

C Equations for Sb, Tb and mdot

      a = lambda1*(GamT*c_w-GamS*c_i)

      b = GamS*c_i*(lambda1*Y(4)-lambda2-lambda3*Tin+
     &         iceTemp-(L/c_i))
     &        -GamT*c_w*(Y(3)-lambda2-lambda3*Tin)

      c = GamS*Y(4)*(c_i*(lambda2+lambda3*Tin-iceTemp)+L)

      Sb   = (1./(2.*a))*(-b-((b**2.-4.*a*c)**0.5)) !Sb
      Tb   = lambda1*Sb+lambda2+lambda3*Tin !Tb
      mdot = GamS*(Cd**0.5)*Y(2)*(Y(4)-Sb)/Sb ! mdot

C Differential equations
C Plume thickness
      YDOT(1)=2*E_0+Cd-(g*Y(1)/(Y(2)**2))*(rho_0-rho_1)
     &  /rho_ref+2*mdot/Y(2)

C Plume vertical velocity
      YDOT(2)=-(Y(2)/Y(1))*(E_0+Cd+mdot/Y(2))
     &  +(g/Y(2))*(rho_0-rho_1)/rho_ref

C Plume temperature
      YDOT(3)=E_0*(Tambient-Y(3))/Y(1)
     &  +mdot*(Tb-Y(3))/(Y(1)*Y(2))
     &  -GamT*(Cd**0.5)*(Y(3)-Tb)/Y(1)

C Plume salinity
      YDOT(4)=E_0*(Sambient-Y(4))/Y(1)
     &  +mdot*(Sb-Y(4))/(Y(1)*Y(2))
     &  -GamS*(Cd**0.5)*(Y(4)-Sb)/Y(1)

C along-plume integrated melt rate and contact area  - 
      YDOT(5) = Y(7)
      YDOT(6) = Y(7) * mdot

      T = Tin

#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C - ------------------------------------------------------------------

      SUBROUTINE  HALFCONE (NEQ, T, Y, YDOT)

#include "ICEPLUME_OPTIONS.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"

      INTEGER  NEQ
      DOUBLE PRECISION T, Y(7), YDOT(6), Tin
      _RL Tambient, Sambient, rho_0, rho_1
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO

#ifdef ALLOW_ICEPLUME

C     Y(1) = r
C     Y(2) = w
C     Y(3) = T
C     Y(4) = S

     
C     Interpolate from imposed ambient profiles

      IF ( abs(T) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(T) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(T),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(T),Sambient)
      ENDIF

      Tin = T
      rho_1   = RHO(Y(3),Y(4),Tin)
      rho_0 = RHO(Tambient,Sambient,Tin)

C     Equations for Sb, Tb and mdot

      a = lambda1*(GamT*c_w-GamS*c_i)

      b = GamS*c_i*(lambda1*Y(4)-lambda2-lambda3*Tin+
     &         iceTemp-(L/c_i))
     &        -GamT*c_w*(Y(3)-lambda2-lambda3*Tin)

      c = GamS*Y(4)*(c_i*(lambda2+lambda3*Tin-iceTemp)+L)

      Sb   = (1./(2.*a))*(-b-((b**2.-4.*a*c)**0.5)) !Sb
      Tb   = lambda1*Sb+lambda2+lambda3*Tin !Tb
      mdot = GamS*(Cd**0.5)*Y(2)*(Y(4)-Sb)/Sb ! mdot

      YDOT(1) = 2.*E_0+4.*mdot/(pi*Y(2))-Y(1)*g*
     &          (rho_0-rho_1)/(2.*Y(2)*Y(2)*rho_ref)+2.*Cd/pi

      YDOT(2) = -2.*E_0*Y(2)/Y(1)-4.*mdot/(pi*Y(1))+g*
     &          (rho_0-rho_1)/(Y(2)*rho_ref)-4.*Cd*Y(2)/(pi*Y(1))

      YDOT(3) = 2.*E_0*(TAMBIENT-Y(3))/Y(1)+4.*mdot*
     &           (Tb-Y(3))/(pi*Y(1)*Y(2))-4.*
     &           GamT*(Cd**0.5)*(Y(3)-Tb)/(pi*Y(1))

      YDOT(4) = 2.*E_0*(Sambient-Y(4))/Y(1)+4.*mdot*(Sb-Y(4))/
     &          (pi*Y(1)*Y(2))-4.*GamS*(Cd**0.5)*(Y(4)-Sb)/(pi*Y(1))

      YDOT(5) = 2.*Y(1)
      YDOT(6) = 2.*Y(1)*mdot

      T = Tin
#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C - ------------------------------------------------------------------

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME

      SUBROUTINE  DETACHED_CONE (NEQ, T, Y, YDOT)

#include "ICEPLUME_OPTIONS.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"

      INTEGER  NEQ
      DOUBLE PRECISION T, Y(9), YDOT(8), Tin
      _RL Tambient, Sambient, rho_0, rho_1
      _RL Wplume, Cplume
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO

      EXTERNAL PLUME_GEOMETRY

#ifdef ALLOW_ICEPLUME

C     Y(1) = r
C     Y(2) = w
C     Y(3) = T
C     Y(4) = S

     
C     Interpolate from imposed ambient profiles

      IF ( abs(T) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(T) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(T),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(T),Sambient)
      ENDIF

      Tin = T
      rho_1   = RHO(Y(3),Y(4),Tin)
      rho_0 = RHO(Tambient,Sambient,Tin)

C     Equations for Sb, Tb and mdot

      a = lambda1*(GamT*c_w-GamS*c_i)

      b = GamS*c_i*(lambda1*Y(4)-lambda2-lambda3*Tin+
     &         iceTemp-(L/c_i))
     &        -GamT*c_w*(Y(3)-lambda2-lambda3*Tin)

      c = GamS*Y(4)*(c_i*(lambda2+lambda3*Tin-iceTemp)+L)

      Sb   = (1./(2.*a))*(-b-((b**2.-4.*a*c)**0.5)) !Sb
      Tb   = lambda1*Sb+lambda2+lambda3*Tin !Tb
      mdot = GamS*(Cd**0.5)*Y(2)*(Y(4)-Sb)/Sb ! mdot


      CALL PLUME_GEOMETRY(Y(1),Y(8),Cplume,Wplume)


      YDOT(1) = 2*E_0*Cplume/sin(Y(7))+
     &          2*Wplume*mdot/(sin(Y(7))*Y(2))-
     &          Y(1)*gravity*(rho_0-rho_1)/(Y(2)*Y(2)*rho_ref)+
     &          Cd*Wplume

      YDOT(2) = -E_0*Cplume*Y(2)/Y(1)/sin(Y(7))-
     &          Wplume*mdot/(Y(1)*sin(Y(7)))+
     &          gravity*(rho_0-rho_1)/(Y(2)*rho_ref)-
     &          Cd*Wplume*Y(2)/Y(1)

      YDOT(3) = (E_0*Cplume*(Tambient-Y(3))/Y(1)+
     &           Wplume*mdot*(Tb-Y(3))/(Y(1)*Y(2))-
     &           GamT*sqrt(Cd)*Wplume*(Y(3)-Tb)/(Y(1)))
     &          /sin(Y(7))


      YDOT(4) = (E_0*Cplume*(Sambient-Y(4))/Y(1)+
     &           Wplume*mdot*(Sb-Y(4))/(Y(1)*Y(2))-
     &           GamS*sqrt(Cd)*Wplume*(Y(4)-Sb)/(Y(1)))
     &          /sin(Y(7))

      YDOT(5) = Wplume
      YDOT(6) = Wplume*mdot

      YDOT(7) = (Y(2)*Y(2)*YDOT(1)+2*Y(2)*Y(1)*YDOT(2)) /
     &          (Y(2)*Y(2)*Y(1))/TAN(Y(7))
      YDOT(8) = 1.0/(TAN(Y(7)))

      T = Tin


#endif /* ALLOW_ICEPLUME */

      RETURN
      END

#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

C----------------------------------------------------------------------
C Dummy routine for ODEPACK. Necessary for Jacobian matrix if stiff ODEs.

      SUBROUTINE jex()
      RETURN
      END

C ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION RHO(t,S,z)

C Equation of state (UNESCO 1983)

C     T = temperature (deg C)
C     S = salinity (PSU)
C     z = depth (m)

      DOUBLE PRECISION T,S,z
      DOUBLE PRECISION rho_0, g, P
      DOUBLE PRECISION kw, Aw, Bw, k0
      DOUBLE PRECISION bulk_modulus
      DOUBLE PRECISION A, B, C, rho_w,rho_zero

      PARAMETER(rho_0=1027)
      PARAMETER(g=9.81)

      P= rho_0*g*abs(z)*1.0E-5
   
C ... RHO_1 (in situ) (saved in Y(7))
      kw= 19652.21+ 148.4206*T- 2.327105*T**2+ 
     &    1.360477e-2*(T**3)-5.155288e-5*(T**4)
      Aw= 3.239908+ 1.43713e-3*T+ 1.16092e-4*T**2-
     &    5.77905e-7*T**3
      Bw= 8.50935e-5- 6.12293e-6*T + 5.2787e-8*(T**2)
      k0= kw + (54.6746- 0.603459*T+ 1.09987e-2*(T**2)
     &    -6.1670e-5*(T**3))*S +(7.944e-2 + 1.6483e-2*
     &    T- 5.3009e-4*(T**2))*(S**1.5)
      A=  Aw+ (2.2838e-3- 1.0981e-5*T- 1.6078e-6*(T**2))
     &    *S+ 1.91075e-4*(S**1.5)
      B= Bw+ (-9.9348e-7+ 2.0816e-8*T+ 9.1697e-10*T**2)*S
      bulk_modulus= k0+ A*P+ B*P**2

      A= 8.24493e-1- 4.0899e-3*T+ 7.6438e-5*T**2-
     &   8.2467e-7*T**3+5.3875e-9*T**4
      B= -5.72466e-3 + 1.0227e-4*T- 1.6546e-6*T**2
      C= 4.8314e-4
      rho_w= 999.842594 + 6.793952e-2*T- 9.095290e-3*T**2+
     &       1.001685e-4*T**3-1.120083e-6*T*
     &       *4+ 6.536336e-9*T**5
      rho_zero= rho_w+ A*S + B*(S**1.5)+ C*(S**2)

      RHO= rho_zero/(1- (P/bulk_modulus))

      RETURN
      END

C-------------------------------------------------------------


      subroutine linint(nx,xtab,ytab,x,y)
c
c     Given a value of x return a value of y based on interpolation
c     within a table of y values (ytab) corresponding to the x values
c     contained in the array xtab.  The subroutine assumes that the
c     values in xtab increase monotonically
c
c    John Mahaffy 2/12/95
c    Modified slightly TRC 2014

      integer nx
      double precision xtab(nx), ytab(nx), x, y

c     local variables
      integer i, i1
      double precision  wx

      if (x.lt.(xtab(1)).or.x.GT.(xtab(nx))) then
         write(6,*) 'x = ', x, '  is out of table range'
         stop
      endif
      do 100 i=2,nx
         if (x.le.xtab(i)) go to 200
  100    continue
  200 i1=i-1
      wx=(x-xtab(i1))/(xtab(i1+1)-xtab(i1))
      y=(1-wx)*ytab(i1)+wx*ytab(i1+1)
      return
      end

C-------------------------------------------------------------

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      SUBROUTINE PLUME_GEOMETRY(A,X,C,W)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

      _RL A   ! input: plume cross-sectional area
      _RL X   ! input: distance of center from wall
      _RL C   ! output: perimeter exposed to ambient
      _RL W   ! output: perimeter exposed to wall

      _RL B, f, fp, tol, adj
      INTEGER ITERMAX, I
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C ---------------------------------------------------

      B = sqrt(A/pi)
    
      IF (B.le.X) THEN

!      PLUME IS FULLY DETACHED    

       C = 2*PI*B
       W = 0

      ELSE
     
!      PLUME IS PARTLY OR NON-DETACHED (half-conical)
!      FIND SHAPE CONSISTENT WITH X AND A

       TOL = 1.e-12
       ITERMAX = 300

       DO I=1,ITERMAX
        F = B*B * (PI-acos(X/B)) + X*(B*B-X*X)**0.5 - A
        FP = 2*B*(PI - acos(X/B)) - X/(1 - X*X/(B*B))**0.5 + 
     &   (X*B)/(B*B - X*X)**0.5
        ADJ = f/fp
        B = B - ADJ
        if (B .LT. X) then
            B=X+.001
        endif
       if ( ABS(ADJ) .LT. TOL) GOTO 20
       enddo

   20  CONTINUE

       if (I.eq.ITERMAX) then
                WRITE(msgBuf,'(2A)')
     &          'cannot use detaching plume without ',
     &          '#define ICEPLUME_ALLOW_DETACHED_PLUME'
                CALL PRINT_ERROR( msgBuf, 1 )
                STOP 'ABNORMAL END: S/R ICEPLUME_PLUME_MODEL'
       endif
    
       W = 2 * sqrt(B*B-X*X)
       C = 2 * (pi-acos(X/b)) * B
     
      ENDIF

      return
      end
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
