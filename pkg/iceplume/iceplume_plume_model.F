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
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"


C     !INPUT/OUTPUT PARAMETERS:

C     ==Local variables==

C     Plume models
C Change the name JENKINS to SHEETPLUME
      EXTERNAL SHEETPLUME, HALFCONE, DETACHED_CONE, JEX !JENKINS
     & , WANDW 
C     For ODEPACK
      INTEGER  IOPT, IOUT, ISTATE, ITASK, ITOL, IWORK(20), LIW, LRW,
     &         MF, NEQ
C In original code Depth takes variable T, which is very confusing
C since we also have T as temperature sometimes. Now change to DD.
      _RL DD, DDOUT

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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('ICEPLUME_PLUME_MODEL',myThid)
#endif
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
C ============
C A few notes on "initial" as inputs:
C Time constant:
C T_sg_0, S_sg_0, Angle_sg_0: read in from namelist or initialized
C                             in readparms and are NEVER CHANGED.
C Time dependent:
C The pair w_sg,r_sg or Q_sg are forcing values and geometry at the
C point source, and are typically read in from file(s).
C If these files not available, then they take values assigned
C from namelist or initialized from readparms.  If read in the pair
C w_sg and r_sg, note that their individual values are not important
C but rather their combination to produce the intended Q_sg, with the
C relation done in iceplume_calc.F.  If read in Q_sg, then we typically
C fix w_sg and compute r_sg (done in iceplume_calc.F).
C Outputs:
C This takes inputs [w,r,Q]_sg, [T,S,Angle]_sg_0 to compute updated Y(9)
C (replacing input dLtangential) to update [r,w,t,s,a,mInt]ProfPlume(iout)
C ============
      Y(2) = w_sg          ! time-dependent vertical velocity at point source
      Y(3) = T_sg_0        ! initial temperature at point source
      Y(4) = S_sg_0        ! initial salinity at point source
      Y(5) = 0.0           ! integrated contact area
      Y(6) = 0.0           ! integrated melt rate
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
      Y(7) = Angle_sg_0    ! initial angle at point source -- make nearly horizontal
      Y(8) = 0.0
      Y(9) = dLtangential
      IF (useDetachPlume) THEN
      Y(1) = HalfRL * PI*r_sg**twoRL ! initial area
                              ! value set for consistency with half-cone
      ELSE
      Y(1) = r_sg
      ENDIF
#else
      Y(1) = r_sg          ! intial plume thickness
      Y(7) = dLtangential
      IF (useTruncPlume) THEN
       Y(7) = L_sg
      ENDIF
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
       DD = iceDepth

C Next point at which to retrieve values
       DDOUT = zProf(iceDepthK-1)

C Clear cells below depth of ice face
C The loop below needs to loop ONLY to Nr+1 and not
C iceDepthK+1 in the case iceDepthK = Nr+1. Else we
C encounter out of bound error
C      DO k = Nr+1,iceDepthK+1
      DO k = Nr+1,iceDepthK
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
        Y(9) = dLtangential
#else
        Y(7) = dLtangential
        IF (useTruncPlume) THEN
         Y(7) = L_sg 
        ENDIF
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

        IF ( useSheetPlume .OR. useTruncPlume ) THEN

         CALL DLSODE (SHEETPLUME,NEQ,Y,DD,DDOUT,ITOL,RTOL,ATOL,ITASK,
     &               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)

        ELSEIF ( useConePlume ) THEN

         CALL DLSODE (HALFCONE,NEQ,Y,DD,DDOUT,ITOL,RTOL,ATOL,ITASK,
     &               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)
        ELSEIF ( useBuoyPlume ) THEN

         CALL DLSODE (WANDW,NEQ,Y,DD,DDOUT,ITOL,RTOL,ATOL,ITASK,
     &               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME

        ELSEIF ( useDetachPlume ) THEN

         CALL DLSODE (DETACHED_CONE,NEQ,Y,DD,DDOUT,ITOL,RTOL,ATOL,
     &        ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF)

#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

        ENDIF

C Test to see if neutral buoyancy has now been reached.
C If solver returns ISTATE = -1, then it has been unable to meet required
C tolerances at this level. This generally occurs because plume has reached
C neutral buoyancy and run out of momentum, and so is no longer rising. At
C this point, we therefore end the call to the plume model.

C Our aim is to catch the plume at the point of neutral buoyancy. We
C therefore perform a manual comparison of ambient and plume density.
C If plume density >= ambient density we assign ISTATE = -1, again
C ending the call to the plume model.

        temperature = Y(3)
        salinity = Y(4)
        depth = DD

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

C If the plume has not reached neutral buoyancy, then we assign a depth at which
C to calculate the next value and loop round to call the plume model again.
C Make sure we're not at the surface

         IF ( iout .NE. 1 ) THEN

C Define present depth

          DD=DDOUT

C Define next depth

          DDOUT = zProf(IOUT-1)

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

      SUBROUTINE  SHEETPLUME (
     I                 NEQ,
     U                 DD, Y,
     O                 YDOT  )
C Note: in functions such as this one,
C we solve at depth DD two sets of equations simutaneously for the
C glacial front mass balance (3eq, iceplume_meltrate.F) and plume
C properties (4eq).  The order is that we take in the subglacial
C DISCHARGE properties [w,r]sg at depth  DD (the bottom of the
C vertical front), and first call iceplume_meltrate to calculate
C the total melt and T,S stored in [mdot,tb,sb].
C Then, we solve the four equations for plume tendencies YDOT,
C Y(T,S,vertical vel, radii) as a function of depth. Y will be
C resaved in iceplume_plume_model into vertical profile properties
C [t,s,w,r,a,mInt]ProfPlume.

Ckiki here I added my meltrate parameterization
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

      INTEGER  NEQ
      DOUBLE PRECISION DD, Y(7), YDOT(6)
      _RL Tambient, Sambient, rho_ambient, rho_plume, DDin
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO
      DOUBLE PRECISION absVelocity, factor_cd

#ifdef ALLOW_ICEPLUME

C Interpolate from imposed ambient profiles

      IF ( abs(DD) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(DD) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(DD),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(DD),Sambient)
      ENDIF

      DDin = DD

C Plume density
      rho_plume   = RHO(Y(3),Y(4),DDin)
C Ambient density
      rho_ambient = RHO(Tambient,Sambient,DDin)

C Equations for Sb, Tb and mdot

c================
C Set some constant that we use all the time, as well as
C minimum background velocity if we use default Jenkin et al:
      IF (ABS(Y(2)).LT.backgroundVelThresh) THEN
       absVelocity = backgroundVelThresh
      ELSE
       absVelocity = ABS(Y(2))
      ENDIF
#ifndef ICEPLUME_ALLOW_SCHULZ22
      factor_Cd = (Cd**HalfRL)*absVelocity
#else
      factor_Cd = (Cd**HalfRL)*(absVelocity-backgroundVelThresh)
#endif

      call iceplume_meltrate(
     I        Y(3),Y(4),Y(2),DD,
     O        mdot, Tb, Sb)

C Differential equations
C Plume thickness
      YDOT(1)=twoRL*E_0+Cd-(gravity*Y(1)/(absVelocity**twoRL))
     &  *(rho_ambient-rho_plume)/rhoConst+twoRL*mdot/absVelocity

C Plume vertical velocity
      YDOT(2)=-(absVelocity/Y(1))*(E_0+Cd+mdot/absVelocity)
     &  +(gravity/absVelocity)*(rho_ambient-rho_plume)/rhoConst

C Plume temperature
C In default code, factor_Cd = (Cd**HalfRL)*absVelocity
C Need to check whether all Y(2) should be replaced with absVelocity.
C I think so. If so, factor_Cd/absVelocity = (Cd**HalfRL). Lastly,
C pull absVelocity out all the way to the end outside of ifdef block,
C as well as the common factor (Tb-Y(3))/Y(1).  Also, note I flipped
C the original -(Y(3)-Tb) to +(Tb-Y(3)) prior to pulling out
      YDOT(3)=E_0*(Tambient-Y(3))/Y(1)
     &  +mdot*(Tb-Y(3))/(Y(1)*absVelocity)
#ifndef ICEPLUME_ALLOW_SCHULZ22
     &  -GamT*(Cd**HalfRL)*(Y(3)-Tb)/Y(1)
#else
     & -(GamT*(Cd**HalfRL)*(absVelocity-backgroundVelThresh)+GamTconst)
     & *(Y(3)-Tb)/(Y(1)*absVelocity)
#endif

C Plume salinity
      YDOT(4)=E_0*(Sambient-Y(4))/Y(1)
     &  +mdot*(Sb-Y(4))/(Y(1)*absVelocity)
#ifndef ICEPLUME_ALLOW_SCHULZ22
     &  -GamS*(Cd**HalfRL)*(Y(4)-Sb)/Y(1)
#else
     & -(GamS*(Cd**HalfRL)*(absVelocity-backgroundVelThresh)+GamSconst)
     &  *(Y(4)-Sb)/(Y(1)*absVelocity)
#endif

C along-plume integrated melt rate and contact area  - 
      YDOT(5) = Y(7)
      YDOT(6) = Y(7) * mdot

C As i suspected, the calculations of YDOT are different depending
C on the geometry, so we cannot generalize it with a subroutine
      DD = DDin

#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C - ------------------------------------------------------------------

      SUBROUTINE  HALFCONE (
     I                 NEQ,
     U                 DD, Y,
     O                 YDOT)
Ckiki: here I added my new melt rate paramterization
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

      INTEGER  NEQ
      DOUBLE PRECISION DD, Y(7), YDOT(6)
      _RL Tambient, Sambient, rho_ambient, rho_plume, DDin !, ut
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO
      DOUBLE PRECISION absVelocity, factor_cd

#ifdef ALLOW_ICEPLUME

C     Interpolate from imposed ambient profiles

      IF ( abs(DD) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(DD) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(DD),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(DD),Sambient)
      ENDIF

      DDin = DD

C Plume density
      rho_plume   = RHO(Y(3),Y(4),DDin)
C Ambient density
      rho_ambient = RHO(Tambient,Sambient,DDin)

C     Equations for Sb, Tb and mdot

C================
C set some constant that we use all the time, as well as
C minimum background velocity if we use default Jenkin et al:
      IF (ABS(Y(2)).LT.backgroundVelThresh) THEN
       absVelocity = backgroundVelThresh
      ELSE
       absVelocity = ABS(Y(2))
      ENDIF
#ifndef ICEPLUME_ALLOW_SCHULZ22
      factor_Cd = (Cd**HalfRL)*absVelocity
#else
      factor_Cd = (Cd**HalfRL)*(absVelocity-backgroundVelThresh)
#endif

C     Equations for Sb, Tb and mdot

      call iceplume_meltrate(
     I        Y(3),Y(4),Y(2),DD,
     O        mdot, Tb, Sb)

C Differential equations
C Plume thickness
      YDOT(1) = twoRL*E_0+4.*mdot/(pi*absVelocity)
     &         -Y(1)*gravity*(rho_ambient-rho_plume)
     &            /(twoRL*absVelocity*absVelocity*rhoConst)
     &         +twoRL*Cd/pi

C Plume vertical velocity
      YDOT(2) = -twoRL*E_0*absVelocity/Y(1)-4.*mdot/(pi*Y(1))
     &          +gravity*(rho_ambient-rho_plume)/(absVelocity*rhoConst)
     &          -4.*Cd*absVelocity/(pi*Y(1))

C Plume temperature
C In default code, factor_Cd = (Cd**HalfRL)*absVelocity
C Need to check whether all Y(2) should be replaced with absVelocity.
C I think so. If so, factor_Cd/absVelocity = (Cd**HalfRL). Lastly,
C pull absVelocity out all the way to the end outside of ifdef block,
C as well as the common factor (Tb-Y(3))/(pi*Y(1)).  Also, note I
C flipped the original -4.*(Y(3)-Tb) to +4.*(Tb-Y(3)) prior to pulling
      YDOT(3) = twoRL*E_0*(Tambient-Y(3))/Y(1)
     &         +4.*mdot*
     &           (Tb-Y(3))/(pi*Y(1)*absVelocity)-4.*
#ifndef ICEPLUME_ALLOW_SCHULZ22
     &           GamT*(Cd**HalfRL)*(Y(3)-Tb)/(pi*Y(1))
#else
     &          +GamTconst
     &      *(Y(3)-Tb)/(pi*Y(1)*absVelocity)
#endif

C same as above, factor_Cd/absVelocity = (Cd**HalfRL)
C same flipping of sign and pulling common factors to the end
      YDOT(4) = twoRL*E_0*(Sambient-Y(4))/Y(1)
     &         +4.*mdot*(Sb-Y(4))/
     &          (pi*Y(1)*absVelocity)-4.*
#ifndef ICEPLUME_ALLOW_SCHULZ22
     &           GamS*(Cd**HalfRL)*(Y(4)-Sb)/(pi*Y(1))
#else
     &              + GamSconst
     &      *(Y(4)-Sb)/(pi*Y(1)*absVelocity)
#endif

C along-plume integrated melt rate and contact area
      YDOT(5) = twoRL*Y(1)
      YDOT(6) = twoRL*Y(1)*mdot

      DD = DDin
#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C - ------------------------------------------------------------------

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME

      SUBROUTINE  DETACHED_CONE (
     I                 NEQ,
     U                 DD, Y,
     O                 YDOT)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

      INTEGER  NEQ
      DOUBLE PRECISION DD, Y(9), YDOT(8), DDin
      _RL Tambient, Sambient, rho_ambient, rho_plume
      _RL Wplume, Cplume
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO
      DOUBLE PRECISION absVelocity, factor_Cd

      EXTERNAL PLUME_GEOMETRY

#ifdef ALLOW_ICEPLUME

C     Interpolate from imposed ambient profiles

      IF ( abs(DD) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(DD) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(DD),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(DD),Sambient)
      ENDIF

      DDin = DD
C plume density
      rho_plume   = RHO(Y(3),Y(4),DDin)         !formerly rho_1
C ambient density
      rho_ambient = RHO(Tambient,Sambient,DDin) !formerly rho_0
C================
C Set some constant that we use all the time, as well as
C minimum background velocity if we use default Jenkin et al:
      IF (ABS(Y(2)).LT.backgroundVelThresh) THEN
       absVelocity = backgroundVelThresh
      ELSE
       absVelocity = ABS(Y(2))
      ENDIF
#ifndef ICEPLUME_ALLOW_SCHULZ22
      factor_Cd = (Cd**HalfRL)*absVelocity
#else
      factor_Cd = (Cd**HalfRL)*(absVelocity-backgroundVelThresh)
#endif

C     Equations for Sb, Tb and mdot

      call iceplume_meltrate(
     I        Y(3),Y(4),Y(2),DD,
     O        mdot, Tb, Sb)


      CALL PLUME_GEOMETRY(Y(1),Y(8),Cplume,Wplume)


C Differential equations
C Plume thickness
      YDOT(1) = twoRL*E_0*Cplume/sin(Y(7))+
     &          twoRL*Wplume*mdot/(sin(Y(7))*absVelocity)-
     &          Y(1)*gravity*(rho_ambient-rho_plume)
     &                       /(absVelocity*absVelocity*rhoConst)
     &          +Cd*Wplume

C Plume vertical velocity
      YDOT(2) = -E_0*Cplume*absVelocity/Y(1)/sin(Y(7))-
     &          Wplume*mdot/(Y(1)*sin(Y(7)))+
     &          gravity*(rho_ambient-rho_plume)/(absVelocity*rhoConst)-
     &          Cd*Wplume*absVelocity/Y(1)

C Plume temperature
      YDOT(3) = (E_0*Cplume*(Tambient-Y(3))/Y(1)+
     &           Wplume*mdot*(Tb-Y(3))/(Y(1)*absVelocity)-
     &           GamT*sqrt(Cd)*Wplume*(Y(3)-Tb)/(Y(1)))
     &          /sin(Y(7))

C Plume salinity
      YDOT(4) = (E_0*Cplume*(Sambient-Y(4))/Y(1)+
     &           Wplume*mdot*(Sb-Y(4))/(Y(1)*absVelocity)-
     &           GamS*sqrt(Cd)*Wplume*(Y(4)-Sb)/(Y(1)))
     &          /sin(Y(7))

      YDOT(5) = Wplume
      YDOT(6) = Wplume*mdot

C rewriting to avoid mult only to div by same number
      YDOT(7) = (absVelocity*absVelocity*YDOT(1)
     &          + twoRL*absVelocity*Y(1)*YDOT(2)) /
     &          (absVelocity*absVelocity*Y(1))/TAN(Y(7))
      YDOT(8) = OneRL/(TAN(Y(7)))

      DD = DDin


#endif /* ALLOW_ICEPLUME */

      RETURN
      END

#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */

C----------------------------------------------------------------------

      SUBROUTINE  WANDW (
     I                 NEQ, 
     U                 DD, Y,
     O                 YDOT)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

      INTEGER  NEQ
      DOUBLE PRECISION DD, Y(7), YDOT(6)
      _RL Tambient, Sambient, rho_ambient, rho_plume, DDin
      DOUBLE PRECISION mdot, Sb, Tb
      DOUBLE PRECISION a,b,c
      DOUBLE PRECISION RHO
      DOUBLE PRECISION absVelocity, factor_Cd

#ifdef ALLOW_ICEPLUME
#ifdef ICEPLUME_ALLOW_SCHULZ22

C Interpolate from imposed ambient profiles

      IF ( abs(DD) .LE. zProfAbs(1) ) THEN
       Tambient = tProf(1)
       Sambient = sProf(1)
      ELSEIF ( abs(DD) .GE. zProfAbs(Nr) ) THEN
       Tambient = tProf(Nr)
       Sambient = sProf(Nr)
      ELSE
       CALL linint(Nr,zProfAbs,tProf,abs(DD),Tambient)
       CALL linint(Nr,zProfAbs,sProf,abs(DD),Sambient)
      ENDIF

      DDin = DD
C Plume density
      rho_plume   = RHO(Y(3),Y(4),DDin)         !formerly rho_1
C Ambient density
      rho_ambient = RHO(Tambient,Sambient,DDin) !formerly rho_0

C Equations for Sb, Tb and mdot
C

C Functional call, assuming same set as in sheetplume and halfcone
      call iceplume_meltrate(
     I        Y(3),Y(4),Y(2),DD,
     O        mdot, Tb, Sb)

C Differential equations
C Plume thickness
      YDOT(1)=twoRL*E_0-(g*Y(1)/(absVelocity**twoRL))
     &  *(rho_ambient-rho_plume)
     &  /rhoConst+twoRL*mdot/absVelocity

C Plume vertical velocity
      YDOT(2)=-(absVelocity/Y(1))*(E_0+mdot/absVelocity)
     &  +(g/absVelocity)*(rho_ambient-rho_plume)/rhoConst

C Plume temperature
      YDOT(3)=E_0*(Tambient-Y(3))/Y(1)
     &  +mdot*(Tb-Y(3))/(Y(1)*absVelocity)
     &  -GamTconst*(Y(3)-Tb)/(Y(1)*absVelocity)
C Plume salinity
      YDOT(4)=E_0*(Sambient-Y(4))/Y(1)
     &  +mdot*(Sb-Y(4))/(Y(1)*absVelocity)
     &  -GamSconst*(Y(4)-Sb)/(Y(1)*absVelocity)

C along-plume integrated melt rate and contact area  - 
      YDOT(5) = Y(7)
      YDOT(6) = Y(7) * mdot

      DD = DDin

#endif /* ICEPLUME_ALLOW_SCHULZ22 */
#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C - ------------------------------------------------------------------
C Dummy routine for ODEPACK. Necessary for Jacobian matrix if stiff ODEs.

      SUBROUTINE jex()
      RETURN
      END

C ---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION RHO(t,S,z)

C Equation of state (UNESCO 1983)

C     T = temperature (deg C)
C     S = salinity (g/kg)
C     z = depth (m)

      DOUBLE PRECISION T,S,z
      DOUBLE PRECISION rho_0, g, P
      DOUBLE PRECISION kw, Aw, Bw, k0
      DOUBLE PRECISION bulk_modulus
      DOUBLE PRECISION A, B, C, rho_w,rho_zero

      PARAMETER(rho_0=1027. _d 0)
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

       C = twoRL*PI*B
       W = zeroRL

      ELSE
     
!      PLUME IS PARTLY OR NON-DETACHED (half-conical)
!      FIND SHAPE CONSISTENT WITH X AND A

       TOL = 1.e-12
       ITERMAX = 300

       DO I=1,ITERMAX
        F = B*B * (PI-acos(X/B)) + X*(B*B-X*X)**HalfRL - A
        FP = twoRL*B*(PI - acos(X/B)) - X/(OneRL - X*X/(B*B))**HalfRL +
     &   (X*B)/(B*B - X*X)**HalfRL
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
    
       W = twoRL * sqrt(B*B-X*X)
       C = twoRL * (pi-acos(X/b)) * B
     
      ENDIF

      return
      end
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
