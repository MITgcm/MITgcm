C$Header: /u/gcmpack/MITgcm/model/src/seawater.F,v 1.3 2008/09/06 17:42:27 jmc Exp $
C$Name:  $

#include "CPP_OPTIONS.h"

C--  File seawater.F: routines that compute quantities related to seawater.
C--   Contents
C--   o FIND_RHO_SCALAR: in-situ density for individual points
C--   o SW_PTMP: function to compute potential temperature
C--   o SW_TEMP: function to compute potential temperature
C--   o SW_ADTG: function to compute adiabatic tmperature gradient
C--              used by sw_ptmp

      SUBROUTINE FIND_RHO_SCALAR(
     I     tLoc, sLoc, pLoc,
     O     rhoLoc,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO_SCALAR
C     |   Calculates rho(S,T,p)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL sLoc, tLoc, pLoc
      _RL rhoLoc
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==

      _RL t1, t2, t3, t4, s1, s3o2, p1, p2, sp5, p1t1
      _RL rfresh, rsalt, rhoP0
      _RL bMfresh, bMsalt, bMpres, BulkMod
      _RL rhoNum, rhoDen, den, epsln
      parameter ( epsln = 0. _d 0 )

      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      rhoLoc  = 0. _d 0
      rhoP0   = 0. _d 0
      bulkMod = 0. _d 0
      rfresh  = 0. _d 0
      rsalt   = 0. _d 0
      bMfresh = 0. _d 0
      bMsalt  = 0. _d 0
      bMpres  = 0. _d 0
      rhoNum  = 0. _d 0
      rhoDen  = 0. _d 0
      den     = 0. _d 0

      t1 = tLoc
      t2 = t1*t1
      t3 = t2*t1
      t4 = t3*t1

      s1  = sLoc
      IF ( s1 .LT. 0. _d 0 ) THEN
C     issue a warning
         WRITE(msgBuf,'(A,E13.5)')
     &        ' FIND_RHO_SCALAR:   WARNING, salinity = ', s1
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         s1 = 0. _d 0
      ENDIF

      IF (equationOfState.EQ.'LINEAR') THEN

         rholoc = rhoNil*(
     &                      sBeta *(sLoc-sRef(1))
     &                     -tAlpha*(tLoc-tRef(1))
     &                   ) + rhoNil
c        rhoLoc = 0. _d  0

      ELSEIF (equationOfState.EQ.'POLY3') THEN

C     this is not correct, there is a field eosSig0 which should be use here
C     but I DO not intent to include the reference level in this routine
         WRITE(msgBuf,'(A)')
     &        ' FIND_RHO_SCALAR: for POLY3, the density is not'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         WRITE(msgBuf,'(A)')
     &         '                 computed correctly in this routine'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         rhoLoc = 0. _d 0

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &      .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates

         s3o2 = s1*SQRT(s1)

         p1 = pLoc*SItoBar
         p2 = p1*p1

C     density of freshwater at the surface
         rfresh =
     &          eosJMDCFw(1)
     &        + eosJMDCFw(2)*t1
     &        + eosJMDCFw(3)*t2
     &        + eosJMDCFw(4)*t3
     &        + eosJMDCFw(5)*t4
     &        + eosJMDCFw(6)*t4*t1
C     density of sea water at the surface
         rsalt =
     &        s1*(
     &             eosJMDCSw(1)
     &           + eosJMDCSw(2)*t1
     &           + eosJMDCSw(3)*t2
     &           + eosJMDCSw(4)*t3
     &           + eosJMDCSw(5)*t4
     &           )
     &        + s3o2*(
     &             eosJMDCSw(6)
     &           + eosJMDCSw(7)*t1
     &           + eosJMDCSw(8)*t2
     &           )
     &           + eosJMDCSw(9)*s1*s1

         rhoP0 = rfresh + rsalt

C     secant bulk modulus of fresh water at the surface
         bMfresh =
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t1
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
C     secant bulk modulus of sea water at the surface
         bMsalt =
     &        s1*( eosJMDCKSw(1)
     &           + eosJMDCKSw(2)*t1
     &           + eosJMDCKSw(3)*t2
     &           + eosJMDCKSw(4)*t3
     &           )
     &    + s3o2*( eosJMDCKSw(5)
     &           + eosJMDCKSw(6)*t1
     &           + eosJMDCKSw(7)*t2
     &           )
C     secant bulk modulus of sea water at pressure p
         bMpres =
     &        p1*( eosJMDCKP(1)
     &           + eosJMDCKP(2)*t1
     &           + eosJMDCKP(3)*t2
     &           + eosJMDCKP(4)*t3
     &           )
     &   + p1*s1*( eosJMDCKP(5)
     &           + eosJMDCKP(6)*t1
     &           + eosJMDCKP(7)*t2
     &           )
     &      + p1*s3o2*eosJMDCKP(8)
     &      + p2*( eosJMDCKP(9)
     &           + eosJMDCKP(10)*t1
     &           + eosJMDCKP(11)*t2
     &           )
     &    + p2*s1*( eosJMDCKP(12)
     &           + eosJMDCKP(13)*t1
     &           + eosJMDCKP(14)*t2
     &           )

         bulkMod = bMfresh + bMsalt + bMpres

C     density of sea water at pressure p
         rhoLoc = rhoP0/(1. _d 0 - p1/bulkMod)

      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN

         sp5 = SQRT(s1)

         p1   = pLoc*SItodBar
         p1t1 = p1*t1

         rhoNum = eosMDJWFnum(0)
     &        + t1*(eosMDJWFnum(1)
     &        +     t1*(eosMDJWFnum(2) + eosMDJWFnum(3)*t1) )
     &        + s1*(eosMDJWFnum(4)
     &        +     eosMDJWFnum(5)*t1  + eosMDJWFnum(6)*s1)
     &        + p1*(eosMDJWFnum(7) + eosMDJWFnum(8)*t2
     &        +     eosMDJWFnum(9)*s1
     &        +     p1*(eosMDJWFnum(10) + eosMDJWFnum(11)*t2) )


         den = eosMDJWFden(0)
     &        + t1*(eosMDJWFden(1)
     &        +     t1*(eosMDJWFden(2)
     &        +         t1*(eosMDJWFden(3) + t1*eosMDJWFden(4) ) ) )
     &        + s1*(eosMDJWFden(5)
     &        +     t1*(eosMDJWFden(6)
     &        +         eosMDJWFden(7)*t2)
     &        +     sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2) )
     &        + p1*(eosMDJWFden(10)
     &        +     p1t1*(eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1) )

         rhoDen = 1.0/(epsln+den)

         rhoLoc = rhoNum*rhoDen

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN
C
      ELSE
       WRITE(msgBuf,'(3A)')
     &        ' FIND_RHO_SCALAR : equationOfState = "',
     &        equationOfState,'"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R FIND_RHO_SCALAR'
      ENDIF

      RETURN
      END

C=================================================================

      _RL FUNCTION SW_PTMP  (S,T,P,PR)

c     ==================================================================
c     SUBROUTINE SW_PTMP
c     ==================================================================
c
c     o   Calculates potential temperature as per UNESCO 1983 report.
c
c     started:
c
c              Armin Koehl akoehl@ucsd.edu
c
c     ==================================================================
c     SUBROUTINE SW_PTMP
c     ==================================================================
C     S  = salinity    [psu      (PSS-78) ]
C     T  = temperature [degree C (IPTS-68)]
C     P  = pressure    [db]
C     PR = Reference pressure  [db]

      implicit none

c     routine arguments
      _RL S,T,P,PR

c     local arguments
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      parameter ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )

c     externals
      _RL sw_adtg
      external sw_adtg
c theta1
      del_P  = PR - P
      del_th = del_P*sw_adtg(S,T,P)
      th     = T + onehalf*del_th
      q      = del_th
c theta2
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

c theta3
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

c theta4
      del_th = del_P*sw_adtg(S,th,P+del_P)
      SW_PTMP     = th + (del_th - two*q)/(two*three)
      return
      end

C======================================================================

CBOP
C     !ROUTINE: SW_TEMP
C     !INTERFACE:
      _RL FUNCTION SW_TEMP( s, t, p, pr )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_TEMP
C     | o compute in-situ temperature from potential temperature
C     *=============================================================*
C
C     REFERENCES:
C     Fofonoff, P. and Millard, R.C. Jr
C     Unesco 1983. Algorithms for computation of fundamental properties of
C     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
C     Eqn.(31) p.39
C
C     Bryden, H. 1973.
C     "New Polynomials for thermal expansion, adiabatic temperature gradient
C     and potential temperature of sea water."
C     DEEP-SEA RES., 1973, Vol20,401-408.
C

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
CML#include "SIZE.h"
CML#include "EEPARAMS.h"
CML#include "PARAMS.h"
CML#include "GRID.h"
CML#include "DYNVARS.h"
CML#include "FFIELDS.h"
CML#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     s      :: salinity
C     t      :: potential temperature
C     p      :: pressure
c     pr     :: reference pressure
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  s, t, p, pr
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES
C     === Local variables ===
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      PARAMETER ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )

c     externals
      _RL sw_adtg
      EXTERNAL sw_adtg
c theta1
C--   here we swap P and PR in order to get in-situ temperature
C     del_P  = PR - P ! to get potential from in-situ temperature
      del_P  = P - PR ! to get in-situ from potential temperature
      del_th = del_P*sw_adtg(S,T,P)
      th     = T + onehalf*del_th
      q      = del_th
c theta2
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

c theta3
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

c theta4
      del_th = del_P*sw_adtg(S,th,P+del_P)
      SW_temp= th + (del_th - two*q)/(two*three)

      RETURN
      END

C======================================================================

      _RL FUNCTION SW_ADTG  (S,T,P)

c     ==================================================================
c     SUBROUTINE SW_ADTG
c     ==================================================================
c
c     o Calculates adiabatic temperature gradient as per UNESCO 1983 routines.
c
c     started:
c
c              Armin Koehl akoehl@ucsd.edu
c
c     ==================================================================
c     SUBROUTINE SW_ADTG
c     ==================================================================

      implicit none
      _RL a0,a1,a2,a3,b0,b1,c0,c1,c2,c3,d0,d1,e0,e1,e2
      _RL S,T,P
      _RL sref

      sref = 35. _d 0
      a0 =  3.5803 _d -5
      a1 = +8.5258 _d -6
      a2 = -6.836 _d -8
      a3 =  6.6228 _d -10

      b0 = +1.8932 _d -6
      b1 = -4.2393 _d -8

      c0 = +1.8741 _d -8
      c1 = -6.7795 _d -10
      c2 = +8.733 _d -12
      c3 = -5.4481 _d -14

      d0 = -1.1351 _d -10
      d1 =  2.7759 _d -12

      e0 = -4.6206 _d -13
      e1 = +1.8676 _d -14
      e2 = -2.1687 _d -16

      SW_ADTG =      a0 + (a1 + (a2 + a3*T)*T)*T
     &     + (b0 + b1*T)*(S-sref)
     &     + ( (c0 + (c1 + (c2 + c3*T)*T)*T) + (d0 + d1*T)*(S-sref) )*P
     &     + (  e0 + (e1 + e2*T)*T )*P*P
      return
      end
