#include "SBO_OPTIONS.h"

      _RL FUNCTION SBO_RHO( DPT, LAT, S, T )
C     /==========================================================\
C     | _RL FUNCTION SBO_RHO                                     |
C     | o Compute density for SBO package.                       |
C     |==========================================================|
C     | CHECK VALUE:                                             |
C     | DPT=5000; LAT=30; S=30; T=30; SBO_RHO=1038.298           |
C     \==========================================================/
      IMPLICIT NONE

C     == Routine arguments ==
C     SBO_RHO - density (kg/m^3)
C     DPT     - depth (m)
C     LAT     - latitude north (deg)
C     S       - salinity (g/kg)
C     T       - potential temperature (deg C)

      _RL DPT,LAT,S,T

      _RL PLAT,D,C1,P,PR,Q,X,SR,V350P,B

      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )

C     First convert depth to pressure
C     Ref: Saunders, "Practical Conversion of Pressure to Depth",
C     J. Phys. Oceanog., April 1981.
C     CHECK VALUE: P80=7500.004 DBARS;FOR LAT=30 DEG., DEPTH=7321.45 METERS

      PLAT=abs(LAT*pi/180.)
      D=sin(PLAT)
      C1=5.92E-3+(D*D)*5.25E-3
      P=((1-C1)-sqrt(((1-C1)**2)-(8.84E-6*abs(DPT))))/4.42E-6

C     Second convert temperature from potential to in situ
C     REF: BRYDEN,H.,1973,DEEP-SEA RES.,20,401-408
C     FOFONOFF,N.,1977,DEEP-SEA RES.,24,489-491
C     CHECKVALUE: THETA= 36.89073 C,S=40 (IPSS-78),T0=40 DEG C,
C     P0=10000 DECIBARS,PR=0 DECIBARS

      PR = P
      P  = 0.
      Q = PR*((((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P+
     &     ((2.7759E-12*T-1.1351E-10)*(S-35.0)+
     &     ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T+
     &     1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*(S-35.0)+
     &     ((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5)

      T = T + 0.5*Q
      P = P + 0.5*PR
      x = PR*((((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P+
     &     ((2.7759E-12*T-1.1351E-10)*(S-35.0)+
     &     ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T+
     &     1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*(S-35.0)+
     &     ((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5)

      T = T + 0.29289322*(x-Q)
      Q = 0.58578644*x + 0.121320344*Q
      x = PR*((((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P+
     &     ((2.7759E-12*T-1.1351E-10)*(S-35.0)+
     &     ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T+
     &     1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*(S-35.0)+
     &     ((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5)

      T = T + 1.707106781*(x-Q)
      Q = 3.414213562*x - 4.121320344*Q
      P = P + 0.5*PR
      x = PR*((((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P+
     &     ((2.7759E-12*T-1.1351E-10)*(S-35.0)+
     &     ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T+
     &     1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*(S-35.0)+
     &     ((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5)
      T = T + (x-2.0*Q)/6.0

C     Third compute density
C     BASED ON 1980 EQUATION
C     OF STATE FOR SEAWATER AND 1978 PRACTICAL SALINITY SCALE.
C     REFERENCES
C     MILLERO, ET AL (1980) DEEP-SEA RES.,27A,255-264
C     MILLERO AND POISSON 1981,DEEP-SEA RES.,28A PP 625-629.
C     BOTH ABOVE REFERENCES ARE ALSO FOUND IN UNESCO REPORT 38 (1981)
C     CHECK VALUE: SIGMA = 59.82037  KG/M**3 FOR S = 40 (IPSS-78) ,
C     T = 40 DEG C, P0= 10000 DECIBARS.

C     CONVERT PRESSURE TO BARS AND TAKE SQUARE ROOT SALINITY.
      P=P/10.
      SR = sqrt(abs(S))

C     INTERNATIONAL ONE-ATMOSPHERE EQUATION OF STATE OF SEAWATER
      x = (4.8314E-4 * S +
     &     ((-1.6546E-6*T+1.0227E-4)*T-5.72466E-3) * SR +
     &     (((5.3875E-9*T-8.2467E-7)*T+7.6438E-5)*T-4.0899E-3)*T
     &     +8.24493E-1)*S + ((((6.536332E-9*T-1.120083E-6)
     &     *T+1.001685E-4)*T-9.095290E-3)*T+6.793952E-2)*T-28.263737

C     SPECIFIC VOLUME AT ATMOSPHERIC PRESSURE
      V350P = 1.0/1028.1063
      x = -x*V350P/(1028.1063+x)

C     COMPUTE COMPRESSION TERMS
      SR = ((((9.1697E-10*T+2.0816E-8)*T-9.9348E-7) * S +
     &     (5.2787E-8*T-6.12293E-6)*T+3.47718E-5) *P +
     &     (1.91075E-4 * SR + (-1.6078E-6*T-1.0981E-5)*T+2.2838E-3) *
     &     S + ((-5.77905E-7*T+1.16092E-4)*T+1.43713E-3)*T-0.1194975)
     &     *P + (((-5.3009E-4*T+1.6483E-2)*T+7.944E-2) * SR +
     &     ((-6.1670E-5*T+1.09987E-2)*T-0.603459)*T+54.6746) * S +
     &     (((-5.155288E-5*T+1.360477E-2)*T-2.327105)*T+148.4206)*T -
     &     1930.06

C     EVALUATE PRESSURE POLYNOMIAL
      B  = (5.03217E-5*P+3.359406)*P+21582.27
      x = x*(1.0 - P/B) + (V350P+x)*P*SR/(B*(B+SR))
      SR = V350P*(1.0 - P/B)
      SBO_RHO = 1028.106331 + P/B/SR - x / (SR*(SR+x))

      RETURN
      END
