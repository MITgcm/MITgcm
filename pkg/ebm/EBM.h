C $Header: /u/gcmpack/MITgcm/pkg/ebm/EBM.h,v 1.3 2004/05/21 21:45:35 heimbach Exp $
C $Name:  $
C
C     CountX - number of ocean points in each latitude band
C     ZonalMeanSST - zonal mean sea surface temperature
C     TmlS   - Average mid-latitude temperature in the 
c              Southern Hemisphere
C     TmlN   - Northern
C     TtS    - Average tropical temperature in the Southern Hemisphere
C     TtN    -                                     Northern
C     winPert - weather  patterns added to the background wind.
      COMMON /EBM_FLD_RL/      
     &               ZonalMeanSST, CountX,
     &               TmlN, TmlS, TtN, TtS
      _RL ZonalMeanSST(1-OLy:sNy+OLy,nSy)
      _RL CountX(1-OLy:sNy+OLy,nSy)
      _RL TmlS, TmlN, TtS, TtN

      COMMON /EBM_FLD_RS/            
     &               Run,
     &               winPert
      _RS Run      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS winPert  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /EBM_PARAM_C/
     &               RunoffFile
      CHARACTER*(MAX_LEN_FNAM) RunoffFile

      COMMON /EBM_PARAM_RL/
     &               tauThetaZonRelax,
     &               lambdaThetaZonRelax,
     &               scale_runoff
      _RL tauThetaZonRelax
      _RL lambdaThetaZonRelax
      _RL scale_runoff

C     Constant parameters
      _RL t_mlt, lv, cp, rho_air
      PARAMETER (t_mlt = 273.15, lv = 2.5e6, cp = 1004., rho_air = 1.27)
C     sin(lat) and Legendre polynomials
cph We will make these three (i,j) arrays to
cph avoid AD recomputations
      _RL S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
      _RL P2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
      _RL P4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSy)
C     Shortwave and albedo parameters
      _RL SW(1-OLy:sNy+OLy,nSy)
      _RL Q0, Q2, A0, A2, A4
      PARAMETER ( Q0 = 1365., Q2 = -0.482) 
      PARAMETER ( A0 = 0.322, A2 = 0.231, A4 = 0.086 ) 
C     Longwave parameters
      _RL LW(1-OLy:sNy+OLy,nSy)
      _RL LW0, LW1
      PARAMETER ( LW0 = 195., LW1 = 2.78 )
C     Heat transport parameters
      _RL Hd(1-OLy:sNy+OLy,nSy), Hd35(2)
      _RL H1, H3, H5
      PARAMETER ( H1 = 3.866, H3 = -2.851, H5 = -1.016 )
C     Freshwater flux parameters
      _RL Fw(1-OLy:sNy+OLy,nSy), Fw35(2)
      _RL F1, F2, F3, F4, F5
      PARAMETER ( F1 = 2.092, F2 = 5.796, F3 = 8.472, 
     &     F4 = 7.728, F5 = 2.362 )
C     Temperature parameterization
      _RL T(1-OLy:sNy+OLy,nSy)
      _RL T_var(4), T0(2), T2(2), T35(2), DTDy35(2)
C     Parameters used to calculate the transport efficiency
      _RL Cl, Cf, Cs, C
      _RL gamma, kappa, De
      _RL trans_eff, Hw, Nw, Tw, At, dz, htil, tau
      PARAMETER (trans_eff = 2.0, Hw = 8.e3, Nw = 0.013, 
     &     Tw = 300., At = 3., dz = 450., htil = 0.8, tau = 5.e-3)
C     Climate change parameter (W/(m^2 y) )
      _RL DLW
      PARAMETER (DLW = 0.06)
C     Latitude boundaries used
      _RL lat(3)
      DATA lat / 0.0,  35.0,  85.0 /

