c----------------------------------------------------------------------------
c.. Common blocks for almost everything that the sea ice model passes around.
c----------------------------------------------------------------------------
c.. number layers of ice  
      integer nlyr        ! maximum number of ice layers
      _RL  rnlyr          ! real value of nlyr
      parameter (nlyr = 2)
      parameter (rnlyr = 2.)

c.. densities
      _RL  rhoa                      ! density of air (kg/m^3)
      _RL  rhos                      ! density of snow (kg/m^3)
      _RL  rhoi                      ! density of ice (kg/m^3)
      _RL  rhosw                     ! density of seawater (kg/m^3)
      _RL  rhoiw                     ! ocean-ice density difference (kg/m^3)

      parameter (rhoa =1.3)
      parameter (rhos = 330.)
      parameter (rhoi = 900.)
      parameter (rhosw = 1026.)
      parameter (rhoiw = rhosw - rhoi)

c.. specific heats
      _RL  cpair          ! specific heat of air (J/kg/K)
      _RL  cpice          ! specific heat of fresh ice (J/kg/K)
      _RL  cpwater        ! specific heat of water (J/kg/K)
      _RL  cpwv           ! specific heat of water vapour (J/kg/KC)
      _RL  cpvir

      parameter(cpair = 1004.)
      parameter(cpice = 2106.)
      parameter (cpwater = 4218.)
      parameter (cpwv = 1.81e3)
      parameter (cpvir = cpwv/cpair -1.)

c .. thermal conductivity. QQ check units
      _RL  kice           ! thermal conductivity of pure ice (W/m/K)
      _RL  ksnow          ! thermal conductivity of snow (W/m/K)

      parameter (kice  = 2.03)  !QQ originally 2.03)
      parameter (ksnow = 0.30)  !QQ originally 0.30)

c .. heat transfer coefficient
      _RL transcoef       ! transfer coef between ice and water (unitless)
 
      parameter (transcoef=0.006)  !QQ originally 0.006

c .. latent heat
      _RL  Lvap           ! latent heat of vaporizn at 0 C (J/kg)
      _RL  Lfresh         ! latent heat of melting of pure ice (J/kg)

      parameter(Lvap = 2.5e+6) 
      parameter (Lfresh = 3.34e5)

c.. Enthalpy
      _RL  qsnow            ! snow enthalpy (J/kg)
      parameter (qsnow = Lfresh)

c .. Albedo
      _RL  albsnodry                 ! albedo of dry snow (Tsfc < 0)
      _RL  albsnowet                 ! albedo of melting snow (Tsfc = 0)
      _RL  albicemax                 ! max albedo of bare ice
      _RL  albicemin                 ! minimum ice albedo (thin melting ice)
      _RL  halb                      ! melt rate parameter for albedo QQ units

      parameter (albsnodry = 0.85)
      parameter (albsnowet = 0.75)
      parameter (albicemax = 0.65)
      parameter (albicemin = 0.20)
      parameter (halb = .5)

c.. Solar parameters
      _RL  i0             ! fraction of penetrating solar rad
      _RL  ksolar         ! bulk solar abs coeff of sea ice (m-1)

      parameter (i0 = 0.3)        !QQQQQQ was 0.3    
      parameter (ksolar = 1.5)    !QQQQQQ was1.5


c .. Salinity
      _RL  saltice                   ! salinity of ice (o/oo)
      _RL  S_winton                  ! winton salinity of ice (o/oo)
      _RL  mu_Tf                     ! Tf:brine salinity ratio (C/ppt)

      parameter (saltice  = 4.)
      parameter (S_winton = 1.)
      parameter (mu_Tf = 0.054)

c .. melting
      _RL  Tf0kel         ! Freezing temp of fresh ice in Kelvin = 273.15
      _RL  Tmlt1                     ! melting temp; depends on S (C)
 
      parameter (Tf0kel = 273.15)
      parameter (Tmlt1=-mu_Tf*S_winton)

c .. wind drag
c     cdrag_[n]    - n = 1,2,3 coefficients used to evaluate
c                    drag coefficient
      _RL cdrag_1,    cdrag_2,     cdrag_3 
       parameter ( cdrag_1     =       0.0027000,
     &             cdrag_2     =       0.0001420,
     &             cdrag_3     =       0.0000764)

c .. constants
      _RL  stefan         ! Stefan-Boltzmann constant (W/m^2 K^4)
      _RL  xkar           ! Von Karman constant  QQ units?
      _RL  Rvap           ! gas constant for H2O vapor (J/kg/K)

      parameter (stefan = 5.67e-8)
      parameter (xkar =  0.4 ) 
      parameter(Rvap = 461.)

c.. Miscellaneous
      _RL  p0             ! surface pressure (mb)

      parameter(p0 = 1013.)

c.. Combinations used for efficiency
      _RL  rhoi_Lfresh    ! rhoi*Lfresh
      _RL  Qcoef          ! another constant for latent heat flux
      _RL  lvrrv          ! (Lvap + Lfresh) / Rvap

      parameter (rhoi_Lfresh = rhoi*Lfresh)
      parameter (Qcoef = 6.11*0.622/p0)
      parameter (lvrrv = (Lvap + Lfresh) / Rvap)

c .. Min/Max
      _RL  himin          ! minimum thickness for ice (m)
      _RL  Terrmax        ! temperature convergence precision (C)

      parameter (himin = 0.01)    !was 0.01?
      parameter (Terrmax = 5.0e-1) !was 5.0e-3????

c .. for bulk formula
      _RL  humid_fac       ! const. for the evaluation of the virtual temp.
      _RL  saltsat         ! reduction of sat. vapor pressure over salt water
      _RL  gamma_blk       ! adiabatic lapse rate


      parameter (humid_fac   =  0.606)
      parameter (saltsat     =  0.980)
      parameter (gamma_blk   =  0.010)
    


c .. for Vince bulk formula QQ check units
      _RL Lvap_ice
      _RL Rgas
      _RL Sha

      parameter (Lvap_ice  = 2.83e6)   ! latent heat from sublimation
      parameter (Rgas      = 287.e0)   ! gas constant for dry air
      parameter (Sha       = Rgas/.286) !

c .. emissivities   QQQ find
      _RL atm_emissivity
      _RL ocean_emissivity
      _RL snow_emissivity
      _RL ice_emissivity
 
cQQQQQQ find real values
      parameter(atm_emissivity=.90d0)     !QQ
      parameter(ocean_emissivity=.985d0) !QQ
      parameter(snow_emissivity=.98d0) !QQ
      parameter(ice_emissivity=.98d0) !QQ
