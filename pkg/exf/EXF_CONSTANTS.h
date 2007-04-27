C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_CONSTANTS.h,v 1.2 2007/04/27 15:27:38 jmc Exp $
C $Name:  $
c
c
c     ==================================================================
c     HEADER exf_constants
c     ==================================================================
c
c     o Header file for constants.
c       These include  - numbers (e.g. 1, 2, 1/2, ...)
c                      - physical constants (e.g. gravitational const.)
c                      - empirical parameters
c                      - control parameters (e.g. max. no of iteration)
c
c     started: Patrick Heimbach heimbach@mit.edu  06-May-2000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     HEADER exf_constants
c     ==================================================================

c     1. numbers

c     exf_half   0.5
c     exf_one    1.0
c     exf_two    2.0

      _RL exf_half
      _RL exf_one
      _RL exf_two

      parameter(
     &              exf_half =  0.5d0 ,
     &              exf_one  =  1.0d0 ,
     &              exf_two  =  2.0d0
     &         )

      real       exf_undef
      parameter( exf_undef = -9000. )

c     2. physical constants

c     Stefan-Boltzmann constant [J*K^-4*m^-2s^-1]
c     sigma = (2*pi^5*k^4)/(15*h^3*c^2)
      _RL stefanBoltzmann
      parameter ( stefanBoltzmann = 5.670D-8 )

c#ifdef ALLOW_ATM_TEMP
c     is identical to "gravity" used in MITgcmUV
c     needs to be marmonized through common constants.h file
      _RL         gravity_mks
      parameter ( gravity_mks = 9.81d0 )
c#endif

c     3. empirical parameters

c     umin         - minimum absolute wind speed used to evaluate
c                    drag coefficient [m/s]
c     atmrho       - mean atmospheric density [kg/(m*3)]
      _RL umin
      _RL atmrho
      parameter ( umin           = 0.5 _d 0 
     &          , atmrho         =        1.200    d0 )

c     To invert the relationship ustar = ustar(umagn) the following
c     parameterization is used:
c  
c      ustar**2 = umagn**2 * CDN(umagn)
c  
c                  / cquadrag_1 * umagn**2 + cquadrag_2; 0 < u < 11 m/s
c      CDN(umagn) =
c                  \ clindrag_1 * umagn + clindrag_2   ; u > 11 m/s
c  
c      clindrag_[n] - n = 1, 2 coefficients used to evaluate
c                     LINEAR relationship of Large and Pond 1981
c      cquadrag_[n] - n = 1, 2 coefficients used to evaluate
c                     quadratic relationship
c      u11          - u = 11 m/s wind speed
c      ustofu11     - ustar = 0.3818 m/s, corresponding to u = 11 m/s

      _RL clindrag_1, clindrag_2
      _RL cquadrag_1, cquadrag_2
      _RL u11
      _RL ustofu11

      parameter (
     &            ustofu11    =         0.381800d0 ,
     &            u11         =        11.      d0 ,
     &            clindrag_1  =         0.000065d0 ,
     &            clindrag_2  =         0.000490d0 ,
     &            cquadrag_1  = clindrag_1/u11/2 ,
     &            cquadrag_2  = clindrag_1*u11/2 + clindrag_2
     &          )

c#ifdef ALLOW_BULKFORMULAE

c     atmcp        - mean atmospheric specific heat [J/kg/deg K]
c     flamb        - latent heat of evaporation [J/kg]
C     flami        - latent heat of melting of pure ice [J/kg]
c     cdrag_[n]    - n = 1,2,3 coefficients used to evaluate
c                    drag coefficient
c     cstanton_[n] - n = 1,2   coefficients used to evaluate
c                    the Stanton number (stable/unstable cond.)
c     dalton       - coefficient used to evaluate the Dalton number
c     zolmin       - minimum stability parameter
c     zref         - reference height
c     
c     karman       - von Karman constant
c     cvapor       - see e.g. Gill (1982) p.41 Eq. (3.1.15)
c     humid_fac    - constant entering the evaluation of the virtual
c                    temperature
c     gamma_blk    - adiabatic lapse rate
c     saltsat      - reduction of saturation vapor pressure over salt water
c     psim_fac     - 
c     cen2kel      - conversion of deg. Centigrade to Kelvin
c     hu           - height of mean wind
c     ht           - height of mean temperature
c     hq           - height of mean rel. humidity

      _RL atmcp
      _RL flamb, flami
      _RL cdrag_1,    cdrag_2,     cdrag_3
      _RL cstanton_1, cstanton_2
      _RL cdalton
      _RL zolmin
      _RL zref
      _RL karman
      _RL cvapor_fac, cvapor_exp
      _RL cvapor_fac_ice, cvapor_exp_ice
      _RL humid_fac
      _RL gamma_blk
      _RL saltsat
      _RL psim_fac
      _RL cen2kel
      _RL hu
      _RL ht
      _RL hq

      parameter ( cdrag_1        =        0.0027000d0 ,
     &            cdrag_2        =        0.0001420d0 ,
     &            cdrag_3        =        0.0000764d0 ,
     &            cstanton_1     =        0.0327000d0 ,
     &            cstanton_2     =        0.0180000d0 ,
     &            cdalton        =        0.0346000d0 ,
     &            atmcp          =     1005.000    d0 ,
     &            flamb          =  2500000.000    d0 ,
     &            flami          =   334000.000    d0 ,
     &            zolmin         =     -100.000    d0 ,
     &            zref           =       10.000    d0 ,
     &            karman         =        0.400    d0 ,
     &            cvapor_fac     =   640380.000    d0 ,
     &            cvapor_exp     =     5107.400    d0 ,
     &            cvapor_fac_ice = 11637800.000    d0 ,
     &            cvapor_exp_ice =     5897.800    d0 ,
     &            humid_fac   =           0.606    d0 ,
     &            gamma_blk   =           0.010    d0 ,
     &            saltsat     =           0.980    d0 ,
     &            psim_fac    =           5.000    d0 ,
     &            cen2kel     =         273.150    d0 ,
     &            hu          =          10.000    d0 ,
     &            ht          =           2.000    d0 ,
     &            hq          =           2.000    d0
     &          )

c#ifdef ALLOW_ATM_TEMP
      _RL         czol
      parameter ( czol = hu*karman*gravity_mks )
c#endif

c     4. control parameters

c     niter_bulk   - Number of iterations to be performed for the
c                    evaluation of the bulk surface fluxes. The ncom
c                    model uses 2 hardwired interation steps (loop
c                    unrolled).
c
      integer     niter_bulk
      parameter ( niter_bulk = 2 )

c#endif /* ALLOW_BULKFORMULAE */
