c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_constants.h,v 1.1 2001/05/14 22:08:40 heimbach Exp $
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
     &              exf_half =  0.5,
     &              exf_one  =  1.0,
     &              exf_two  =  2.0
     &         )


      real       exf_undef
      parameter( exf_undef = -9000. )

c     2. physical constants

#ifdef ALLOW_ATM_TEMP
c     is identical to "gravity" used in MITgcmUV
c     needs to be marmonized through common constants.h file
      _RL     gravity_mks
      parameter (gravity_mks = 9.81D0)
#endif

c     3. empirical parameters
c        climsaltfreeze dummy not relevant

      _RL         climtempfreeze
      parameter ( climtempfreeze = -1.9 )

      _RL         climsaltfreeze
      parameter ( climsaltfreeze = -999.9 )

#ifdef ALLOW_BULKFORMULAE

c     atmrho       - mean atmospheric density [kg/(m*3)]
c     atmcp        - mean atmospheric specific heat [ ? ]
c     flamb        - latent heat of evaporation [ ? ]
c     cdrag_[n]    - n = 1,2,3 coefficients used to evaluate
c                    drag coefficient
c     cstanton_[n] - n = 1,2   coefficients used to evaluate
c                    the Stanton number (stable/unstable cond.)
c     dalton       - coefficient used to evaluate the Dalton number
c     umin         - minimum absolute wind speed used to evaluate
c                    drag coefficient [m/s]
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

      _RL atmrho,     atmcp
      _RL flamb
      _RL cdrag_1,    cdrag_2,     cdrag_3
      _RL cstanton_1, cstanton_2
      _RL cdalton
      _RL umin
      _RL zolmin
      _RL zref
      _RL karman
      _RL cvapor_fac, cvapor_exp
      _RL humid_fac
      _RL gamma_blk
      _RL saltsat
      _RL psim_fac
      _RL cen2kel
      _RL hu
      _RL ht
      _RL hq

      parameter ( cdrag_1     =       0.0027000,
     &            cdrag_2     =       0.0001420,
     &            cdrag_3     =       0.0000764,
     &            cstanton_1  =       0.0327000,
     &            cstanton_2  =       0.0180000,
     &            cdalton     =       0.0346000,
     &            atmrho      =       1.200,
     &            atmcp       =    1005.000,
     &            flamb       = 2500000.000,
     &            umin        =       0.500,
     &            zolmin      =    -100.000,
     &            zref        =      10.000,
     &            karman      =       0.400,
     &            cvapor_fac  =  640380.000,
     &            cvapor_exp  =    5107.400,
     &            humid_fac   =       0.606,
     &            gamma_blk   =       0.010,
     &            saltsat     =       0.980,
     &            psim_fac    =       5.000,
     &            cen2kel     =     273.150,
     &            hu          =      10.000,
     &            ht          =       2.000,
     &            hq          =       2.000
     &          )


#ifndef ALLOW_ATM_WIND
#ifdef  ALLOW_ATM_TEMP
c     To invert the relationship ustar = ustar(umagn) the following
c     parameterization is used:
c  
c       ustar**2 = umagn**2 * CDN(umagn)
c  
c                   / cquadrag_1 * umagn**2 + cquadrag_2;   0 < u < 11 m/s
c       CDN(umagn) =
c                   \ clindrag_1 * umagn + clindrag_2   ;   u > 11 m/s
c  
c       clindrag_[n] - n = 1, 2 coefficients used to evaluate
c                      LINEAR relationship of Large and Pond 1981
c       cquadrag_[n] - n = 1, 2 coefficients used to evaluate
c                      quadratic relationship
c       u11          - u = 11 m/s wind speed
c       ustofu11     - ustar = 0.3818 m/s, corresponding to u = 11 m/s

      _RL clindrag_1, clindrag_2
      _RL cquadrag_1, cquadrag_2
      _RL u11
      _RL ustofu11

      parameter (
     &            ustofu11    =         0.381800 ,
     &            u11         =        11.       ,
     &            clindrag_1  =         0.000065 ,
     &            clindrag_2  =         0.000490 ,
     &            cquadrag_1  = clindrag_1/u11/2 ,
     &            cquadrag_2  = clindrag_1*u11/2 + clindrag_2
     &          )
#endif
#endif

#ifdef ALLOW_ATM_TEMP
      _RL         czol
      parameter ( czol = hu*karman*gravity_mks )
#endif

#endif

c     4. control parameters


#ifdef ALLOW_BULKFORMULAE

c     niter_bulk   - Number of iterations to be performed for the
c                    evaluation of the bulk surface fluxes. The ncom
c                    model uses 2 hardwired interation steps (loop
c                    unrolled).
c
      integer     niter_bulk
      parameter ( niter_bulk = 2 )

#endif

