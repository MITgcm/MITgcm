C
C     store directives for checkpoint level 1 AFTER surface forcing
C     has been read
C
C     We store surface forcing fields --- if required --- to avoid
C     calling load_fields_driver in forward_step_ad.  Many of these
C     directives are only necessary in specific cases, for example with
C     pkg/seaice of pkg/shelfice, but since there are many potential
C     cases where these stores may become necessary, we just add them
C     here for all forcing fields under the sun. TAF will pick the
C     correct directives as they become necessary.
C
CADJ STORE gcmSST     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE sst        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE sss        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE qnet       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE empmr      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltflux   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE fu, fv     = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef SHORTWAVE_HEATING
CADJ STORE qsw        = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE pload      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ALLOW_EXF
CADJ STORE uwind      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE vwind      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE wspeed     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE sh         = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hflux      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE sflux      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE ustress    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE vstress    = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_ATM_TEMP
CADJ STORE atemp      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE aqh        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hs         = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hl         = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE lwflux     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE evap       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE precip     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE snowprecip = comlev1, key = ikey_dynamics, kind = isbyte
#   ifdef SHORTWAVE_HEATING
CADJ STORE swflux     = comlev1, key = ikey_dynamics, kind = isbyte
#   endif
#  endif
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE lwdown     = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure  = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ALLOW_RUNOFF
CADJ STORE runoff     = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ALLOW_RUNOFTEMP
CADJ STORE runoftemp  = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#ifdef ALLOW_SALTFLX
CADJ STORE saltflx    = comlev1, key = ikey_dynamics, kind = isbyte
#endif
#  ifdef EXF_SEAICE_FRACTION
CADJ STORE exf_iceFraction=comlev1,key=ikey_dynamics, kind = isbyte
#  endif
#  ifdef EXF_ALLOW_TIDES
CADJ STORE tidePot    = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ALLOW_CLIMSST_RELAXATION
CADJ STORE climsst    = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ALLOW_CLIMSSS_RELAXATION
CADJ STORE climsss    = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifdef ALLOW_CLIMSTRESS_RELAXATION
CADJ STORE climustr   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE climvstr   = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
# endif /* ALLOW_EXF */
#ifdef ALLOW_GEOTHERMAL_FLUX
CADJ STORE geothermalFlux = comlev1, key=ikey_dynamics, kind=isbyte
#endif
#if ( defined ALLOW_SHELFICE || defined ALLOW_STEEP_ICECAVITY )
CADJ STORE addMass    = comlev1, key=ikey_dynamics, kind=isbyte
#endif
# ifdef ALLOW_BLING
CADJ STORE wind       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE atmosP     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE silica     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE fIce       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE inputFe    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE apco2      = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_BLING */
# ifdef ALLOW_DIC
CADJ STORE wind       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE atmosP     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE silicaSurf = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE fIce       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE inputFe    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE PAR        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE CHL        = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_DIC */
# ifdef ALLOW_CFC
CADJ STORE pisVel     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE atmosP     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE fIce       = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_CFC */
# ifdef ALLOW_CTRL
#  ifdef ALLOW_GENTIM2D_CONTROL
C     in some cases this also requires storing the surface control
C     variables because they are used by packages, e.g. shelfice
CADJ STORE xx_gentim2d = comlev1, key = ikey_dynamics, kind = isbyte
#  else
C     there is no equivalent to xx_gentim2d for deprecated code so we just
C     omit it here
#  endif
# endif /* ALLOW_CTRL */
