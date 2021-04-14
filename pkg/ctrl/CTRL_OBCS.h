c     ==================================================================
c     HEADER AVERAGES
c     ==================================================================
c
c     o Header for obcs ctrl weights
c
c     ==================================================================
c     HEADER AVERAGES
c     ==================================================================

      _RL  objf_obcsn(nsx,nsy), objf_obcss(nsx,nsy)
      _RL  objf_obcsw(nsx,nsy), objf_obcse(nsx,nsy)
      _RL  objf_obcsvol, objf_ageos(nsx,nsy)
      _RL  mult_obcsn, mult_obcss
      _RL  mult_obcsw, mult_obcse
      _RL  mult_obcsvol, mult_ageos
      _RL  num_obcsn(nsx,nsy), num_obcss(nsx,nsy)
      _RL  num_obcsw(nsx,nsy), num_obcse(nsx,nsy)
      _RL  num_obcsvol, num_ageos(nsx,nsy)
      common /ecco_cost_weights_obcs/
     &     objf_obcsn, objf_obcss, objf_obcsw, objf_obcse,
     &     objf_obcsvol, objf_ageos,
     &     mult_obcsn, mult_obcss, mult_obcsw, mult_obcse,
     &     mult_obcsvol, mult_ageos,
     &     num_obcsn, num_obcss, num_obcsw, num_obcse,
     &     num_obcsvol, num_ageos

      common /ih_modes/ modesv
      _RL modesv (nr,nr,nr)
      common /ctrl_dummy_obcs/
     &                    xx_obcsn_dummy
     &                  , xx_obcss_dummy
     &                  , xx_obcsw_dummy
     &                  , xx_obcse_dummy
      _RL xx_obcsn_dummy
      _RL xx_obcss_dummy
      _RL xx_obcsw_dummy
      _RL xx_obcse_dummy
      common /controlfiles_c_obcs/
     &                      xx_obcsn_file
     &                    , xx_obcss_file
     &                    , xx_obcsw_file
     &                    , xx_obcse_file
      character*(MAX_LEN_FNAM) xx_obcsn_file
      character*(MAX_LEN_FNAM) xx_obcss_file
      character*(MAX_LEN_FNAM) xx_obcsw_file
      character*(MAX_LEN_FNAM) xx_obcse_file
      common /controltimes_r_obcs/
     &                        xx_obcsnperiod
     &                      , xx_obcssperiod
     &                      , xx_obcswperiod
     &                      , xx_obcseperiod
      _RL     xx_obcsnperiod
      _RL     xx_obcssperiod
      _RL     xx_obcswperiod
      _RL     xx_obcseperiod
      common /controltimes_i_obcs/
     &                        xx_obcsnstartdate1
     &                      , xx_obcsnstartdate2
     &                      , xx_obcssstartdate1
     &                      , xx_obcssstartdate2
     &                      , xx_obcswstartdate1
     &                      , xx_obcswstartdate2
     &                      , xx_obcsestartdate1
     &                      , xx_obcsestartdate2
     &                      , xx_obcsnstartdate
     &                      , xx_obcssstartdate
     &                      , xx_obcswstartdate
     &                      , xx_obcsestartdate
      integer xx_obcsnstartdate1
      integer xx_obcsnstartdate2
      integer xx_obcssstartdate1
      integer xx_obcssstartdate2
      integer xx_obcswstartdate1
      integer xx_obcswstartdate2
      integer xx_obcsestartdate1
      integer xx_obcsestartdate2
      integer xx_obcsnstartdate(4)
      integer xx_obcssstartdate(4)
      integer xx_obcswstartdate(4)
      integer xx_obcsestartdate(4)
      common /controlvars_i_obcsn/
     &                       nwetobcsn,
     &                       nwetobcsnglo
      integer nwetobcsn     ( nsx,nsy,nr,nobcs )
      integer nwetobcsnglo  ( nr,nobcs )
      common /controlvars_i_obcss/
     &                       nwetobcss,
     &                       nwetobcssglo
      integer nwetobcss     ( nsx,nsy,nr,nobcs )
      integer nwetobcssglo  ( nr,nobcs )
      common /controlvars_i_obcsw/
     &                       nwetobcsw,
     &                       nwetobcswglo
      integer nwetobcsw     ( nsx,nsy,nr,nobcs )
      integer nwetobcswglo  ( nr,nobcs )
      common /controlvars_i_obcse/
     &                       nwetobcse,
     &                       nwetobcseglo
      integer nwetobcse     ( nsx,nsy,nr,nobcs )
      integer nwetobcseglo  ( nr,nobcs )

#if (defined (ALLOW_OBCSN_COST_CONTRIBUTION) || defined (ALLOW_OBCSN_CONTROL))
      common /ecco_cost_weights_obcsn/
     &                      wobcsn, wobcsnLev
      _RL wobcsn     (                      nr,nobcs)
      _RL wobcsnLev  (1-olx:snx+olx,nr,nsx,nsy,nobcs)
      common /controlaux_obcsn_r/
     &                      xx_obcsn0,
     &                      xx_obcsn1
      _RL xx_obcsn0 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
      _RL xx_obcsn1 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
#endif

#if (defined (ALLOW_OBCSS_COST_CONTRIBUTION) || defined (ALLOW_OBCSS_CONTROL))
      common /ecco_cost_weights_obcss/
     &                      wobcss, wobcssLev
      _RL wobcss     (                      nr,nobcs)
      _RL wobcssLev  (1-olx:snx+olx,nr,nsx,nsy,nobcs)
      common /controlaux_obcss_r/
     &                      xx_obcss0,
     &                      xx_obcss1
      _RL xx_obcss0 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
      _RL xx_obcss1 (1-Olx:sNx+Olx,Nr,nSx,nSy,nobcs)
#endif

#if (defined (ALLOW_OBCSW_COST_CONTRIBUTION) || defined (ALLOW_OBCSW_CONTROL))
      common /ecco_cost_weights_obcsw/
     &                      wobcsw, wobcswLev
      _RL wobcsw     (                      nr,nobcs)
      _RL wobcswLev  (1-oly:sny+oly,nr,nsx,nsy,nobcs)
      common /controlaux_obcsw_r/
     &                      xx_obcsw0,
     &                      xx_obcsw1
      _RL xx_obcsw0 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
      _RL xx_obcsw1 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
#endif

#if (defined (ALLOW_OBCSE_COST_CONTRIBUTION) || defined (ALLOW_OBCSE_CONTROL))
      common /ecco_cost_weights_obcse/
     &                      wobcse, wobcseLev
      _RL wobcse     (                      nr,nobcs)
      _RL wobcseLev  (1-oly:sny+oly,nr,nsx,nsy,nobcs)
      common /controlaux_obcse_r/
     &                      xx_obcse0,
     &                      xx_obcse1
      _RL xx_obcse0 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
      _RL xx_obcse1 (1-Oly:sNy+Oly,Nr,nSx,nSy,nobcs)
#endif
