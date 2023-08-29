CBOP
C     !ROUTINE: CTRL_OBCS.h
C     !INTERFACE:
C     #include "CTRL_OBCS.h"

C     !DESCRIPTION:
C     *================================================================*
C     | CTRL_OBCS.h
C     | o Header file for OBCS Control and related weights
C     *================================================================*
CEOP

C-- IMPORTANT NOTE: The declaration of parameter "nobcs" has been moved
C   from SIZE.h. If you encounter compile time errors related to "nobcs",
C   make sure that your SIZE.h does not contain any declaration of this
C   parameter.
C--
      INTEGER nobcs
      PARAMETER ( nobcs = 4 )

      _RL  objf_obcsn(nSx,nSy), objf_obcss(nSx,nSy)
      _RL  objf_obcsw(nSx,nSy), objf_obcse(nSx,nSy)
      _RL  objf_obcsvol, objf_ageos(nSx,nSy)
      _RL  mult_obcsn, mult_obcss
      _RL  mult_obcsw, mult_obcse
      _RL  mult_obcsvol, mult_ageos
      _RL  num_obcsn(nSx,nSy), num_obcss(nSx,nSy)
      _RL  num_obcsw(nSx,nSy), num_obcse(nSx,nSy)
      _RL  num_obcsvol, num_ageos(nSx,nSy)
      COMMON /ecco_cost_weights_obcs/
     &     objf_obcsn, objf_obcss, objf_obcsw, objf_obcse,
     &     objf_obcsvol, objf_ageos,
     &     mult_obcsn, mult_obcss, mult_obcsw, mult_obcse,
     &     mult_obcsvol, mult_ageos,
     &     num_obcsn, num_obcss, num_obcsw, num_obcse,
     &     num_obcsvol, num_ageos

      COMMON /ih_modes/ modesv
      _RL modesv (Nr,Nr,Nr)
      COMMON /ctrl_dummy_obcs/
     &                    xx_obcsn_dummy,
     &                    xx_obcss_dummy,
     &                    xx_obcsw_dummy,
     &                    xx_obcse_dummy
      _RL xx_obcsn_dummy
      _RL xx_obcss_dummy
      _RL xx_obcsw_dummy
      _RL xx_obcse_dummy
      COMMON /controlfiles_c_obcs/
     &                      xx_obcsn_file,
     &                      xx_obcss_file,
     &                      xx_obcsw_file,
     &                      xx_obcse_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsn_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcss_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsw_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcse_file
      COMMON /controltimes_r_obcs/
     &                        xx_obcsnperiod,
     &                        xx_obcssperiod,
     &                        xx_obcswperiod,
     &                        xx_obcseperiod
      _RL     xx_obcsnperiod
      _RL     xx_obcssperiod
      _RL     xx_obcswperiod
      _RL     xx_obcseperiod
      COMMON /controltimes_i_obcs/
     &                        xx_obcsnstartdate1,
     &                        xx_obcsnstartdate2,
     &                        xx_obcssstartdate1,
     &                        xx_obcssstartdate2,
     &                        xx_obcswstartdate1,
     &                        xx_obcswstartdate2,
     &                        xx_obcsestartdate1,
     &                        xx_obcsestartdate2,
     &                        xx_obcsnstartdate,
     &                        xx_obcssstartdate,
     &                        xx_obcswstartdate,
     &                        xx_obcsestartdate
      INTEGER xx_obcsnstartdate1
      INTEGER xx_obcsnstartdate2
      INTEGER xx_obcssstartdate1
      INTEGER xx_obcssstartdate2
      INTEGER xx_obcswstartdate1
      INTEGER xx_obcswstartdate2
      INTEGER xx_obcsestartdate1
      INTEGER xx_obcsestartdate2
      INTEGER xx_obcsnstartdate(4)
      INTEGER xx_obcssstartdate(4)
      INTEGER xx_obcswstartdate(4)
      INTEGER xx_obcsestartdate(4)
      COMMON /controlvars_i_obcsn/
     &                       nwetobcsn,
     &                       nwetobcsnglo
      INTEGER nwetobcsn     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcsnglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcss/
     &                       nwetobcss,
     &                       nwetobcssglo
      INTEGER nwetobcss     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcssglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcsw/
     &                       nwetobcsw,
     &                       nwetobcswglo
      INTEGER nwetobcsw     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcswglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcse/
     &                       nwetobcse,
     &                       nwetobcseglo
      INTEGER nwetobcse     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcseglo  ( Nr,nobcs )

C     This is moved from ecco_local_params.h, because it is the only
C     parameter used (by obcs ctrl parameters)
      COMMON /ecco_data_errfile/
     &     data_errfile
      CHARACTER*(MAX_LEN_FNAM) data_errfile

#if ( defined ALLOW_OBCSN_COST_CONTRIBUTION || defined ALLOW_OBCSN_CONTROL )
      COMMON /ecco_cost_weights_obcsn/
     &                      wobcsn, wobcsnLev
      _RL wobcsn     (                      Nr,nobcs)
      _RL wobcsnLev  (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      COMMON /controlaux_obcsn_r/
     &                      xx_obcsn0,
     &                      xx_obcsn1
      _RL xx_obcsn0 (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      _RL xx_obcsn1 (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
#endif

#if ( defined ALLOW_OBCSS_COST_CONTRIBUTION || defined ALLOW_OBCSS_CONTROL )
      COMMON /ecco_cost_weights_obcss/
     &                      wobcss, wobcssLev
      _RL wobcss     (                      Nr,nobcs)
      _RL wobcssLev  (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      COMMON /controlaux_obcss_r/
     &                      xx_obcss0,
     &                      xx_obcss1
      _RL xx_obcss0 (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      _RL xx_obcss1 (1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
#endif

#if ( defined ALLOW_OBCSW_COST_CONTRIBUTION || defined ALLOW_OBCSW_CONTROL )
      COMMON /ecco_cost_weights_obcsw/
     &                      wobcsw, wobcswLev
      _RL wobcsw     (                      Nr,nobcs)
      _RL wobcswLev  (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      COMMON /controlaux_obcsw_r/
     &                      xx_obcsw0,
     &                      xx_obcsw1
      _RL xx_obcsw0 (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL xx_obcsw1 (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
#endif

#if ( defined ALLOW_OBCSE_COST_CONTRIBUTION || defined ALLOW_OBCSE_CONTROL )
      COMMON /ecco_cost_weights_obcse/
     &                      wobcse, wobcseLev
      _RL wobcse     (                      Nr,nobcs)
      _RL wobcseLev  (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      COMMON /controlaux_obcse_r/
     &                      xx_obcse0,
     &                      xx_obcse1
      _RL xx_obcse0 (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL xx_obcse1 (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
#endif
