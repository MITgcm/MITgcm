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

#ifdef ALLOW_COST
      _RL  objf_obcsE(nSx,nSy), objf_obcsW(nSx,nSy)
      _RL  objf_obcsN(nSx,nSy), objf_obcsS(nSx,nSy)
      _RL  objf_obcsvol, objf_ageos(nSx,nSy)
      _RL  num_obcsE(nSx,nSy), num_obcsW(nSx,nSy)
      _RL  num_obcsN(nSx,nSy), num_obcsS(nSx,nSy)
      _RL  num_obcsvol, num_ageos(nSx,nSy)
      COMMON /ctrl_cost_weights_obcs/
     &     objf_obcsE, objf_obcsW, objf_obcsN, objf_obcsS,
     &     objf_obcsvol, objf_ageos,
     &     num_obcsE, num_obcsW, num_obcsN, num_obcsS,
     &     num_obcsvol, num_ageos

      COMMON /ctrl_cost_mult_obcs/
     &     mult_obcsE, mult_obcsW, mult_obcsN, mult_obcsS,
     &     mult_obcsvol, mult_ageos
      _RL  mult_obcsE, mult_obcsW
      _RL  mult_obcsN, mult_obcsS
      _RL  mult_obcsvol, mult_ageos
#endif

      COMMON /ih_modes/ modesv
      _RL modesv (Nr,Nr,Nr)
      COMMON /ctrl_dummy_obcs/
     &                    xx_obcsE_dummy,
     &                    xx_obcsW_dummy,
     &                    xx_obcsN_dummy,
     &                    xx_obcsS_dummy
      _RL xx_obcsE_dummy
      _RL xx_obcsW_dummy
      _RL xx_obcsN_dummy
      _RL xx_obcsS_dummy
      COMMON /controlfiles_c_obcs/
     &                      xx_obcsE_file,
     &                      xx_obcsW_file,
     &                      xx_obcsN_file,
     &                      xx_obcsS_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsE_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsW_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsN_file
      CHARACTER*(MAX_LEN_FNAM) xx_obcsS_file
      COMMON /controltimes_r_obcs/
     &                        xx_obcsEperiod,
     &                        xx_obcsWperiod,
     &                        xx_obcsNperiod,
     &                        xx_obcsSperiod
      _RL     xx_obcsEperiod
      _RL     xx_obcsWperiod
      _RL     xx_obcsNperiod
      _RL     xx_obcsSperiod
      COMMON /controltimes_i_obcs/
     &                        xx_obcsEstartdate1,
     &                        xx_obcsEstartdate2,
     &                        xx_obcsWstartdate1,
     &                        xx_obcsWstartdate2,
     &                        xx_obcsNstartdate1,
     &                        xx_obcsNstartdate2,
     &                        xx_obcsSstartdate1,
     &                        xx_obcsSstartdate2,
     &                        xx_obcsEstartdate,
     &                        xx_obcsWstartdate,
     &                        xx_obcsNstartdate,
     &                        xx_obcsSstartdate
      INTEGER xx_obcsEstartdate1
      INTEGER xx_obcsEstartdate2
      INTEGER xx_obcsWstartdate1
      INTEGER xx_obcsWstartdate2
      INTEGER xx_obcsNstartdate1
      INTEGER xx_obcsNstartdate2
      INTEGER xx_obcsSstartdate1
      INTEGER xx_obcsSstartdate2
      INTEGER xx_obcsEstartdate(4)
      INTEGER xx_obcsWstartdate(4)
      INTEGER xx_obcsNstartdate(4)
      INTEGER xx_obcsSstartdate(4)

      COMMON /controlvars_i_obcsE/
     &                       nwetobcsE,
     &                       nwetobcsEglo
      INTEGER nwetobcsE     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcsEglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcsW/
     &                       nwetobcsW,
     &                       nwetobcsWglo
      INTEGER nwetobcsW     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcsWglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcsN/
     &                       nwetobcsN,
     &                       nwetobcsNglo
      INTEGER nwetobcsN     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcsNglo  ( Nr,nobcs )
      COMMON /controlvars_i_obcsS/
     &                       nwetobcsS,
     &                       nwetobcsSglo
      INTEGER nwetobcsS     ( nSx,nSy,Nr,nobcs )
      INTEGER nwetobcsSglo  ( Nr,nobcs )

C     This is moved from ecco_local_params.h, because it is the only
C     parameter used (by obcs ctrl parameters)
      COMMON /obcs_data_errfile/
     &     obcs_data_errfile
      CHARACTER*(MAX_LEN_FNAM) obcs_data_errfile

#ifdef ALLOW_OBCSE_CONTROL
      COMMON /ctrl_cost_weights_obcsE/
     &     wobcsE
      _RL wobcsE   (                      Nr,nobcs)
      COMMON /controlaux_obcsE_r/
     &                      xx_obcsE0,
     &                      xx_obcsE1
      _RL xx_obcsE0(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL xx_obcsE1(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
#endif

#ifdef ALLOW_OBCSW_CONTROL
      COMMON /ctrl_cost_weights_obcsW/
     &       wobcsW
      _RL wobcsW   (                      Nr,nobcs)
      COMMON /controlaux_obcsW_r/
     &                      xx_obcsW0,
     &                      xx_obcsW1
      _RL xx_obcsW0(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL xx_obcsW1(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
#endif

#ifdef ALLOW_OBCSN_CONTROL
      COMMON /ctrl_cost_weights_obcsN/
     &       wobcsN
      _RL wobcsN   (                      Nr,nobcs)
      COMMON /controlaux_obcsN_r/
     &                      xx_obcsN0,
     &                      xx_obcsN1
      _RL xx_obcsN0(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      _RL xx_obcsN1(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
#endif

#ifdef ALLOW_OBCSS_CONTROL
      COMMON /ctrl_cost_weights_obcsS/
     &       wobcsS
      _RL wobcsS   (                      Nr,nobcs)
      COMMON /controlaux_obcsS_r/
     &                      xx_obcsS0,
     &                      xx_obcsS1
      _RL xx_obcsS0(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      _RL xx_obcsS1(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
#endif

#ifdef ALLOW_OBCS_CONTROL
# ifdef ALLOW_OBCS_WEIGHTS2D
C     These fields are not used anywhere in the code so they are not
C     defined by default.
      COMMON /ctrl_cost_weights2d/
     &     wobcsELev, wobcsWLev, wobcsNLev, wobcsSLev
      _RL wobcsELev(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL wobcsWLev(1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL wobcsNLev(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
      _RL wobcsSLev(1-OLx:sNx+OLx,Nr,nSx,nSy,nobcs)
# endif
#endif
