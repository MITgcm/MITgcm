#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_READPARMS
C     !INTERFACE:
      SUBROUTINE GGL90_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GGL90_READPARMS                               |
C     | o Routine to read in file data.ggl90                     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instance of GGL90_READPARMS
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !FUNCTIONS:
C     === Functions ===
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error message buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit, iL
C     retired variables
      INTEGER nRetired
      _RL GGL90taveFreq

C--   GGL90 vertical mixing parameters
      NAMELIST /GGL90_PARM01/
     &     GGL90dumpFreq, GGL90taveFreq,
     &     GGL90diffTKEh,
     &     GGL90mixingMaps, GGL90writeState,
     &     GGL90ck, GGL90ceps, GGL90alpha, GGL90m2,
     &     GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom,
     &     GGL90mixingLengthMin, mxlMaxFlag, mxlSurfFlag,
     &     GGL90viscMax, GGL90diffMax, GGL90TKEFile,
     &     GGL90_dirichlet, calcMeanVertShear, useIDEMIX

#ifdef ALLOW_GGL90_IDEMIX
c-----------------------------------------------------------------------
c  IDEMIX
c-----------------------------------------------------------------------
      NAMELIST /GGL90_PARM02/
     & IDEMIX_tau_v, IDEMIX_tau_h, IDEMIX_gamma, IDEMIX_jstar,
     & IDEMIX_mu0,IDEMIX_tidal_file,IDEMIX_wind_file,
     & IDEMIX_mixing_efficiency, IDEMIX_diff_max,
     & IDEMIX_diff_min, IDEMIX_frac_F_b, IDEMIX_frac_F_s,
     & IDEMIX_include_GM,IDEMIX_include_GM_bottom
#endif /* ALLOW_GGL90_IDEMIX */
CEOP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGGL90 ) THEN
C-    pkg GGL90 is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ggl90 is found
         CALL PACKAGES_UNUSED_MSG( 'useGGL90', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' GGL90_READPARMS: opening data.ggl90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      errIO = 0
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.ggl90', 'GGL90_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default TKE vertical mixing parameters
      GGL90dumpFreq        = dumpFreq
      GGL90taveFreq        = UNSET_RL
      GGL90mixingMaps      = .FALSE.
      GGL90writeState      = .FALSE.
      GGL90ck              = 0.1 _d 0
      GGL90ceps            = 0.7 _d 0
      GGL90alpha           = 1.0 _d 0
C     Blanke and Delecluse (1993, JPO) use
      GGL90m2              = 3.75 _d 0
      GGL90TKEmin          = 1.0 _d -11
C     Blanke and Delecluse (1993, JPO) use
      GGL90TKEsurfMin      = 1.0 _d -04
      GGL90TKEbottom       = UNSET_RL
      GGL90viscMax         = 1. _d 2
      GGL90diffMax         = 1. _d 2
      GGL90diffTKEh        = 0.0 _d 0
      GGL90mixingLengthMin = 1.0 _d -08
      mxlMaxFlag           = 0
      mxlSurfFlag          = .FALSE.
      GGL90TKEFile         = ' '
      GGL90_dirichlet      = .TRUE.
      calcMeanVertShear    = .FALSE.
      useIDEMIX            = .FALSE.

#ifdef ALLOW_GGL90_IDEMIX
C-----------------------------------------------------------------------
C     set default parameter for IDEMIX
C-----------------------------------------------------------------------
C     New (and different from Olbers and Eden, 20130) parameter default
C     values for tau_v, jstar, mu0 according to Pollmann et al. (2017):
      IDEMIX_tau_v      =  2.0*86400.0 _d 0
      IDEMIX_tau_h      = 10.0*86400.0 _d 0
      IDEMIX_gamma      = 1.57 _d 0
      IDEMIX_jstar      = 5.0 _d 0
      IDEMIX_mu0        = 1.0 _d 0/3.0 _d 0
      IDEMIX_mixing_efficiency = 0.1666 _d 0
      IDEMIX_diff_max   = 1.0 _d 0
      IDEMIX_diff_min   = 1.0 _d -9
      IDEMIX_frac_F_b   = 1.0 _d 0
C     only approx. 20% of near inertial wind power input reaches IDEMIX
      IDEMIX_frac_F_s   = 0.2 _d 0
C     IDEMIX_tidal_file = 'tidal_energy.bin'
C     IDEMIX_wind_file  = 'wind_energy.bin'
      IDEMIX_tidal_file = ' '
      IDEMIX_wind_file  = ' '
      IDEMIX_include_GM = .FALSE.
      IDEMIX_include_GM_bottom = .FALSE.
#endif /* ALLOW_GGL90_IDEMIX */

C-----------------------------------------------------------------------
C define some non-dimensional constants and
C the vertical mixing coefficients in m-k-s units
C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.ggl90".
      WRITE(msgBuf,'(A)')
     &  ' GGL90_READPARMS ; starts to read GGL90_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ( UNIT=iUnit, NML=GGL90_PARM01 )
c     READ( UNIT=iUnit, NML=GGL90_PARM01, IOSTAT=errIO )
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R GGL90_READPARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.ggl90"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist GGL90_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      WRITE(msgBuf,'(A)')
     &     ' GGL90_READPARMS: read GGL90_PARM01 : OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_GGL90_IDEMIX
      IF (useIDEMIX) THEN
       WRITE(msgBuf,'(A)')
     &  ' GGL90_READPARMS ; starts to read GGL90_PARM02'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       READ( UNIT=iUnit, NML=GGL90_PARM02 )
c      READ( UNIT=iUnit, NML=GGL90_PARM02, IOSTAT=errIO )
       IF ( errIO .LT. 0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R GGL90_READPARMS'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'Error reading numerical model '
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'parameter file "data.ggl90"'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &   'Problem in namelist GGL90_PARM02'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R GGL90_READPARMS'
       ENDIF
       WRITE(msgBuf,'(A)')
     &     ' GGL90_READPARMS: read GGL90_PARM02 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      WRITE(msgBuf,'(A)')
     &     ' GGL90_READPARMS: finished reading data.ggl90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_GGL90_IDEMIX
      IF ( useIDEMIX ) THEN
       IF ( OLx .LT. 3 .or. OLy .lt. 3) THEN
        WRITE(msgBuf,'(A)') 'OLx/OLy must be greater than 2'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R GGL90_READPARMS'
       ENDIF
      ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

C Now set-up any remaining parameters that result from the input parameters
      IF ( GGL90TKEbottom .EQ. UNSET_RL ) THEN
       GGL90TKEbottom = GGL90TKEmin
      ENDIF
#ifdef ALLOW_GGL90_IDEMIX
      IF ( GGL90TKEmin .LT. 0. ) THEN   ! CE: changed le to lt  !!!
#else
      IF ( GGL90TKEmin .LE. 0. ) THEN
#endif
       WRITE(msgBuf,'(A)')
     &      'GGL90TKEmin must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90TKEbottom .LT. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &      'GGL90TKEbottom must not be less than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90mixingLengthMin .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &      'GGL90mixingLengthMin must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90viscMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'GGL90viscMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90diffMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'GGL90diffMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF

C-    Retired parameters
      nRetired = 0
      IF ( GGL90taveFreq .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R GGL90_READPARMS: "GGL90taveFreq" ',
     &  'is no longer allowed in file "data.ggl90"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R GGL90_READPARMS: use diagnostics variables instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R GGL90_READPARMS: ',
     &  'Error reading parameter file "data.ggl90"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'some out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF

C--   print TKE vertical mixing parameters to stdout for better debugging
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// GGL90 configuration'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_RL( GGL90dumpFreq, INDEX_NONE,'GGL90dumpFreq =',
     &'   /* GGL90 state write out interval ( s ). */')
      CALL WRITE_0D_RL( GGL90taveFreq, INDEX_NONE,'GGL90taveFreq =',
     &'   /* GGL90 averaging interval ( s ). */')
      CALL WRITE_0D_L(GGL90mixingMaps,INDEX_NONE,
     &     'GGL90mixingMAPS =', '   /* GGL90 IO flag */')
      CALL WRITE_0D_L(GGL90writeState,INDEX_NONE,
     &     'GGL90writeState =', '   /* GGL90 IO flag */')
      CALL WRITE_0D_RL( GGL90ck, INDEX_NONE,'GGL90ck =',
     &'   /* GGL90 viscosity parameter */')
      CALL WRITE_0D_RL( GGL90ceps, INDEX_NONE,'GGL90ceps =',
     &'   /* GGL90 dissipation parameter */')
      CALL WRITE_0D_RL( GGL90alpha, INDEX_NONE,'GGL90alpha =',
     &'   /* GGL90 TKE diffusivity parameter */')
      CALL WRITE_0D_RL( GGL90m2, INDEX_NONE,'GGL90m2 =',
     &'   /* GGL90 wind stress to vertical stress ratio */')
      CALL WRITE_0D_RL( GGL90TKEmin, INDEX_NONE,'GGL90TKEmin =',
     &'   /* GGL90 minimum kinetic energy ( m^2/s^2 ) */')
      CALL WRITE_0D_RL( GGL90TKEsurfMin, INDEX_NONE,
     &     'GGL90TKEsurfMin =',
     &'   /* GGL90 minimum surface kinetic energy ( m^2/s^2 ) */')
      CALL WRITE_0D_RL( GGL90TKEbottom, INDEX_NONE,
     &     'GGL90TKEbottom =',
     &     '   /* GGL90 bottom kinetic energy ( m^2/s^2 ) */')
      CALL WRITE_0D_RL( GGL90viscMax, INDEX_NONE,'GGL90viscMax =',
     &     '   /* GGL90 upper limit for viscosity (m^2/s ) */')
      CALL WRITE_0D_RL( GGL90diffMax, INDEX_NONE,'GGL90diffMax =',
     &     '   /* GGL90 upper limit for diffusivity (m^2/s ) */')
      CALL WRITE_0D_RL( GGL90diffTKEh, INDEX_NONE,'GGL90diffTKEh =',
     &     '   /* GGL90 horizontal diffusivity for TKE ( m^2/s ) */')
      CALL WRITE_0D_RL( GGL90mixingLengthMin, INDEX_NONE,
     &     'GGL90mixingLengthMin =',
     &     '   /* GGL90 minimum mixing length (m) */')
      CALL WRITE_0D_I(mxlMaxFlag, INDEX_NONE, 'mxlMaxFlag =',
     &     '   /* Flag for limiting mixing-length method */')
      CALL WRITE_0D_L(mxlSurfFlag,INDEX_NONE,
     &     'mxlSurfFlag =',
     &     '   /* GGL90 flag for near surface mixing */')
      CALL WRITE_0D_L( calcMeanVertShear, INDEX_NONE,
     &     'calcMeanVertShear =',
     &     ' /* calc Mean of Vert.Shear (vs shear of Mean flow) */')
      iL = MAX_LEN_MBUF - 22
      iL = MIN( iL, MAX(ILNBLNK(GGL90TKEFile),1) )
      WRITE(msgBuf,'(A,A)')'GGL90: GGL90TKEFile = ',GGL90TKEFile(1:iL)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL WRITE_0D_L(GGL90_dirichlet,INDEX_NONE,
     & 'GGL90_dirichlet =', '   /* GGL90 Boundary condition flag */')
#ifdef ALLOW_GGL90_IDEMIX
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      CALL WRITE_0D_L(useIDEMIX,INDEX_NONE, 'useIDEMIX =',
     &     '   /* turn on IDEMIX contribution. */')
C
      IF ( useIDEMIX ) THEN
       CALL WRITE_0D_RL( IDEMIX_tau_v, INDEX_NONE,'IDEMIX_tau_v =',
     &     '   /* IDEMIX vertical group speed parameter. */')
       CALL WRITE_0D_RL( IDEMIX_tau_h, INDEX_NONE,'IDEMIX_tau_h =',
     &     '   /* IDEMIX horizontal group speed parameter. */')
       CALL WRITE_0D_RL( IDEMIX_gamma, INDEX_NONE,'IDEMIX_gamma =',
     &     '   /* IDEMIX group speed parameter. */')
       CALL WRITE_0D_RL( IDEMIX_jstar, INDEX_NONE,'IDEMIX_jstar =',
     &     '   /* IDEMIX baroclinic mode bandwidth. */')
       CALL WRITE_0D_RL( IDEMIX_mu0, INDEX_NONE,'IDEMIX_mu0 =',
     &     '   /* IDEMIX dissipation parameter. */')
       CALL WRITE_0D_RL( IDEMIX_mixing_efficiency,INDEX_NONE,
     &     'IDEMIX_mixing_efficiency =',
     &     '   /* IDEMIX mixing efficiency. */')
       CALL WRITE_0D_RL( IDEMIX_diff_max,INDEX_NONE,'IDEMIX_diff_max =',
     &     '   /* IDEMIX maximal diffusivity. */')
       CALL WRITE_0D_RL( IDEMIX_diff_min,INDEX_NONE,'IDEMIX_diff_min =',
     &     '   /* IDEMIX minimal diffusivity. */')
       CALL WRITE_0D_RL( IDEMIX_frac_F_b,INDEX_NONE,'IDEMIX_frac_F_b =',
     &     '   /* Fraction of F_b which enters IW field. */')
       CALL WRITE_0D_RL( IDEMIX_frac_F_s,INDEX_NONE,'IDEMIX_frac_F_s =',
     &     '   /* Fraction of F_s which enters IW field. */')
       CALL WRITE_0D_L(IDEMIX_include_GM,INDEX_NONE,
     & 'IDEMIX_include_GM =', '   /* IDEMIX GM flag */')
       CALL WRITE_0D_L(IDEMIX_include_GM_bottom,INDEX_NONE,
     & 'IDEMIX_include_GM_bottom =', '   /* IDEMIX GM flag */')
C
       CALL WRITE_0D_C( IDEMIX_tidal_file, -1, INDEX_NONE,
     &  'IDEMIX_tidal_file =', ' /* file name of tidal energy field */')
       CALL WRITE_0D_C( IDEMIX_wind_file, -1, INDEX_NONE,
     &  'IDEMIX_wind_file  =', ' /* file name of wind energy field */')
      ENDIF
#endif /* ALLOW_GGL90_IDEMIX */
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of GGL90 config. summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_GGL90 */

      RETURN
      END
