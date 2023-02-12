
#include "ICEPLUME_OPTIONS.h"

CBOP
C !ROUTINE: ICEPLUME_READPARMS

      SUBROUTINE ICEPLUME_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize ICEPLUME parameters, read in data.iceplume

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ICEPLUME.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXF
# ifdef USE_EXF_INTERPOLATION
#  include "EXF_INTERP_SIZE.h"
#  include "SET_GRID.h"
# endif
#endif
# ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
c# include "W2_EXCH2_TOPOLOGY.h"
# endif /* ALLOW_EXCH2 */

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
C  bi,bj,i,j,k		:: loop indices

      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, j, i, k
#ifdef ALLOW_EXF
#ifdef USE_EXF_INTERPOLATION
      INTEGER inp_gNx,inp_gNy
#endif /* USE_EXF_INTERPOLATION */
#endif /* ALLOW_EXF */

CEOP
#ifdef ALLOW_ICEPLUME

C    PARMO1 - ICEPLUME inputs
      NAMELIST /ICEPLUME_PARM01/
     &     runoffQsgfile, plumeMaskFile, plumeLengthFile,
     &     T_sg_0, S_sg_0, E_0, Angle_sg_0, wVel_sg_0,
     &     RTOL, ATOL,
     &     IceTemp,
     &     usePlumeDiagnostics,
     &     conserveMass,
     &     c_i,
     &     lambda1,
     &     lambda2,
     &     lambda3,
     &     GamT,
     &     GamS,
     &     Cd,
     &     useSheetPlume,
     &     useConePlume,
     &     useTruncPlume,
     &     useBuoyPlume,
     &     slopeTmod,
     &     interceptTmod,
#ifdef ICEPLUME_ALLOW_SCHULZ22
     &     facGamSGamT,
     &     GamTconst,
     &     GamSconst,
     &     Lp,
#endif
     &     maxDepth,
     &     backgroundVelThresh
catn runoffVelfile, runoffRadfile, 

C    PARMO2 - iceplume inputs required for thermodynamics
catn change icefront to iceplume in case we ever run with icefront and
catn have duplicate definitions of variables. change tend to tendBG to
catn indicate these flags or fields are relevant for the background melt
      NAMELIST /ICEPLUME_PARM02/
     &     ICEPLUMElatentHeat, ICEPLUMEHeatCapacity_Cp,
     &     applyIcePlumeBGTendT,applyIcePlumeBGTendS

#ifdef ALLOW_PTRACERS
C    PARM03 - inputs relating to passive tracers
      NAMELIST /ICEPLUME_PARM03/
     &     ptracerMaskFile,
     &     useInputPtracers
#endif /* ALLOW_PTRACERS */ 

#ifdef ALLOW_EXF
      NAMELIST /ICEPLUME_EXF_PARM04/
     &     runoffQsgperiod,   runoffQsgStartTime,
     &     runoffQsgstartdate1, runoffQsgstartdate2,
     &     runoffQsgconst,      runoffQsg_inscal,
     &     runoffQsg_remov_intercept, runoffQsg_remov_slope
     &    ,runoffQsgRepCycle
#ifdef USE_EXF_INTERPOLATION
     &    ,runoffQsg_interpMethod
     &    ,runoffQsg_lon0,runoffQsg_lon_inc,runoffQsg_nlon
     &    ,runoffQsg_lat0,runoffQsg_lat_inc,runoffQsg_nlat
#endif /* USE_EXF_INTERPOLATION */
#endif /* ALLOW_EXF */

      IF ( .NOT.useICEPLUME ) THEN
C-    pkg ICEPLUME is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         ICEPLUMEisON = .FALSE.
C     print a (weak) warning if data.iceplume is found
         CALL PACKAGES_UNUSED_MSG( 'useICEPLUME', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      ICEPLUMEisON = .TRUE.

C     Default values for PARM01
      E_0      = 0.1D0
      T_sg_0   = 1.0D-3
      S_sg_0   = 1.0D-3
      Angle_sg_0= PI/twoRL
      wVel_sg_0= 1.0D0
      RTOL     = 1.0D-5
      ATOL     = 1.0D-5
      iceTemp  = 0.

catn      runoffVelfile = ' '
catn      runoffRadfile = ' '
#ifdef ALLOW_EXF
      runoffQsgstartdate1       = 0
      runoffQsgstartdate2       = 0
      runoffQsgStartTime        = UNSET_RL
C fieldperiod=0 means input file is one time-constant field
C fieldperiod=-12 means input file contains 12 monthly means
      runoffQsgperiod           = 0.0 _d 0
C     Repeat period for forcing fields (s)
C     For example, for yearly repeat period: repeatPeriod=31556925.
      runoffQsgRepCycle         = 0.0 _d 0
      runoffQsgconst            = 0.0 _d 0
      runoffQsg_remov_intercept = 0.0 _d 0
      runoffQsg_remov_slope     = 0.0 _d 0
      runoffQsg_inscal          = 1.0 _d 0
#ifdef USE_EXF_INTERPOLATION
c in exf_init_interp:
c inp_gNx = exch2_mydNx(1) or Nx
c inp_gNy = exch2_mydNy(1) or Ny
c inp_lon0 = xgOrigin + delX(1)*halfRL
c inp_lat0 = ygOrigin + delY(1)*halfRL
c inp_dLon = delX(1)
c      DO j=1,MAX_LAT_INC
c        IF (j.LT.inp_gNy) THEN
c          inp_dLat(j) = (delY(j) + delY(j+1))*halfRL
c        ELSE
c          inp_dLat(j) = 0.
c        ENDIF
c      ENDDO
#ifdef ALLOW_EXCH2
      inp_gNx = exch2_mydNx(1)
      inp_gNy = exch2_mydNy(1)
#else
      inp_gNx = Nx
      inp_gNy = Ny
#endif /* ALLOW_EXCH2 */

      runoffQsg_lon0            = xgOrigin + delX(1)*halfRL
      runoffQsg_lon_inc         = delX(1)
      runoffQsg_nlon            = inp_gNx
      runoffQsg_lat0            = ygOrigin + delY(1)*halfRL
      DO j=1,MAX_LAT_INC
        IF (j.LT.inp_gNy) THEN
          runoffQsg_lat_inc(j)  = (delY(j) + delY(j+1))*halfRL
        ELSE
          runoffQsg_lat_inc(j)  = 0.
        ENDIF
      ENDDO
      runoffQsg_nlat            = inp_gNy
catn default interpmethod=0 implies no interpolation
      runoffQsg_interpMethod    = 0
#endif /* USE_EXF_INTERPOLATION */
      runoffQsgfile = ' '
C-    internal parameters (not in namelist):
      runoffQsgmask = 'c'
#endif /* ALLOW_EXF */
c-
      plumeMaskFile = ' '
      plumeLengthFile = ' '
      usePlumeDiagnostics = .FALSE.
      conserveMass    = .FALSE.
      useSheetPlume   = .FALSE.
      useConePlume    = .FALSE.
      useTruncPlume   = .FALSE.
catn  Will use rhoConst instead of setting yet another rho_ref
catn  In addition, inside iceplume_calc.F there is another rho_0=1027.
catn  which will also now be set to rhoConst.
catn      rho_ref= 1020.D0
catn      rhoShelfIce = 917.D0
catn: Use default values for gravity and HeatCapacity_Cp
catn      g      = 9.81D0
catn      c_w    = 3994.D0
      c_i    = 2009.D0
catn: Should not redefine Latent heat of fusion, use mitgcm parms
catn      L      = 334000D0
      lambda1= -0.0573D0
      lambda2= 0.0832D0
      lambda3= 0.000761D0
      GamT   = 0.022
      GamS   = 0.00062
      backgroundVelThresh = 0.001
      maxDepth = UNSET_RL
      Cd     = 0.0025
      slopeTmod = 1.0
      interceptTmod = 0.0 
#ifdef ICEPLUME_ALLOW_SCHULZ22
catn set some values if using Schulz et al. GRL 2022:
c      useBuoyPlume    = .FALSE.
      facGamSGamT = 0.07
      GamTconst = 0.001
      GamSconst = facGamSGamT*GamTconst
      backgroundVelThresh = 0.05
      Lp = 220. _d 0
#endif

C Default values for PARMO2
       ICEPLUMElatentHeat       =  334.0 _d 3
       ICEPLUMEHeatCapacity_Cp  = 2000.0 _d 0
       applyIcePlumeBGTendT     = .TRUE.
       applyIcePlumeBGTendS     = .TRUE.

#ifdef ALLOW_PTRACERS
C Default values for PARM03
       ptracerMaskFile= ' '
       useInputPtracers = .FALSE.
#endif /* ALLOW_PTRACERS */

C Open and read the data.icefront file
      WRITE(msgBuf,'(A)') ' ICEPLUME_READPARMS: opening data.iceplume'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.iceplume', 'ICEPLUME_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=ICEPLUME_PARM01)
      READ(UNIT=iUnit,NML=ICEPLUME_PARM02)
#ifdef ALLOW_PTRACERS
      READ(UNIT=iUnit,NML=ICEPLUME_PARM03)
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
       WRITE(msgBuf,'(A)')
     &     ' ICEPLUME_READPARMS: reading ICEPLUME_EXF_PARM04'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       READ(UNIT=iUnit,NML=ICEPLUME_EXF_PARM04)
      ENDIF
#endif /* ALLOW_EXF */
      WRITE(msgBuf,'(A)')
     &  ' ICEPLUME_READPARMS: finished reading data.iceplume'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C Zero values for T_sg and S_sg can cause the routine to crash
      IF ( T_sg_0.EQ.0.D0 ) T_sg_0 = 1.0D-3
      IF ( S_sg_0.EQ.0.D0 ) S_sg_0 = 1.0D-3

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
C     and tiled OB indices to be set.
      _BARRIER

#endif /* ALLOW_ICEPLUME */
      RETURN
      END
