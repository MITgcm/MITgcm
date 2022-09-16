
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

CEOP
#ifdef ALLOW_ICEPLUME

C    PARMO1 - ICEPLUME inputs
      NAMELIST /ICEPLUME_PARM01/
     &     runoffVelfile, runoffRadfile, plumeMaskFile,
     &     T_sg, S_sg, E_0, w_sg, r_sg, theta_sg,
     &     RTOL, ATOL,
     &     IceTemp,
     &     usePlumeDiagnostics,
     &     conserveMass,
     &     rho_ref,
     &     g,
     &     c_w,
     &     c_i,
     &     L,
     &     lambda1,
     &     lambda2,
     &     lambda3,
     &     GamT,
     &     GamS,
     &     Cd,
     &     useSheetPlume,
     &     useConePlume,
     &     maxDepth,
     &     backgroundVel

C    PARMO2 - icefront inputs required for thermodynamics
      NAMELIST /ICEPLUME_PARM02/
     &     ICEFRONTlatentHeat, ICEFRONTHeatCapacity_Cp,
     &     applyIcefrontTendT,applyIcefrontTendS

#ifdef ALLOW_PTRACERS
C    PARM03 - inputs relating to passive tracers
      NAMELIST /ICEPLUME_PARM03/
     &     ptracerMaskFile,
     &     useInputPtracers
#endif /* ALLOW_PTRACERS */ 

      _BEGIN_MASTER(myThid)

C     Default values for PARM01
      E_0      = 0.1D0
      T_sg     = 1.0D-3
      S_sg     = 1.0D-3
      w_sg     = 1.0D-2
      r_sg     = 1.0D-2
      theta_sg = PI/2.
      RTOL     = 1.0D-5
      ATOL     = 1.0D-5
      iceTemp  = 0.
      runoffVelfile = ' '
      runoffRadfile = ' '
      plumeMaskFile = ' '
      usePlumeDiagnostics = .FALSE.
      conserveMass    = .FALSE.
      useSheetPlume   = .FALSE.
      useConePlume    = .FALSE.
      rho_ref= 1020.D0
      g      = 9.81D0
      c_w    = 3994.D0
      c_i    = 2009.D0
      L      = 334000D0
      lambda1= -0.0573D0
      lambda2= 0.0832D0
      lambda3= 0.000761D0
      GamT   = 0.022
      GamS   = 0.00062
      Cd     = 0.0025
      backgroundVel = 0.001

C Default values for PARMO2
       ICEFRONTlatentHeat       =  334.0 _d 3
       ICEFRONTHeatCapacity_Cp  = 2000.0 _d 0
       applyIcefrontTendT       = .TRUE.
       applyIcefrontTendS       = .TRUE.

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
      WRITE(msgBuf,'(A)')
     &  ' ICEPLUME_READPARMS: finished reading data.iceplume'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )

C Close the open data file
      CLOSE(iUnit)

C Zero values for T_sg and S_sg can cause the routine to crash
      IF ( T_sg.EQ.0.D0 ) T_sg = 1.0D-3
      IF ( S_sg.EQ.0.D0 ) S_sg = 1.0D-3

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
C     and tiled OB indices to be set.
      _BARRIER

#endif /* ALLOW_ICEPLUME */
      RETURN
      END
