#include "ICEBERG_OPTIONS.h"

CBOP
C !ROUTINE: ICEBERG_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE ICEBERG_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize ICEBERG parameters, read in data.iceberg

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_ICEBERG

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /ICEBERG_PARM01/
     &     brg_Rho,
     &     brg_iceTemp,
     &     brg_uMin,
     &     brg_lambda1,
     &     brg_lambda2,
     &     brg_lambda3,
     &     brg_GamT,
     &     brg_GamS,
     &     brg_c_w,
     &     brg_c_i,
     &     brg_L,
     &     brg_Cd,
     &     brg_SelectDrag,  
     &     brg_DragForm,    
     &     brg_SelectFill,
     &     ICEBERGmaskFile, ICEBERGmeltFile, ICEBERGmaskNumsFile,
     &     ICEBERGlengthFile, ICEBERGwidthsFile, ICEBERGdepthsFile,
     &     ICEBERGnumPerCellFile, ICEBERGdriftFile,
     &     ICEBERGbarrierFile, ICEBERGopenFracFile,
     &     ICEBERGareaFile

#ifdef ALLOW_PTRACERS
C    PARM02 - inputs relating to passive tracers
      NAMELIST /ICEBERG_PARM02/
     &     brg_ptracerMaskFile,
     &     brg_useInputPtracers
#endif /* ALLOW_PTRACERS */ 

C Set defaults values for parameters in ICEBERG.h
      brg_Rho                   =  917.0 _d 0
      brg_iceTemp               = .0 _d 0
      brg_uMin                  = 0.06
      brg_lambda1               = -0.0573D0
      brg_lambda2               = 0.0832D0
      brg_lambda3               = 0.000761D0
      brg_GamT                  = 0.022
      brg_GamS                  = 0.00062
      brg_c_w                   = 3974.D0
      brg_c_i                   = 2000.D0
      brg_L                     = 334000D0
      brg_Cd                    = 0.0025
      brg_DragForm              = 0.0025
      brg_SelectDrag            = 3 
      brg_SelectFill            = 3 
      ICEBERGmaskFile              = ' '
      ICEBERGmeltFile           = ' '
      ICEBERGmaskNumsFile          = ' '
      ICEBERGlengthFile            = ' '
      ICEBERGwidthsFile            = ' '
      ICEBERGdepthsFile            = ' '
      ICEBERGnumPerCellFile     = ' '
      ICEBERGdriftFile          = ' '
      ICEBERGbarrierFile        = ' '
      ICEBERGopenFracFile       = ' '
      ICEBERGareaFile           = ' '

#ifdef ALLOW_PTRACERS
C Default values for PARM02
       brg_ptracerMaskFile= ' '
       brg_useInputPtracers = 0
#endif /* ALLOW_PTRACERS */

C Open and read the data.iceberg file
      WRITE(msgBuf,'(A)') ' ICEBERG_READPARMS: opening data.iceberg'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.iceberg', 'ICEBERG_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=ICEBERG_PARM01)
#ifdef ALLOW_PTRACERS
      READ(UNIT=iUnit,NML=ICEBERG_PARM02)
#endif /* ALLOW_PTRACERS */
      WRITE(msgBuf,'(A)')
     &  ' ICEBERG_READPARMS: finished reading data.iceberg'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
C Close the open data file
      CLOSE(iUnit)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ICEBERG */

      RETURN
      END
