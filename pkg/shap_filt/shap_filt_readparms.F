C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_readparms.F,v 1.5 2001/12/11 14:31:59 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE SHAP_FILT_READPARMS                           |
C     | o Routine to initialize Shapiro Filter parameters        |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHAP_FILT.h"

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

      NAMELIST /SHAP_PARM01/
     & Shap_funct, shap_filt_uvStar, shap_filt_TrStagg,
     &    nShapT,  nShapTrPhys, Shap_Trtau, Shap_TrLength,
     &    nShapUV, nShapUVPhys, Shap_uvtau, Shap_uvLength

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   SHAP_FILT_READPARMS has been called so we know that
C     the package is active.
c     SHAPIsOn=.TRUE.

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' SHAP_FILT_READPARMS: opening data.shap'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.shap', 'SHAP_FILT_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default flags and values for Shapiro Filter
      Shap_funct = 2
      shap_filt_uvStar =.TRUE. 
      shap_filt_TrStagg=.TRUE.
      nShapT = 0
      nShapUV = 0
      nShapTrPhys = 0
      nShapUVPhys = 0
      Shap_Trtau = deltaTtracer
      Shap_TrLength = 0.
      Shap_uvtau = deltaTMom
      Shap_TrLength = 0.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=SHAP_PARM01)

      WRITE(msgBuf,'(A)') 
     &   ' SHAP_FILT_READPARMS: finished reading data.shap'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      write(*,*) 'Shap_funct, nShap_Tr,UV _Phys=',
     &    Shap_funct, nShapT, nShapUV, nShapTrPhys, nShapUVPhys
      write(*,*) 'Shap_Trtau,Shap_uvtau=',Shap_Trtau,Shap_uvtau
C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Check the parameters :
 
       IF ( .NOT.shap_filt_uvStar ) THEN
 
C- Notes: applying the filter at the end of the time step (after SOLVE_FOR_P)
C    affects the barotropic flow divergence ; this might not be consistent
C    with some option of the code.
 
        IF ( rigidLid ) THEN
         WRITE(msgBuf,'(2A)') 'SHAP_FILT with rigidLid ',
     &                         'needs shap_filt_uvStar=.true.'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHAP_FILT_READPARMS'
        ELSEIF ( .NOT.exactConserv ) THEN
         WRITE(msgBuf,'(2A)') 'S/R SHAP_FILT_READPARMS: WARNING <<< ',
     &    'applying Filter after SOLVE_FOR_P (shap_filt_uvStar=FALSE)'
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(2A)') 'S/R SHAP_FILT_READPARMS: WARNING <<< ',
     &    'requires to recompute Eta after ==> turn on exactConserv '
         CALL PRINT_ERROR( msgBuf , 1)
        ENDIF
 
       ENDIF                    

#endif /* ALLOW_SHAP_FILT */
      RETURN
      END
