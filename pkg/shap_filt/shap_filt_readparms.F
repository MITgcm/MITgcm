C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_readparms.F,v 1.8 2003/08/03 03:12:36 jmc Exp $
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
     &    nShapUV, nShapUVPhys, Shap_uvtau, Shap_uvLength,
     &    Shap_noSlip, Shap_diagFreq

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
      Shap_noSlip = 0.
      Shap_diagFreq = diagFreq

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=SHAP_PARM01)

      WRITE(msgBuf,'(A)') 
     &   ' SHAP_FILT_READPARMS: finished reading data.shap'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)

C- print out some kee parameters :
      CALL WRITE_0D_I( Shap_funct, INDEX_NONE,
     & 'Shap_funct =',
     & '   /* select Shapiro filter function */')  
      CALL WRITE_0D_I( nShapT , INDEX_NONE,
     & 'nShapTr =',
     & '   /* power of Shapiro filter for Tracers */')  
      CALL WRITE_0D_I( nShapUV, INDEX_NONE,
     & 'nShapUV =',
     & '   /* power of Shapiro filter for momentum */')  

      IF (Shap_funct.EQ.2) THEN
       CALL WRITE_0D_I( nShapTrPhys, INDEX_NONE,
     & 'nShapTrPhys =',
     & '   /* power of physical-space filter (Tracer) */')  
       CALL WRITE_0D_I( nShapUVPhys, INDEX_NONE,
     & 'nShapUVPhys =',
     & '   /* power of physical-space filter (Momentum) */')  
      ENDIF

      CALL WRITE_0D_R8( Shap_Trtau, INDEX_NONE,
     & 'Shap_Trtau =',
     & '   /* time scale of Shapiro filter (Tracer) */')  
      CALL WRITE_0D_R8( Shap_TrLength, INDEX_NONE,
     & 'Shap_TrLength =',
     & '   /* Length scale of Shapiro filter (Tracer) */')  
      CALL WRITE_0D_R8( Shap_uvtau, INDEX_NONE,
     & 'Shap_uvtau =',
     & '   /* time scale of Shapiro filter (Momentum) */')  
      CALL WRITE_0D_R8( Shap_uvLength, INDEX_NONE,
     & 'Shap_uvLength =',
     & '   /* Length scale of Shapiro filter (Momentum) */')  
      CALL WRITE_0D_R8( Shap_noSlip, INDEX_NONE,
     & 'Shap_noSlip =',
     &  '  /* No-slip parameter (0=Free-slip ; 1=No-slip)*/')  
      CALL WRITE_0D_R8( Shap_diagFreq, INDEX_NONE,
     & 'Shap_diagFreq =',
     & '   /* Frequency^-1 for diagnostic output (s)*/')  

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Check the Options :
#ifndef USE_OLD_SHAPIRO_FILTERS
#ifdef NO_SLIP_SHAP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
         WRITE(msgBuf,'(2A)') 'SHAP_FILT: CPP-option NO_SLIP_SHAP',
     &                        ' only in OLD_SHAPIRO S/R ;'
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(2A)') ' ==> use parameter Shap_noSlip=1. ',
     &                        '(in "data.shap") instead'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHAP_FILT_READPARMS'
#endif
#endif

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
         CALL PRINT_MESSAGE(msgBuf, errorMessageUnit, SQUEEZE_RIGHT,1)
         WRITE(msgBuf,'(2A)') 'S/R SHAP_FILT_READPARMS: WARNING <<< ',
     &    'requires to recompute Eta after ==> turn on exactConserv '
         CALL PRINT_MESSAGE(msgBuf, errorMessageUnit, SQUEEZE_RIGHT,1)
        ENDIF
 
       ENDIF                    

#endif /* ALLOW_SHAP_FILT */
      RETURN
      END
