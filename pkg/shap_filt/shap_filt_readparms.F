C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_readparms.F,v 1.16 2014/05/27 23:41:30 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_READPARMS
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHAP_FILT_READPARMS
C     | o Routine to initialize Shapiro Filter parameters
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
CEOP

      NAMELIST /SHAP_PARM01/
     &   Shap_funct, shap_filt_uvStar, shap_filt_TrStagg,
     &   Shap_alwaysExchUV, Shap_alwaysExchTr,
     &   nShapT,nShapS, nShapTrPhys, Shap_Trtau, Shap_TrLength,
     &   nShapUV, nShapUVPhys, Shap_uvtau, Shap_uvLength,
     &   Shap_noSlip, Shap_diagFreq

      IF ( .NOT.useSHAP_FILT ) THEN
C-    pkg SHAP_FILT is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
c        SHAPIsOn = .FALSE.
C     print a (weak) warning if data.shap is found
         CALL PACKAGES_UNUSED_MSG( 'useSHAP_FILT', ' ', 'shap' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C--   SHAP_FILT_READPARMS has been called so we know that
C     the package is active.
c     SHAPIsOn = .TRUE.

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
      shap_filt_uvStar  = .TRUE.
      shap_filt_TrStagg = .TRUE.
      Shap_alwaysExchUV = .FALSE.
      Shap_alwaysExchTr = .FALSE.
      nShapT = 0
      nShapS = -1
      nShapUV = 0
      nShapTrPhys = 0
      nShapUVPhys = 0
      Shap_Trtau = dTtracerLev(1)
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

C     for backward compatibility:
      IF (nShapS.EQ.-1) nShapS = nShapT

      IF (Shap_funct.EQ.20) THEN
C     use shap-funct S2 with nShap_Phys=nShap
C     to get exactly the same results as shap-funct S2G.
        nShapTrPhys = MAX(nShapT,nShapS)
        nShapUVPhys = nShapUV
      ENDIF

      IF ( Shap_funct.EQ.1 .OR. Shap_funct.EQ.4
     &     .OR. Shap_funct.EQ.21
     &   ) THEN
        Shap_alwaysExchUV = .TRUE.
      ENDIF
      IF ( Shap_funct.EQ.1 .OR. Shap_funct.EQ.4
     &   ) THEN
        Shap_alwaysExchTr = .TRUE.
      ENDIF

C- print out some kee parameters :
      CALL WRITE_0D_I( Shap_funct, INDEX_NONE,
     & 'Shap_funct =',
     & '   /* select Shapiro filter function */')
      CALL WRITE_0D_I( nShapT , INDEX_NONE,
     & 'nShapT =',
     & '   /* power of Shapiro filter for Temperat */')
      CALL WRITE_0D_I( nShapS , INDEX_NONE,
     & 'nShapS =',
     & '   /* power of Shapiro filter for Salinity */')
      CALL WRITE_0D_I( nShapUV, INDEX_NONE,
     & 'nShapUV =',
     & '   /* power of Shapiro filter for momentum */')

      CALL WRITE_0D_L( shap_filt_uvStar,  INDEX_NONE,
     & 'shap_filt_uvStar =',' /* apply filter before Press. Solver */')
      CALL WRITE_0D_L( shap_filt_TrStagg, INDEX_NONE,
     & 'shap_filt_TrStagg =',
     & ' /* filter T,S before calc PhiHyd (staggerTimeStep) */')
      CALL WRITE_0D_L( Shap_alwaysExchUV, INDEX_NONE,
     & 'Shap_alwaysExchUV =',' /* always exch(U,V)    nShapUV times*/')
      CALL WRITE_0D_L( Shap_alwaysExchTr, INDEX_NONE,
     & 'Shap_alwaysExchTr =',' /* always exch(Tracer) nShapTr times*/')

      IF (Shap_funct.EQ.2) THEN
       CALL WRITE_0D_I( nShapTrPhys, INDEX_NONE,
     & 'nShapTrPhys =',
     & '   /* power of physical-space filter (Tracer) */')
       CALL WRITE_0D_I( nShapUVPhys, INDEX_NONE,
     & 'nShapUVPhys =',
     & '   /* power of physical-space filter (Momentum) */')
      ENDIF

      CALL WRITE_0D_RL( Shap_Trtau, INDEX_NONE,
     & 'Shap_Trtau =',
     & '   /* time scale of Shapiro filter (Tracer) */')
      CALL WRITE_0D_RL( Shap_TrLength, INDEX_NONE,
     & 'Shap_TrLength =',
     & '   /* Length scale of Shapiro filter (Tracer) */')
      CALL WRITE_0D_RL( Shap_uvtau, INDEX_NONE,
     & 'Shap_uvtau =',
     & '   /* time scale of Shapiro filter (Momentum) */')
      CALL WRITE_0D_RL( Shap_uvLength, INDEX_NONE,
     & 'Shap_uvLength =',
     & '   /* Length scale of Shapiro filter (Momentum) */')
      CALL WRITE_0D_RL( Shap_noSlip, INDEX_NONE,
     & 'Shap_noSlip =',
     &  '  /* No-slip parameter (0=Free-slip ; 1=No-slip)*/')
      CALL WRITE_0D_RL( Shap_diagFreq, INDEX_NONE,
     & 'Shap_diagFreq =',
     & '   /* Frequency^-1 for diagnostic output (s)*/')

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

C-    Some Filters / options are not available on CS-grid:
      IF (useCubedSphereExchange) THEN
       IF ( Shap_funct.EQ.1 .OR. Shap_funct.EQ.4 ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SHAP_FILT on CS-grid ',
     &           'does not work with Shap_funct=', Shap_funct
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHAP_FILT_READPARMS'
       ELSEIF ( Shap_funct.EQ.21 .AND. nShapUV.GT.0
     &                 .AND. nSx*nSy*nPx*nPy .NE. 6 ) THEN
         WRITE(msgBuf,'(2A)') 'SHAP_FILT on CS-grid:',
     &     ' multi-tiles / face not implemented with'
         CALL PRINT_ERROR( msgBuf , 1)
         WRITE(msgBuf,'(A,I3,A)') ' Shap_funct=', Shap_funct,
     &     ' ; => use instead Shap_funct=2 & nShap[]Phys=0'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SHAP_FILT_READPARMS'
       ENDIF
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_SHAP_FILT */
      RETURN
      END
