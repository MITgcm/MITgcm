C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_check.F,v 1.10 2003/11/12 03:58:48 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_CHECK                                  |
C     | o Check dependances with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GAD.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_GMREDI

       WRITE(msgBuf,'(A)') 'GMREDI_CHECK: #define GMREDI'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C- print out some kee parameters :
       CALL WRITE_0D_L( GM_AdvForm, INDEX_NONE,
     &  'GM_AdvForm =', '  /* if FALSE => use SkewFlux Form */')     
       CALL WRITE_0D_L( GM_AdvSeparate, INDEX_NONE,
     & 'GM_AdvSeparate =',' /* Calc Bolus & Euler Adv. separately */')     
       CALL WRITE_0D_L( GM_ExtraDiag, INDEX_NONE,
     &  'GM_ExtraDiag =','  /* Tensor Extra Diag (line 1&2) non 0 */')
       CALL WRITE_0D_R8( GM_isopycK, INDEX_NONE,'GM_isopycK =',
     &  '   /* Background Isopyc. Diffusivity ( m^2/s ) */')       
       CALL WRITE_0D_R8( GM_background_K*GM_skewflx, INDEX_NONE,
     &  ' GM_skewflx*K =',
     &  '   /* Background GM_SkewFlx Diffusivity ( m^2/s ) */')       
       CALL WRITE_0D_R8( GM_background_K*GM_advect, INDEX_NONE,
     &  ' GM_advec*K =',
     &  '   /* Backg. GM-Advec(=Bolus) Diffusivity ( m^2/s ) */')       
       CALL WRITE_0D_R8( GM_Visbeck_alpha, INDEX_NONE,
     &  ' GM_Visbeck_alpha =','   /* Visbeck alpha coeff. ( ) */')       
       WRITE(msgBuf,'(A,A40)')' Tapering/Cliping : ',GM_taper_scheme
       CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
       CALL WRITE_0D_R8( GM_Small_Number, INDEX_NONE,
     &  ' GM_Small_Number =','  /* epsilon used in slope calc */')       
       CALL WRITE_0D_R8( GM_slopeSqCutoff, INDEX_NONE,
     &  ' GM_slopeSqCutoff =', '  /* Slope^2 cut-off value */')       


C--  Check parameters:

      _BEGIN_MASTER(myThid)

C-     GM/Redi needs implicit diffusion (will be packaged later)
       IF (.NOT.implicitDiffusion) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
       ENDIF

#ifndef GM_VISBECK_VARIABLE_K
C     Make sure we are not trying to use something that is unavailable
      IF (GM_Visbeck_alpha .NE. 0.) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: Visbeck variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_VISBECK_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, 1 )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifndef GM_BOLUS_ADVEC
C     Make sure we are not trying to use some arrays that are unavailable
      IF (GM_AdvForm) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: GM Advection form used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_BOLUS_ADVEC'
       CALL PRINT_ERROR( msgBuf, 1 )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifndef GM_EXTRA_DIAGONAL
C     Make sure we are not trying to use some arrays that are unavailable
      IF (GM_ExtraDiag) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: GM_skew_Flux_K & GM_isopycK not equal'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_EXTRA_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, 1 )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. (tempMultiDimAdvec.OR.saltMultiDimAdvec)
     &       .AND. (Olx.LT.3 .OR. Oly.LT.3) ) THEN
C note: a) this is a practical limitation. In theory, WITHOUT Visbeck,
C       changing some loop indices would make it works with Olx=Oly=2
C       b) we really need 1 flag (like multiDimAdvection) that tells
C       if a least 1 tracer is using a multi-Dim advection scheme.
        WRITE(msgBuf,'(A,2I3,A)')
     &  'GMREDI_CHECK: Olx,Oly WRONG: cannot use those AdvScheme (',
     &     tempAdvScheme, saltAdvScheme, ') '
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A,A)')
     &  'GMREDI_CHECK: and GM_AdvForm(not_Separate) with',
     &  ' (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_GMREDI */
      RETURN
      END
