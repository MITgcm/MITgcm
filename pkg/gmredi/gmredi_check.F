C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_check.F,v 1.5 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_GMREDI
      IF (GMRediIsOn) THEN

       WRITE(msgBuf,'(A)') 'GMREDI_CHECK: #define GMREDI'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C      GM/Redi needs implicit diffusion (will be packaged later)
       IF (.NOT.implicitDiffusion) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
       ENDIF

C- print out some kee parameters :
       CALL WRITE_0D_L( GM_AdvForm, INDEX_NONE,
     &  'GM_AdvForm =', '  /* if FALSE => use SkewFlux Form */')     
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

      ENDIF

#endif
      RETURN
      END
