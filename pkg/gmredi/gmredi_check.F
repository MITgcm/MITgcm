C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_check.F,v 1.30 2014/05/18 02:49:52 m_bates Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_CHECK
C     !INTERFACE:
      SUBROUTINE GMREDI_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CHECK
C     | o Check consistency with model configuration
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
#ifdef ALLOW_PTRACERS
      INTEGER iTr
      LOGICAL redFlag
#endif

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'GMREDI_CHECK: #define GMREDI'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

C- print out some kee parameters :
       CALL WRITE_0D_L( GM_AdvForm, INDEX_NONE,
     &  'GM_AdvForm =', '     /* if FALSE => use SkewFlux Form */')
       CALL WRITE_0D_L( GM_InMomAsStress, INDEX_NONE,
     &  'GM_InMomAsStress =', ' /* if TRUE => apply as Eddy Stress */')
       CALL WRITE_0D_L( GM_AdvSeparate, INDEX_NONE,
     & 'GM_AdvSeparate =',' /* Calc Bolus & Euler Adv. separately */')
       CALL WRITE_0D_L( GM_ExtraDiag, INDEX_NONE,
     &  'GM_ExtraDiag =','   /* Tensor Extra Diag (line 1&2) non 0 */')
       CALL WRITE_0D_RL( GM_isopycK, INDEX_NONE, 'GM_isopycK =',
     &  '    /* Background Isopyc. Diffusivity [m^2/s] */')
       CALL WRITE_0D_RL( GM_background_K*GM_skewflx, INDEX_NONE,
     &  'GM_skewflx*K =',
     &  '  /* Background GM_SkewFlx Diffusivity [m^2/s] */')
       CALL WRITE_0D_RL( GM_background_K*GM_advect, INDEX_NONE,
     &  'GM_advec*K =',
     &  '    /* Backg. GM-Advec(=Bolus) Diffusivity [m^2/s]*/')
       CALL WRITE_0D_RL( GM_Kmin_horiz, INDEX_NONE, 'GM_Kmin_horiz =',
     &  ' /* Minimum Horizontal Diffusivity [m^2/s] */')
       CALL WRITE_0D_RL( GM_Visbeck_alpha, INDEX_NONE,
     &  'GM_Visbeck_alpha =', ' /* Visbeck alpha coeff. [-] */')
       CALL WRITE_0D_RL( GM_Small_Number, INDEX_NONE,
     &  'GM_Small_Number =', '  /* epsilon used in slope calc */')
       CALL WRITE_0D_RL( GM_slopeSqCutoff, INDEX_NONE,
     &  'GM_slopeSqCutoff =', ' /* Slope^2 cut-off value */')
       CALL WRITE_0D_C( GM_taper_scheme, 0, INDEX_NONE,
     &  'GM_taper_scheme =',
     &  '  /* Type of Tapering/Clipping scheme */')
       CALL WRITE_0D_RL( GM_maxSlope, INDEX_NONE,
     &  'GM_maxSlope =', '  /* Maximum Slope (Tapering/Clipping) */')
       CALL WRITE_0D_RL( GM_facTrL2dz, INDEX_NONE,
     &  'GM_facTrL2dz =',
     &  ' /* Minimum Trans.Layer Thick. (factor of dz) */')
       CALL WRITE_0D_RL( GM_facTrL2ML, INDEX_NONE,
     &  'GM_facTrL2ML =',
     &  ' /* Max.Trans.Layer Thick. (factor of MxL Depth)*/')
       CALL WRITE_0D_RL( GM_maxTransLay, INDEX_NONE,
     &  'GM_maxTransLay =',
     &  ' /* Maximum Transition Layer Thickness [m] */')
       CALL WRITE_0D_L( GM_UseBVP, INDEX_NONE,
     &  'GM_UseBVP =',
     &  ' /* if TRUE => use bvp a la Ferrari et al. (2010) */')
       CALL WRITE_0D_I( GM_BVP_ModeNumber, INDEX_NONE,
     &  'GM_BVP_ModeNumber =',
     &  ' /* Vertical mode number for BVP wave speed */')
       CALL WRITE_0D_RL( GM_BVP_cMin, INDEX_NONE,
     &  'GM_BVP_cMin =',
     &  ' /* Minimum wave speed for BVP [m/s] */')
       CALL WRITE_0D_L( GM_useSubMeso, INDEX_NONE,
     &  'GM_useSubMeso =',
     &  ' /* if TRUE => use Sub-Meso param. (B.Fox-Kemper) */')
       CALL WRITE_0D_RL( subMeso_Ceff, INDEX_NONE,
     &  'subMeso_Ceff =',
     &  ' /* efficiency coeff. of Mixed-Layer Eddies [-] */')
       CALL WRITE_0D_RL( subMeso_invTau, INDEX_NONE,
     &  'subMeso_invTau =',
     &  ' /* inverse of Sub-Meso mixing time-scale [/s] */')
       CALL WRITE_0D_RL( subMeso_LfMin, INDEX_NONE,
     &  'subMeso_LfMin =',' /* minimum length-scale "Lf" [m] */')
       CALL WRITE_0D_RS( subMeso_Lmax, INDEX_NONE,
     &  'subMeso_Lmax =',' /* maximum grid-scale length [m] */')

C--  Check parameters:

C-     GM/Redi needs implicit diffusion (will be packaged later)
      IF ( .NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF

#ifndef GM_VISBECK_VARIABLE_K
C     Make sure we are not trying to use something that is unavailable
      IF ( GM_Visbeck_alpha.NE.0. ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: Visbeck variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_VISBECK_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifndef GM_BOLUS_ADVEC
C     Make sure we are not trying to use some arrays that are unavailable
      IF ( GM_AdvForm ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: GM Advection form used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_BOLUS_ADVEC'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifdef GM_K3D
# ifndef HAVE_LAPACK
      WRITE(msgBuf, '(A)')
     &     'Must use CPP option HAVE_LAPACK when using GM_K3D'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP ' ABNORMAL END: S/R GMREDI_CHECK'
# endif

      IF ( GM_useK3D) THEN
        CALL WRITE_0D_L( GM_useK3D, INDEX_NONE,
     &   'GM_useK3D =', '     /* if TRUE => use K3D for diffusivity */')
        
        IF ( GM_K3D_likeGM ) THEN
          CALL WRITE_0D_L( GM_K3D_likeGM, INDEX_NONE,
     &         'GM_K3D_likeGM =', 
     &         '     /* if TRUE => Uses a GM-type closure */')
        
          CALL WRITE_0D_L( GM_K3D_smooth, INDEX_NONE,
     &         'GM_K3D_smooth =', 
     &         ' /* if TRUE => Expands in terms of baroclinic modes */')
        
          IF ( GM_K3D_smooth ) THEN
            CALL WRITE_0D_I( GM_K3D_NModes, INDEX_NONE,
     &           'GM_K3D_NModes =',
     &           ' /* Number of modes for expansion */')
          ENDIF

        ELSE
          CALL WRITE_0D_I( GM_K3D_NModes, INDEX_NONE,
     &         'GM_K3D_NModes =',
     &         ' /* Number of modes for expansion */')

        ENDIF

      ENDIF

C     Make sure that we use K3D with the advective form only.
C     The skew form is not presently supported.
      IF ( GM_useK3D .AND. .NOT. GM_AdvForm) THEN
        WRITE(msgBuf,'(A)')
     &       ' GMREDI_CHECK: GM_useK3D=.TRUE. but GM_AdvForm=.FALSE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       ' GMREDI_CHECK: To use GM_useK3D set GM_AdvForm=.TRUE.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP ' ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF

      IF ( GM_useK3D) THEN
C       Make sure that we use K3D without smoothing only when we specify
C       a constant K should be used
        IF ( .NOT. GM_K3D_likeGM .AND. .NOT. GM_K3D_smooth) THEN
          WRITE(msgBuf,'(A)')
     &  ' GMREDI_CHECK: GM_K3D_likeGM=.FALSE. and GM_K3D_smooth=.FALSE.'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' GMREDI_CHECK: To have variable K set GM_K3D_smooth=.TRUE.'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP ' ABNORMAL END: S/R GMREDI_CHECK'
        ENDIF
      ENDIF

#endif

#ifndef GM_EXTRA_DIAGONAL
C     Make sure we are not trying to use some arrays that are unavailable
      IF ( GM_ExtraDiag ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: GM_skew_Flux_K & GM_isopycK not equal'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_EXTRA_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifndef GM_NON_UNITY_DIAGONAL
      IF ( GM_iso2dFile .NE. ' ' .OR.
     &     GM_iso1dFile .NE. ' ' ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: needs #define GM_NON_UNITY_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: to use GM_iso2dFile or GM_iso1dFile'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif

#ifdef GM_EXCLUDE_SUBMESO
      IF ( GM_useSubMeso ) THEN
        WRITE(msgBuf,'(2A)') ' GMREDI_CHECK: ',
     &   'cannot use Sub-Meso (GM_useSubMeso=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') ' GMREDI_CHECK: ',
     &   'when compiled with #define GM_EXCLUDE_SUBMESO'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
#endif /* GM_EXCLUDE_SUBMESO */

      IF ( GM_useSubMeso .AND. .NOT.GM_AdvForm ) THEN
        WRITE(msgBuf,'(2A)') ' GMREDI_CHECK: ',
     &   'Sub-Meso only implemented within GM_AdvForm'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF

      IF ( GM_InMomAsStress ) THEN
#ifdef ALLOW_EDDYPSI
        IF (.NOT. GM_AdvForm) THEN
          WRITE(msgBuf,'(2A)') ' GMREDI_CHECK: ',
     &         'GM_InMomAsStress=.TRUE. but GM_AdvForm=.FALSE.'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R GMREDI_CHECK'
        ENDIF
        IF (.NOT. GM_useK3D) THEN
          WRITE(msgBuf,'(3A)')
     &         ' GMREDI_CHECK: Using GM_InMomAsStress and not',
     &         ' GM_useK3D. GM_InMomAsStress=.true. has only been',
     &         ' tested with GM_K3D=.true.'
        ENDIF
#else /* ALLOW_EDDYPSI */
       WRITE(msgBuf,'(2A)')
     &  ' GMREDI_CHECK: need to define ALLOW_EDDYPSI in CPP_OPTIONS.h',
     &  ' to use GM_InMomAsStress'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
#endif /* ALLOW_EDDYPSI */
      ENDIF
      IF ( GM_InMomAsStress .AND. .NOT.GM_AdvForm ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: need GM_AdvForm=T to use GM_InMomAsStress'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF

#ifdef ALLOW_PTRACERS
      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. usePTRACERS ) THEN
        redFlag = .FALSE.
        DO iTr=1,PTRACERS_numInUse
         IF ( .NOT.PTRACERS_useGMRedi(iTr) ) THEN
          redFlag = .TRUE.
          WRITE(msgBuf,'(2A,I3,A,L5)') ' GMREDI_CHECK:',
     &     ' pTracers_useGMRedi(',iTr,' )=', PTRACERS_useGMRedi(iTr)
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO
        IF ( redFlag ) THEN
          WRITE(msgBuf,'(2A)') ' GMREDI_CHECK:',
     &     ' but GM Advective Form applies to all tracers !'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R GMREDI_CHECK'
        ENDIF
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_GENERIC_ADVDIFF
C     Check size of overlap region
      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. GM_Visbeck_alpha.NE.0.
     &       .AND. useMultiDimAdvec ) THEN
C       Visbeck variable K requires 1 more row/column in the overlap:
C       might need to increase Olx,Oly from 2 to 3 if GM advective
C       form & multi-dim advection are used. This happens when:
C       a) using a 5 points stencil advection scheme ; or
C       b) using a 3 points stencil advection scheme on CS-grid
        GAD_OlMinSize(2) = MAX( GAD_OlMinSize(2), 1)
        WRITE(msgBuf,'(A,9I3)')
     &      'GMREDI_CHECK: GAD_OlMinSize=', GAD_OlMinSize
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

      _END_MASTER(myThid)

#endif /* ALLOW_GMREDI */
      RETURN
      END
