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
C     myThid :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_GMREDI
C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iL, errCount
      _RL     tmpVar
CEOP
#ifdef ALLOW_PTRACERS
      INTEGER iTr
      LOGICAL redFlag
#endif

      _BEGIN_MASTER(myThid)
      errCount = 0

       WRITE(msgBuf,'(A)') 'GMREDI_CHECK: #define GMREDI'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )

C- print out some key parameters :
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
c      CALL WRITE_0D_RL( GM_background_K, INDEX_NONE,
c    &  'GM_background_K =',
c    &  ' /* Background GM (=Bolus) Diffusivity [m^2/s] */')
       tmpVar = GM_background_K*( oneRL - GM_skewflx )
       CALL WRITE_0D_RL( tmpVar, INDEX_NONE, 'GM_advec*K =',
     &  '    /* Backg. GM-Advec(=Bolus) Diffusivity [m^2/s] */')
       CALL WRITE_0D_RL( GM_background_K*GM_skewflx, INDEX_NONE,
     &  'GM_skewflx*K =',
     &  '  /* Background GM_SkewFlx Diffusivity [m^2/s] */')
       CALL WRITE_0D_RL( GM_isoFac_calcK, INDEX_NONE,
     &  'GM_isoFac_calcK =',
     &  ' /* Fraction of dynamic K added to Redi tensor */')
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
       CALL WRITE_0D_L( GM_useLeithQG, INDEX_NONE,
     &  'GM_useLeithQG =',
     &  ' /* if TRUE => add QG Leith viscosity to GMRedi tensor */')
       CALL WRITE_0D_L( GM_useGEOM, INDEX_NONE,
     &  'GM_useGEOM =', ' /* using GEOMETRIC */')

C--  Check parameters:

C-     GM/Redi needs implicit diffusion (will be packaged later)
      IF ( .NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , myThid )
        errCount = errCount + 1
      ENDIF

#ifndef GM_READ_K3D_REDI
      IF ( GM_K3dRediFile.NE.' ' ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM_K3dRediFile is set in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_READ_K3D_REDI'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
#ifndef GM_READ_K3D_GM
      IF ( GM_K3dGMFile.NE.' ' ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM_K3dGMFile is set in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_READ_K3D_GM'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

#ifndef GM_VISBECK_VARIABLE_K
C     Make sure we are not trying to use something that is unavailable
      IF ( GM_Visbeck_alpha.NE.0. ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: Visbeck variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_VISBECK_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
#ifdef OLD_VISBECK_CALC
C Was to use old calculation (before 2007/05/24) of Visbeck etal K_GM+Redi
C (which depends on tapering scheme); got removed late 2025.
c     IF ( GM_Visbeck_alpha.NE.0. ) THEN
       WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   '#define OLD_VISBECK_CALC not allowed in GMREDI_OPTIONS.h'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   ' since OLD_VISBECK_CALC code has been removed'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
c     ENDIF
#endif

#ifdef GM_GEOM_VARIABLE_K
      IF ( GM_useGEOM ) THEN
       CALL WRITE_0D_RL( GEOM_alpha, INDEX_NONE,
     &  'GEOM_alpha      =', ' /* GEOM alpha coeff. [-] */')
       CALL WRITE_0D_RL( GEOM_lmbda, INDEX_NONE,
     &  'GEOM_lmbda      =', ' /* GEOM ene diss rate [/s] */')
       CALL WRITE_0D_RL( GEOM_diffKh_EKE, INDEX_NONE,
     &  'GEOM_diffKh_EKE =', ' /* GEOM Eke diffusion coeff [m2/s] */')
       CALL WRITE_0D_L( GEOM_vert_struc, INDEX_NONE,
     &  'GEOM_vert_struc =', ' /* GEOM vertical structure function */')
       CALL WRITE_0D_RL( GEOM_vert_struc_min, INDEX_NONE,
     &  'GEOM_vert_struc_min =', ' /* vert struc min [-] */')
       CALL WRITE_0D_RL( GEOM_vert_struc_max, INDEX_NONE,
     &  'GEOM_vert_struc_min =', ' /* vert struc min [-] */')
       CALL WRITE_0D_RL( GEOM_minVal_K, INDEX_NONE,
     &  'GEOM_minVal_K   =', ' /* GEOM_K3d lower bound [m2/s] */')
       CALL WRITE_0D_RL( GEOM_maxVal_K, INDEX_NONE,
     &  'GEOM_maxVal_K   =', ' /* GEOM_K3d upper bound [m2/s] */')
       IF (usingPcoords) THEN
        WRITE(msgBuf,'(3A)') 'GMREDI_CHECK:',
     &       ' p-coords not yet implemented with GM_useGEOM'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
#else /* GM_GEOM_VARIABLE_K */
C     Make sure we are not trying to use something that is unavailable
      IF ( GM_useGEOM ) THEN
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: GEOM variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' GMREDI_CHECK: without #define GM_GEOM_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif /* GM_GEOM_VARIABLE_K */

#ifndef GM_BOLUS_ADVEC
C     Make sure we are not trying to use some arrays that are unavailable
      IF ( GM_AdvForm ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM Advection form used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_BOLUS_ADVEC'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

#ifdef GM_BATES_K3D
# ifndef HAVE_LAPACK
c     IF ( GM_useBatesK3d ) THEN
        WRITE(msgBuf, '(A)')
     &     'Must use CPP option HAVE_LAPACK when using BatesK3d'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
c     ENDIF
# endif
      IF ( GM_useBatesK3d ) THEN
        CALL WRITE_0D_L( GM_useBatesK3d, INDEX_NONE,
     &   'GM_useBatesK3d =',
     &   ' /* if TRUE => use BatesK3d for GM diffusivity */')
        IF ( GM_Bates_use_constK ) THEN
          CALL WRITE_0D_L( GM_Bates_use_constK, INDEX_NONE,
     &         'GM_Bates_use_constK =',
     &         ' /* if TRUE => Uses a constant K for'//
     &         ' the eddy transport closure */')
          CALL WRITE_0D_L( GM_Bates_smooth, INDEX_NONE,
     &         'GM_Bates_smooth =',
     &         ' /* if TRUE => Expands in terms of baroclinic modes */')
          IF ( GM_Bates_smooth ) THEN
            CALL WRITE_0D_I( GM_Bates_NModes, INDEX_NONE,
     &           'GM_Bates_NModes =',
     &           ' /* Number of modes for expansion */')
          ENDIF
        ELSE
          CALL WRITE_0D_I( GM_Bates_NModes, INDEX_NONE,
     &         'GM_Bates_NModes =',
     &         ' /* Number of modes for expansion */')
        ENDIF
      ENDIF

C     Make sure that we use BatesK3d with the advective form only.
C     The skew form is not presently supported.
      IF ( GM_useBatesK3d .AND. .NOT.GM_AdvForm ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &       'GM_useBatesK3d=.TRUE. but GM_AdvForm=.FALSE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'GMREDI_CHECK: To use BatesK3d set GM_AdvForm=.TRUE.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( GM_useBatesK3d .AND. deepAtmosphere ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: GM_useBatesK3d',
     &   ' not yet fixed for deep geometry (deepAtmosphere=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

#else /* GM_BATES_K3D */
C     Make sure we are not trying to use something that is unavailable
      IF ( GM_useBatesK3d ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM_useBatesK3d is set to True in data.gmredi'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_BATES_K3D'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif /* GM_BATES_K3D */

#ifndef GM_EXTRA_DIAGONAL
C     Make sure we are not trying to use some arrays that are unavailable
      IF ( GM_ExtraDiag ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM_skew_Flux_K & GM_isopycK not equal'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: without #define GM_EXTRA_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

#ifndef GM_NON_UNITY_DIAGONAL
      IF ( GM_iso2dFile .NE. ' ' .OR.
     &     GM_iso1dFile .NE. ' ' ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: needs #define GM_NON_UNITY_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: to use GM_iso2dFile or GM_iso1dFile'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( GM_isoFac_calcK .NE. zeroRL .AND.
     &     ( GM_Visbeck_alpha.NE.zeroRL .OR.
     &       GM_useBatesK3d .OR. GM_useLeithQG ) ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: needs #define GM_NON_UNITY_DIAGONAL'
       CALL PRINT_ERROR( msgBuf, myThid )
       IF ( GM_Visbeck_alpha.NE.zeroRL ) THEN
        WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: to use Visbeck computed K'
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( GM_useBatesK3d .OR. GM_useLeithQG ) THEN
        WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: to use GM_useBatesK3d or GM_useLeithQG'
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       errCount = errCount + 1
      ENDIF
      IF ( errCount .EQ. 0 ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** GMREDI_CHECK: ',
     &       '#undef GM_NON_UNITY_DIAGONAL not recommended'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

#ifndef ALLOW_GM_LEITH_QG
      IF ( GM_useLeithQG ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: GM_useLeithQG used in data.gmredi without'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: #define ALLOW_GM_LEITH_QG'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
      IF ( GM_useLeithQG .AND. .NOT.momStepping ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   'cannot use GM_useLeithQG with "momStepping= FALSE"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

#ifdef GM_EXCLUDE_SUBMESO
      IF ( GM_useSubMeso ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   'cannot use Sub-Meso (GM_useSubMeso=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   'when compiled with #define GM_EXCLUDE_SUBMESO'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* GM_EXCLUDE_SUBMESO */

      IF ( GM_useSubMeso .AND. .NOT.GM_AdvForm ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: ',
     &   'Sub-Meso only implemented within GM_AdvForm'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( GM_InMomAsStress ) THEN
#ifdef ALLOW_EDDYPSI
        IF ( .NOT.GM_useBatesK3d ) THEN
          WRITE(msgBuf,'(3A)') 'GMREDI_CHECK: ',
     &         'Using GM_InMomAsStress and not GM_useBatesK3d. ',
     &         'GM_InMomAsStress=T has only been tested with BatesK3d'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
#else /* ALLOW_EDDYPSI */
       WRITE(msgBuf,'(2A)')
     &  'GMREDI_CHECK: need to define ALLOW_EDDYPSI in CPP_OPTIONS.h',
     &  ' to use GM_InMomAsStress'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* ALLOW_EDDYPSI */
      ENDIF
      IF ( GM_InMomAsStress .AND. .NOT.GM_AdvForm ) THEN
       WRITE(msgBuf,'(A)')
     &   'GMREDI_CHECK: need GM_AdvForm=T to use GM_InMomAsStress'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( usingPcoords ) THEN
C-    Some GM pieces of code have not been fixed to work in Pressure-coordinates
       IF ( GM_taper_scheme.EQ.'fm07' .OR.
     &      GM_taper_scheme.EQ.'stableGmAdjTap' ) THEN
        iL = ILNBLNK(GM_taper_scheme)
        WRITE(msgBuf,'(3A)') 'GMREDI_CHECK: GM_taper_scheme "',
     &   GM_taper_scheme(1:iL), '" not yet fixed for P-Coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_useBVP ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: GM_useBVP',
     &    ' code not yet fixed for P-Coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_Visbeck_alpha.NE.zeroRL ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: Visbeck',
     &    'scheme not yet fixed for P-Coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_useBatesK3d ) THEN
        WRITE(msgBuf,'(2A,I3)') '** WARNING ** GMREDI_CHECK: ',
     &    'GM_useBatesK3d potentially unsafe with P-Coordinate'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( GM_useLeithQG ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: GM_useLeithQG',
     &    ' code not yet fixed for P-Coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_useSubMeso ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: GM_useSubMeso',
     &   ' not yet fixed for P-Coordinate'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

      IF ( useShelfIce .OR. topoFile.NE.' ' ) THEN
C-    Some GM options are likely to not work when some dry grid cells are found
C     near the top (e.g., under ice-shelf). This might change as more pieces of
C     code get fixed (e.g., once Mixed-Layer depth is fixed) but for now safer
C     to just stop here if it is the case.
       IF ( GM_taper_scheme.EQ.'ldd97' .OR.
     &      GM_taper_scheme.EQ.'fm07' ) THEN
        iL = ILNBLNK(GM_taper_scheme)
        WRITE(msgBuf,'(3A)') 'GMREDI_CHECK: GM_taper_scheme "',
     &   GM_taper_scheme(1:iL), '" not yet fixed for dry cell @ top'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_useSubMeso ) THEN
        WRITE(msgBuf,'(2A)') 'GMREDI_CHECK: GM_useSubMeso',
     &   ' not yet fixed for dry cell @ top'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( GM_useBatesK3d ) THEN
        WRITE(msgBuf,'(2A,I3)') '** WARNING ** GMREDI_CHECK: ',
     &    'GM_useBatesK3d potentially unsafe with dry cell @ top'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( GM_Visbeck_alpha.NE.zeroRL ) THEN
        WRITE(msgBuf,'(2A,I3)') '** Warning ** GMREDI_CHECK: ',
     &    'Visbeck scheme not fully tested with dry cell @ top'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( GM_useLeithQG ) THEN
        WRITE(msgBuf,'(2A,I3)') '** Warning ** GMREDI_CHECK: ',
     &    'GM_useLeithQG not fully tested with dry cell @ top'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
C-    end special check for dry cell @ top
      ENDIF

#ifdef ALLOW_PTRACERS
      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. usePTRACERS ) THEN
        redFlag = .FALSE.
        DO iTr=1,PTRACERS_numInUse
         IF ( .NOT.PTRACERS_useGMRedi(iTr) ) THEN
          redFlag = .TRUE.
          WRITE(msgBuf,'(2A,I3,A,L5)') 'GMREDI_CHECK:',
     &     ' pTracers_useGMRedi(',iTr,' )=', PTRACERS_useGMRedi(iTr)
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO
        IF ( redFlag ) THEN
          WRITE(msgBuf,'(2A)') 'GMREDI_CHECK:',
     &     ' but GM Advective Form applies to all tracers !'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_GENERIC_ADVDIFF
C     Check size of overlap region
      IF ( GM_AdvForm .AND. .NOT.GM_AdvSeparate
     &       .AND. GM_Visbeck_alpha.NE.0.
     &       .AND. useMultiDimAdvec ) THEN
C       Visbeck variable K requires 1 more row/column in the overlap:
C       might need to increase OLx,OLy from 2 to 3 if GM advective
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

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'GMREDI_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
      ENDIF
      _END_MASTER(myThid)

#endif /* ALLOW_GMREDI */
      RETURN
      END
