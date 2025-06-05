#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_CHECK( myThid )

C     *==========================================================*
C     | SUBROUTINE KPP_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP_PARAMS.h"

C     === Routine arguments ===
C     myThid :: my Thread Id Number
      INTEGER myThid

#ifdef ALLOW_KPP
C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'KPP_CHECK: #define ALLOW_KPP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Print out KPP parameters:
      CALL WRITE_0D_RL( kpp_freq, INDEX_NONE, ' kpp_freq =',
     &  ' /* frequency of KPP calculation */')

      CALL WRITE_0D_L( KPP_ghatUseTotalDiffus, INDEX_NONE,
     &  'KPP_ghatUseTotalDiffus=',
     &  ' /* non-local term fct of total diffus */')
      CALL WRITE_0D_L( KPPuseDoubleDiff, INDEX_NONE,
     &  'KPPuseDoubleDiff =', ' /* include double diffusive contrib */')
      CALL WRITE_0D_L( KPPuseSWfrac3D, INDEX_NONE,
     &  'KPPuseSWfrac3D =', ' /* for light attenuation use SWfrac3D */')
      CALL WRITE_0D_L( LimitHblStable, INDEX_NONE,
     &  'LimitHblStable =', ' /* limits depth of hbl if stable cond.*/')
      CALL WRITE_0D_RL( minKPPhbl, INDEX_NONE, ' minKPPhbl =',
     &  ' /* minimum KPPhbl value [m] */')
      CALL WRITE_0D_RL( epsln,     INDEX_NONE, ' epsln     =',
     &  ' /* constant [-] */')
      CALL WRITE_0D_RL( phepsi,    INDEX_NONE, ' phepsi    =',
     &  ' /* constant [-] */')
      CALL WRITE_0D_RL( epsilon,   INDEX_NONE, ' epsilon   =',
     &  ' /* constant [-] */')
      CALL WRITE_0D_RL( vonk,      INDEX_NONE, ' vonk      =',
     &  ' /* Von Karmans constant [-] */')
      CALL WRITE_0D_RL( dB_dz,     INDEX_NONE, ' dB_dz     =',
     &  ' /* maximum N^2 in mixed layer [s^-2] */')
      CALL WRITE_0D_RL( conc1,     INDEX_NONE, ' conc1     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( conam,     INDEX_NONE, ' conam     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( concm,     INDEX_NONE, ' concm     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( conc2,     INDEX_NONE, ' conc2     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( conas,     INDEX_NONE, ' conas     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( concs,     INDEX_NONE, ' concs     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( conc3,     INDEX_NONE, ' conc3     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( zetam,     INDEX_NONE, ' zetam     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( zetas,     INDEX_NONE, ' zetas     =',
     &  ' /* scalar constant [-] */')
      CALL WRITE_0D_RL( Ricr,      INDEX_NONE, ' Ricr      =',
     &  ' /* critical bulk Richardson Number [-] */')
      CALL WRITE_0D_RL( cekman,    INDEX_NONE, ' cekman    =',
     &  ' /* coeff for Ekman depth [-] */')
      CALL WRITE_0D_RL( cmonob,    INDEX_NONE, ' cmonob    =',
     &  ' /* coeff for Monin-Obukhov depth [-] */')
      CALL WRITE_0D_RL( concv,     INDEX_NONE, ' concv     =',
     &  ' /* buoyancy freq ratio [-] */')
      CALL WRITE_0D_RL( hbf,       INDEX_NONE, ' hbf       =',
     &  ' /* solar radiation depth ratio [-] */')
      CALL WRITE_0D_RL( zmin,      INDEX_NONE, ' zmin      =',
     &  ' /* minimum for zehat in table [m3/s3] */')
      CALL WRITE_0D_RL( zmax,      INDEX_NONE, ' zmax      =',
     &  ' /* maximum for zehat in table [m3/s3] */')
      CALL WRITE_0D_RL( umin,      INDEX_NONE, ' umin      =',
     &  ' /* minimum for ustar in table [m/s] */')
      CALL WRITE_0D_RL( umax,      INDEX_NONE, ' umax      =',
     &  ' /* maximum for ustar in table [m/s] */')
      CALL WRITE_0D_I( num_v_smooth_Ri, INDEX_NONE,
     & 'num_v_smooth_Ri =', ' /* number of vertical smoothing */')
      CALL WRITE_0D_RL( Riinfty,   INDEX_NONE, ' Riinfty   =',
     &  ' /* shear instability Ri number limit [-] */')
      CALL WRITE_0D_RL( BVSQcon,   INDEX_NONE, ' BVSQcon   =',
     &  ' /* Brunt-Vaisala squared (=N^2) [s^-2] */')
      CALL WRITE_0D_RL( difm0,     INDEX_NONE, ' difm0     =',
     &  ' /* max viscosity from shear instab. [m2/s] */')
      CALL WRITE_0D_RL( difs0,     INDEX_NONE, ' difs0     =',
     &  ' /* max diffusiv. from shear instab. [m2/s] */')
      CALL WRITE_0D_RL( dift0,     INDEX_NONE, ' dift0     =',
     &  ' /* max diffusiv. from shear instab. [m2/s] */')
      CALL WRITE_0D_RL( difmcon,   INDEX_NONE, ' difmcon   =',
     &  ' /* convective viscosity [m2/s] */')
      CALL WRITE_0D_RL( difscon,   INDEX_NONE, ' difscon   =',
     &  ' /* convective diffusiv. [m2/s] */')
      CALL WRITE_0D_RL( diftcon,   INDEX_NONE, ' diftcon   =',
     &  ' /* convective diffusiv. [m2/s] */')
      CALL WRITE_0D_RL( Rrho0,     INDEX_NONE, ' Rrho0     =',
     &  ' /* double diffusion density ratio [-] */')
      CALL WRITE_0D_RL( dsfmax,    INDEX_NONE, ' dsfmax    =',
     &  ' /* max diffusiv. for salt fingering [m2/s] */')
      CALL WRITE_0D_RL( cstar,     INDEX_NONE, ' cstar     =',
     &  ' /* coeff for non-locak transport [-] */')
C--
      CALL WRITE_0D_L( KPPwriteState, INDEX_NONE,
     &  'KPPwriteState =', ' /* write KPP fields to file */')
      CALL WRITE_0D_RL( kpp_dumpFreq, INDEX_NONE, ' kpp_dumpFreq =',
     &  ' /* dump freq of KPP output */')
      CALL WRITE_0D_RL( kpp_taveFreq, INDEX_NONE, ' kpp_taveFreq =',
     &  ' /* time-averaging freq of KPP output */')

C--  Check parameters:

C     KPP needs convection turned off (will be packaged later)
      IF (cAdjFreq.NE.0.  .OR.
     &     ivdc_kappa.NE.0.) THEN
         WRITE(msgBuf,'(A)') 'Some form of convection has been enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     KPP needs implicit vertical diffusion and viscosity
      IF (.NOT.implicitDiffusion ) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitDiffusion to be enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
      IF ( .NOT.implicitViscosity .AND. momStepping ) THEN
         WRITE(msgBuf,'(A)') 'KPP needs implicitViscosity to be enabled'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF

C     Check that overlap region is adequate.
C     When horizontal smoothing is turned on,
C     KPP_CALC computes vertical viscosity and diffusivity for region
C     (-2:sNx+3,-2:sNy+3) as required by CALC_DIFFUSIVITY and requires
C     values of uVel, vVel, surfaceForcingU, surfaceForcingV in the
C     region (-2:sNx+4,-2:sNy+4).
C     Hence overlap region needs to be set OLx=4, OLy=4.
#if defined(KPP_SMOOTH_SHSQ) || defined(KPP_SMOOTH_DVSQ)
#  define KPP_NEEDS_OVERLAP_4
#endif
#if defined(KPP_SMOOTH_DBLOC) || defined(KPP_SMOOTH_DENS)
#  define KPP_NEEDS_OVERLAP_4
#endif
#ifdef KPP_NEEDS_OVERLAP_4
#ifdef KPP_REACTIVATE_OL4
      IF (OLx.LT.4 .OR. OLy.LT.4) THEN
       WRITE(msgBuf,'(A)') 'KPP horizontal smoothing requires OLx=OLy=4'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
#endif
#endif

#ifdef EXCLUDE_KPP_DOUBLEDIFF
      IF ( KPPuseDoubleDiff ) THEN
       WRITE(msgBuf,'(A,A)') 'KPPuseDoubleDiff=.TRUE. requires ',
     &      'that EXCLUDE_KPP_DOUBLEDIFF is undefined'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R KPP_CHECK'
      ENDIF
#endif /* EXCLUDE_KPP_DOUBLEDIFF */

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

#endif /* ALLOW_KPP */
      RETURN
      END
