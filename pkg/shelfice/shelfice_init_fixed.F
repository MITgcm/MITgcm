#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif

      SUBROUTINE SHELFICE_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE SHELFICE_INIT_FIXED
C     | o Routine to initialize SHELFICE parameters and variables.
C     *============================================================*
C     | Initialize SHELFICE parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#ifdef ALLOW_COST
# include "cost.h"
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid ::  Number of this instance of SHELFICE_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     === Local variables ===
C     i, j, bi, bj :: Loop counters
C     SHI_minDepth :: minimum Ice-Shelf depth
C     msgBuf       :: Informational/error message buffer
      INTEGER i, j, bi, bj
      _RS SHI_minDepth
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_CTRL
      INTEGER k
#endif

#ifdef ALLOW_MNC
C     Initialize MNC variable information for SHELFICE
      IF ( useMNC .AND. shelfice_dump_mnc ) THEN
        CALL SHELFICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          shelficeMassInit   (i,j,bi,bj) = 0. _d 0
          shelficeLoadAnomaly(i,j,bi,bj) = 0. _d 0
          shelfIceMassDynTendency(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( SHELFICEloadAnomalyFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RL( SHELFICEloadAnomalyFile, ' ',
     &                      shelficeLoadAnomaly, 0, myThid )
      ENDIF
      IF ( SHELFICEmassFile.NE.' ' ) THEN
       CALL READ_FLD_XY_RL( SHELFICEmassFile, ' ',
     &                      shelficeMassInit, 0, myThid )
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1, sNy
          DO i = 1, sNx
           shelficeMassInit(i,j,bi,bj) =
     &         shelficeLoadAnomaly(i,j,bi,bj)*recip_gravity
     &       - rhoConst*Ro_surf(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      _EXCH_XY_RL( shelficeMassInit, myThid )

C-----------------------------------------------------------------------
C--   Initialize SHELFICE variables kTopC
C--   kTopC is the same as kSurfC, except outside ice-shelf area:
C--   kTop = 0 where there is no ice-shelf (where kSurfC=1)
C--   and over land (completely dry column) where kSurfC = Nr+1
C-----------------------------------------------------------------------

C--   Currently, there is no specific pkg/shelfice criteria to decide when very
C     thin ice-shelf vanishes: some limits come from ocean hFacMin/hFacMinDr;
C     but should depend on shelficeMass if SHELFICEMassStepping is used.
C     Here the SHI_minDepth threshold is just to get around machine round-off
C     isssues so does not need to be a run-time params.
      SHI_minDepth = 1. _d -6
      SHI_minDepth = rF(1) - drF(1)*SHI_minDepth
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        IF ( SHELFICEMassStepping ) THEN
C-      In this case, it is recommended to initialise ice-shelf Mass (from file)
C       And if updating "kTopC" is permitted (SHI_update_kTopC=T), kTopC will
C       be reset in SHELFICE_INIT_VARIA after loading shelficeMass from pickup
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &          shelficeMassInit(i,j,bi,bj).GT.zeroRL ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
           ELSE
            kTopC(i,j,bi,bj) = 0
           ENDIF
          ENDDO
         ENDDO
        ELSE
C-      no time-evolving Ice-Shelf: set kTopC according to Ro_surf
C       Note: use Ro_surf instead of R_shelfIce to stay consistent
C             with ocean mask & hFac, see issue #99
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &          Ro_surf(i,j,bi,bj).LT.SHI_minDepth ) THEN
c    &          R_shelfIce(i,j,bi,bj).LT.rF(1) ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
           ELSE
            kTopC(i,j,bi,bj) = 0
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_CTRL
C     maskSHI is a hack to play along with the general ctrl-package
C     infrastructure, where only the k=1 layer of a 3D mask is used
C     for 2D fields. We cannot use maskInC instead, because routines
C     like ctrl_get_gen and ctrl_set_unpack_xy require 3D masks.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           maskSHI(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( kTopC(i,j,bi,bj).GE.1
     &          .AND. maskC(i,j,k,bi,bj).NE.zeroRS ) THEN
            maskSHI(i,j,k,bi,bj) = 1. _d 0
            maskSHI(i,j,1,bi,bj) = 1. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_CTRL */

      IF ( debugLevel.GE.debLevC ) THEN
       WRITE(msgBuf,'(A)')
     &  'SHELFICE_INIT_FIXED: checking Ice-Shelf extension & kTopC'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,1PE18.10)')
     &  'SHELFICE_INIT_FIXED:  SHI_minDepth =', SHI_minDepth
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
c        DO j = 1-OLy, sNy+OLy
c         DO i = 1-OLx, sNx+OLx
         DO j = 1, sNy
          DO i = 1, sNx
           IF ( kTopC(i,j,bi,bj).NE.0 .AND.
     &          ( shelficeMassInit(i,j,bi,bj).LE.zeroRL
     &            .OR. R_shelfIce(i,j,bi,bj).GE.rF(1) ) ) THEN
             WRITE(standardMessageUnit,'(A,2I4,2I3,A,1P3E17.9)')
     &        ' Ice-Shelf (kTopC>0) in i,j,bi,bj=', i, j, bi, bj,
     &        ' but R_shelf,Ro_surf,MassIni=', R_shelfIce(i,j,bi,bj),
     &         Ro_surf(i,j,bi,bj), shelficeMassInit(i,j,bi,bj)
           ELSEIF ( kTopC(i,j,bi,bj).EQ.0 .AND.
     &          ( shelficeMassInit(i,j,bi,bj).GT.zeroRL
     &            .OR. R_shelfIce(i,j,bi,bj).LT.rF(1) ) ) THEN
             WRITE(standardMessageUnit,'(A,2I4,2I3,A,1P3E17.9)')
     &        ' no Ice-Shelf (kTopC=0) i,j,bi,bj=', i, j, bi, bj,
     &        ' but R_shelf,Ro_surf,MassIni=', R_shelfIce(i,j,bi,bj),
     &         Ro_surf(i,j,bi,bj), shelficeMassInit(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       WRITE(msgBuf,'(A)')
     &  'SHELFICE_INIT_FIXED: checking Ice-Shelf extension: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
c     IF ( SHELFICEloadAnomalyFile .EQ. ' ' ) THEN
C-   In case we need shelficeLoadAnomaly in phi0surf for initial pressure
C    calculation (if using selectP_inEOS_Zc=2 or 3)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           IF ( kTopC(i,j,bi,bj).EQ.0 ) THEN
            R_shelfIce         (i,j,bi,bj) = rF(1)
            shelficeMassInit   (i,j,bi,bj) = 0. _d 0
            shelficeLoadAnomaly(i,j,bi,bj) = 0. _d 0
           ELSE
            shelficeLoadAnomaly(i,j,bi,bj) = gravity
     &      *(shelficeMassInit(i,j,bi,bj)+rhoConst*Ro_surf(i,j,bi,bj))
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c     ELSE
c      _EXCH_XY_RS( shelficeLoadAnomaly, myThid )
c     ENDIF
      CALL WRITE_FLD_XY_RL ( 'shelficemassinit', ' ',
     &                       shelficeMassInit, 0, myThid )
      IF ( debugLevel.GE.debLevC ) THEN
       CALL WRITE_FLD_XY_RL( 'SHICE_pLoadAnom', ' ',
     I                       shelficeLoadAnomaly, -1, myThid )
      ENDIF

      IF ( SHELFICEMassStepping .AND.
     &     SHELFICEMassDynTendFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( SHELFICEMassDynTendFile, ' ',
     &                      shelfIceMassDynTendency, 0, myThid )
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       diagName  = 'SHIfwFlx'
       diagTitle = 'Ice shelf fresh water flux (positive upward)'
       diagUnits = 'kg/m^2/s        '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIhtFlx'
       diagTitle = 'Ice shelf heat flux  (positive upward)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHI_TauX'
       diagTitle =
     &     'Ice shelf bottom stress, zonal  comp., >0 increases uVel'
       diagUnits = 'N/m^2           '
       diagCode  = 'UU      L1      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHI_TauY'
       diagTitle =
     &     'Ice shelf bottom stress, merid. comp., >0 increases vVel'
       diagUnits = 'N/m^2           '
       diagCode  = 'VV      L1      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHIForcT'
       diagTitle = 'Ice shelf forcing for theta, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIForcS'
       diagTitle = 'Ice shelf forcing for salt, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgammT'
       diagTitle = 'Ice shelf exchange coefficient for theta'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgammS'
       diagTitle = 'Ice shelf exchange coefficient for salt'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHI_mass'
       diagTitle = 'dynamic ice shelf mass for surface load anomaly'
       diagUnits = 'kg/m^2          '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#if ( defined ALLOW_MOM_COMMON && defined ALLOW_MOM_TEND_EXTRA_DIAGS )
cHP adding diagnostic for drag from ice shelf to allow exp. dissip decomp
      diagName  = 'UShIDrag'
      diagTitle = 'U momentum tendency from ice shelf Drag'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VShIDrag'
      diagTitle = 'V momentum tendency from ice shelf Drag'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

#ifdef SHI_ALLOW_GAMMAFRICT
       diagName  = 'SHIuStar'
       diagTitle = 'Friction velocity at bottom of ice shelf'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHICDrag'
       diagTitle = 'Shelfice drag coefficient for u* parameterization'
       diagUnits = '1               '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

#ifdef ALLOW_SHELFICE_REMESHING
       diagName  = 'SHIRshel'
       diagTitle = 'depth of shelfice'
       diagUnits = 'm               '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

# ifdef ALLOW_AUTODIFF
#  ifndef SHI_ALLOW_GAMMAFRICT
       diagName  = 'ADJshict'
       diagTitle = 'dJ/dgammaT: Sens. to shelfice heat transfer coeff'
       diagUnits = 'dJ/(m/s)        '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'ADJshics'
       diagTitle = 'dJ/dgammaS: Sens. to shelfice salt transfer coeff'
       diagUnits = 'dJ/(m/s)        '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#  else
       diagName  = 'ADJshicd'
       diagTitle = 'dJ/dcDrag: Sensitivity to shelfice u* drag coeff'
       diagUnits = 'dJ/1            '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#  endif
# endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
