#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
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
      INTEGER i, j, bi, bj
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
# ifdef ALLOW_SHIFWFLX_COST_CONTRIBUTION
      _RL dummy
# endif
#endif

#ifdef ALLOW_MNC
C     Initialize MNC variable information for SHELFICE
      IF ( useMNC .AND. (shelfice_tave_mnc.OR.shelfice_dump_mnc)
     &    ) THEN
        CALL SHELFICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

C-----------------------------------------------------------------------
C--   Initialize SHELFICE variables kTopC
C--   kTopC is the same as kSurfC, except outside ice-shelf area:
C--   kTop = 0 where there is no ice-shelf (where kSurfC=1)
C--   and over land (completely dry column) where kSurfC = Nr+1
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &         Ro_surf(i,j,bi,bj).LT.rF(1) ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          ELSE
            kTopC(i,j,bi,bj) = 0
          ENDIF
          shelficeMassInit   (i,j,bi,bj) = 0. _d 0
          shelficeLoadAnomaly(i,j,bi,bj) = 0. _d 0
          shelfIceMassDynTendency(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
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
           IF ( Ro_surf(i,j,bi,bj).LT.rF(1)
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

#ifdef ALLOW_COST
#if  (defined (ALLOW_SHIFWFLX_COST_CONTRIBUTION) && \
      defined (ALLOW_SHIFWFLX_CONTROL))
      IF ( shifwflx_errfile .NE. ' ' ) THEN
       CALL READ_REC_XY_RL( shifwflx_errfile, wshifwflx, 1, 0, myThid )
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
c--   Test for missing values.
          IF (wshifwflx(i,j,bi,bj) .LT. -9900.) THEN
           wshifwflx(i,j,bi,bj) = 0. _d 0
          ENDIF
c--   use weight as mask
          wshifwflx(i,j,bi,bj) =
     &         max(wshifwflx(i,j,bi,bj),wshifwflx0)
     &         *maskSHI(i,j,1,bi,bj)
          IF (wshifwflx(i,j,bi,bj) .NE. 0.) THEN
           wshifwflx(i,j,bi,bj) =
     &          1./wshifwflx(i,j,bi,bj)/wshifwflx(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL ACTIVE_WRITE_XY_LOC( 'wshifwflx', wshifwflx,
     &                          1, 0, myThid, dummy )
#endif /* ALLOW_SHIFWFLX_COST_CONTRIBUTION and ALLOW_SHIFWFLX_CONTROL */
#endif /* ALLOW_COST */

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
      CALL WRITE_FLD_XY_RL ( 'shelficemassinit', ' ',
     &                       shelficeMassInit, 0, myThid )

c     IF ( SHELFICEloadAnomalyFile .EQ. ' ' ) THEN
C-   In case we need shelficeLoadAnomaly in phi0surf for initial pressure
C    calculation (if using selectP_inEOS_Zc=2 or 3)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           shelficeLoadAnomaly(i,j,bi,bj) = gravity
     &      *(shelficeMassInit(i,j,bi,bj)+rhoConst*Ro_surf(i,j,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c     ELSE
c      _EXCH_XY_RS( shelficeLoadAnomaly, myThid )
c     ENDIF
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

       diagName  = 'SHIuStar'
       diagTitle = 'Friction velocity at bottom of ice shelf'
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

       diagName  = 'SHICDrag'
       diagTitle = 'Shelfice drag coefficient for u* parameterization'
       diagUnits = '1             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

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
       diagUnits = 'dJ/1          '
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
