C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_fixed.F,v 1.30 2014/08/23 20:22:16 torge Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_INIT_FIXED( myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_INIT_FIXED
C     | o Initialization of sea ice model.
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
#ifdef SEAICE_ITD
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*15 HlimitMsgFormat
C     k - loop counter for ITD categories
      INTEGER k
      _RL tmpVar
#endif
C     i,j, bi,bj  :: Loop counters
c     INTEGER i, j, bi, bj
      INTEGER kSurface
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif
#ifdef SHORTWAVE_HEATING
cif   Helper variable for determining the fraction of sw radiation
cif   penetrating the model shallowest layer
      _RL dummyTime
      _RL swfracba(2)
      _RL tmpFac
#endif /* SHORTWAVE_HEATING */

      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     Initialize MNC variable information for SEAICE
      IF ( useMNC .AND.
     &    (seaice_tave_mnc.OR.seaice_dump_mnc.OR.SEAICE_mon_mnc)
     &   ) THEN
        CALL SEAICE_MNC_INIT( myThid )
      ENDIF

C     Only Master Thread updates parameter in commom block:
      _BEGIN_MASTER(myThid)

C     restart parameter
      SEAICEmomStartBDF = 0
      IF ( SEAICEuseBDF2 ) SEAICEmomStartBDF = nIter0

#ifdef SHORTWAVE_HEATING
       tmpFac    = -1.0
       dummyTime = 1.0
       swfracba(1) = ABS(rF(1))
       swfracba(2) = ABS(rF(2))
       CALL SWFRAC(
     I       2, tmpFac,
     U       swfracba,
     I       dummyTime, 0, myThid )
       SWFracB = swfracba(2)
#else /* SHORTWAVE_HEATING */
       SWFracB = 0. _d 0
#endif /* SHORTWAVE_HEATING */

C--   Set mcPheePiston coeff (if still unset)
      IF ( SEAICE_mcPheePiston.EQ.UNSET_RL ) THEN
        IF ( SEAICE_availHeatFrac.NE.UNSET_RL ) THEN
          SEAICE_mcPheePiston = SEAICE_availHeatFrac
     &                        * drF(kSurface)/SEAICE_deltaTtherm
        ELSE
          SEAICE_mcPheePiston = MCPHEE_TAPER_FAC
     &                        * STANTON_NUMBER * USTAR_BASE
          SEAICE_mcPheePiston = MIN( SEAICE_mcPheePiston,
     &                          drF(kSurface)/SEAICE_deltaTtherm )
        ENDIF
      ENDIF

C--   SItracer specifications for basic tracers
#ifdef ALLOW_SITRACER
      DO iTracer = 1, SItrNumInUse
C     "ice concentration" tracer that should remain .EQ.1.
       IF (SItrName(iTracer).EQ.'one') THEN
         SItrFromOcean0(iTracer)    =ONE
         SItrFromFlood0(iTracer)    =ONE
         SItrExpand0(iTracer)       =ONE
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
C     age tracer: no age in ocean, or effect from ice cover changes
       IF (SItrName(iTracer).EQ.'age') THEN
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIf
C     salinity tracer:
       IF (SItrName(iTracer).EQ.'salinity') THEN
         SItrMate(iTracer)          ='HEFF'
         SItrExpand0(iTracer)       =ZERO
         IF ( SEAICE_salinityTracer ) THEN
           SEAICE_salt0    = ZERO
           SEAICE_saltFrac = ZERO
         ENDIF
       ENDIF
C     simple, made up, ice surface roughness index prototype
       IF (SItrName(iTracer).EQ.'ridge') THEN
         SItrMate(iTracer)          ='AREA'
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
#ifdef SEAICE_GREASE
C     grease ice tracer:
c     (Smedrud and Martin, 2014, Ann. Glac.)
       IF (SItrName(iTracer).EQ.'grease') THEN
         SItrMate(iTracer)          ='HEFF'
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
#endif /* SEAICE_GREASE */
      ENDDO
#endif /* ALLOW_SITRACER */

#ifdef SEAICE_ITD
C     use Equ. 22 of Lipscomb et al. (2001, JGR) to generate ice
C     thickness category limits:
C     - dependends on given number of categories nITD
C     - choose between original parameters of Lipscomb et al. (2001):
C       c1=3.0/N, c2=15*c1, c3=3.0
C       or emphasize thin end of ITD (in order to enhance ice growth):
C       c1=1.5/N, c2=42*c1, c3=3.3
C       -> HINT: set parameters c1, c2 and c3 in seaice_readparms.F
      Hlimit(0) = 0. _d 0
      IF ( nITD.GT.1 ) THEN
       tmpVar = nITD
       tmpVar = oneRL / tmpVar
       Hlimit_c1 = Hlimit_c1*tmpVar
       Hlimit_c2 = Hlimit_c2*Hlimit_c1
       DO k=1,nITD-1
        Hlimit(k) = Hlimit(k-1)
     &            + Hlimit_c1
     &            + Hlimit_c2
     &  *( oneRL + TANH( Hlimit_c3 *( FLOAT(k-1)*tmpVar - oneRL ) ) )
       ENDDO
      ENDIF
C     thickest category is unlimited
      Hlimit(nITD) = 999.9 _d 0

      WRITE(msgBuf,'(A,I2,A)')
     &     ' SEAICE_INIT_FIXED: ', nITD,
     &     ' sea ice thickness categories'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F6.2,F6.1)'
      WRITE(msgBuf,HlimitMsgFormat)
     &     ' SEAICE_INIT_FIXED: Hlimit = ', (Hlimit(k),k=0,nITD)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif /* SEAICE_ITD */

C     Only Master Thread updates parameter in common block:
      _END_MASTER(myThid)

#ifdef SEAICE_ALLOW_JFNK
C     Only Master Thread updates parameter in common block:
      _BEGIN_MASTER(myThid)
C     initialise some diagnostic counters for the JFNK solver
      totalNewtonIters   = 0
      totalNewtonFails   = 0
      totalKrylovIters   = 0
      totalKrylovFails   = 0
      totalJFNKtimeSteps = 0
      _END_MASTER(myThid)
C     this cannot be done here, because globalArea is only defined
C     after S/R PACKAGES_INIT_FIXED, so we move it to S/R SEAICE_INIT_VARIA
CML      CALL SEAICE_MAP2VEC( nVec, rAw, rAs,
CML     &     scalarProductMetric, .TRUE., myThid )
CML      DO bj=myByLo(myThid),myByHi(myThid)
CML       DO bi=myBxLo(myThid),myBxHi(myThid)
CML        DO i=1,nVec
CML         scalarProductMetric(i,1,bi,bj) =
CML     &        scalarProductMetric(i,1,bi,bj)/globalArea
CML        ENDDO
CML       ENDDO
CML      ENDDO
#endif /* SEAICE_ALLOW_JFNK */

C--   all threads wait for master to finish initialisation of shared params
      _BARRIER

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL SEAICE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

C--   Summarise pkg/seaice configuration
      CALL SEAICE_SUMMARY( myThid )

      RETURN
      END
