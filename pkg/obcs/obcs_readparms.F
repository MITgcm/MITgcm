C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_readparms.F,v 1.48 2014/11/25 01:07:23 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_READPARMS
C     !INTERFACE:
      SUBROUTINE OBCS_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_READPARMS
C     | o Routine to initialize OBCS variables and constants.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_SEAICE.h"
#ifdef ALLOW_ORLANSKI
#include "ORLANSKI.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      :: Informational/error message buffer
C     iUnit       :: Work variable for IO unit number
C  OB_indexUnset  :: OB index value for places where no OB is defined
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER i, j
      INTEGER bi, bj, iG, jG, iGm, jGm
      INTEGER OB_indexUnset
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */

C These are input arrays (of integers) that contain the *absolute*
C computational index of an open-boundary (OB) point.
C A zero (0) element means there is no corresponding OB in that column/row.
C The computational coordinate refers to "tracer" cells.
C For a northern/southern OB, the OB V point is to the south/north.
C For an eastern/western OB, the OB U point is to the west/east.
C eg.
C     OB_Jnorth(3)=34  means that:
C          T( 3 ,34) is a an OB point
C          U(3:4,34) is a an OB point
C          V( 4 ,34) is a an OB point
C while
C     OB_Jsouth(3)=1  means that:
C          T( 3 ,1) is a an OB point
C          U(3:4,1) is a an OB point
C          V( 4 ,2) is a an OB point
C
C For convenience, negative values for Jnorth/Ieast refer to
C points relative to the Northern/Eastern edges of the model
C eg. OB_Jnorth(3)=-1  means that the point (3,Ny) is a northern O-B.
C
C With exch2, the global domain used for specifying the boundary (and
C boundary value files) is different for N,S and E,W boundaries:
C - for N,S, the facets are stacked in x (like W2_mapIO=-1)
C - for E,W, the facets are stacked in y, so that E,W boundaries in
C   different facets cannot have the same I
C
C OB_Jnorth(W2_maxXStackNx) :: global index array of northern open-boundary point
C OB_Jsouth(W2_maxXStackNx) :: global index array of southern open-boundary point
C OB_Ieast(W2_maxYStackNy)  :: global index array of eastern  open-boundary point
C OB_Iwest(W2_maxYStackNy)  :: global index array of western  open-boundary point

#ifdef ALLOW_EXCH2
      INTEGER OB_Jnorth(W2_maxXStackNx)
      INTEGER OB_Jsouth(W2_maxXStackNx)
      INTEGER OB_Ieast(W2_maxYStackNy)
      INTEGER OB_Iwest(W2_maxYStackNy)
#else
      INTEGER OB_Jnorth(Nx)
      INTEGER OB_Jsouth(Nx)
      INTEGER OB_Ieast(Ny)
      INTEGER OB_Iwest(Ny)
#endif

C With exch2, we use different global domains for specifying
C N,S resp. E,W boundaries (and for reading in the corresponding data):
C
C     OBNS_Nx ::  width of global domain for OB_Jnorth, OB_Jsouth
C     OBNS_Ny :: height of global domain for OB_Jnorth, OB_Jsouth
C     OBEW_Nx ::  width of global domain for OB_Ieast, OB_Iwest
C     OBEW_Ny :: height of global domain for OB_Ieast, OB_Iwest

      INTEGER OBNS_Nx, OBNS_Ny
      INTEGER OBEW_Nx, OBEW_Ny
CEOP

C     retired parameters
      INTEGER nRetired
      LOGICAL useOBCSYearlyFields

      NAMELIST /OBCS_PARM01/
     &          insideOBmaskFile,
     &          OBNconnectFile, OBSconnectFile,
     &          OBEconnectFile, OBWconnectFile,
     &          OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     &          useOrlanskiNorth,useOrlanskiSouth,
     &          useOrlanskiEast,useOrlanskiWest,
     &          useStevensNorth,useStevensSouth,
     &          useStevensEast,useStevensWest,
     &          OBCS_u1_adv_T, OBCS_u1_adv_S,
     &          OBNuFile,OBNvFile,OBNtFile,OBNsFile,OBNaFile,OBNhFile,
     &          OBSuFile,OBSvFile,OBStFile,OBSsFile,OBSaFile,OBShFile,
     &          OBEuFile,OBEvFile,OBEtFile,OBEsFile,OBEaFile,OBEhFile,
     &          OBWuFile,OBWvFile,OBWtFile,OBWsFile,OBWaFile,OBWhFile,
     &          OBNslFile,OBSslFile,OBEslFile,OBWslFile,
     &          OBNsnFile,OBSsnFile,OBEsnFile,OBWsnFile,
     &          OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &          OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile,
     &          OBNetaFile, OBSetaFile, OBEetaFile, OBWetaFile,
     &          OBNwFile, OBSwFile, OBEwFile, OBWwFile,
     &          OBNAmFile, OBSAmFile, OBEAmFile, OBWAmFile,
     &          OBNPhFile, OBSPhFile, OBEPhFile, OBWPhFile,
#ifdef ALLOW_PTRACERS
     &          OBCS_u1_adv_Tr,
     &          OBNptrFile,OBSptrFile,OBEptrFile,OBWptrFile,
#endif
     &          useOBCSsponge, useSeaiceSponge,
     &          OBCSsponge_N , OBCSsponge_S,
     &          OBCSsponge_E, OBCSsponge_W,
     &          OBCSsponge_UatNS, OBCSsponge_UatEW,
     &          OBCSsponge_VatNS, OBCSsponge_VatEW,
     &          OBCSsponge_Theta, OBCSsponge_Salt, useLinearSponge,
     &          useOBCSbalance, useOBCStides, useOBCSprescribe,
     &          OBCS_balanceFacN, OBCS_balanceFacS,
     &          OBCS_balanceFacE, OBCS_balanceFacW,
     &          useOBCSYearlyFields, OBCSfixTopo,
     &          OBCS_uvApplyFac,
     &          OBCS_monitorFreq, OBCS_monSelect, OBCSprintDiags,
     &          tidalPeriod

#ifdef ALLOW_ORLANSKI
      NAMELIST /OBCS_PARM02/
     & CMAX, cvelTimeScale, CFIX, useFixedCEast, useFixedCWest
#endif

#ifdef ALLOW_OBCS_SPONGE
      NAMELIST /OBCS_PARM03/
     &          Urelaxobcsinner,Urelaxobcsbound,
     &          Vrelaxobcsinner,Vrelaxobcsbound,
     &          spongeThickness
#endif
#ifdef ALLOW_OBCS_STEVENS
      NAMELIST /OBCS_PARM04/
     &          TrelaxStevens,SrelaxStevens,
     &          useStevensPhaseVel,useStevensAdvection
#endif /* ALLOW_OBCS_STEVENS */
#ifdef ALLOW_OBCS_SEAICE_SPONGE
      NAMELIST /OBCS_PARM05/
     &           Arelaxobcsinner, Arelaxobcsbound,
     &           Hrelaxobcsinner, Hrelaxobcsbound,
     &          SLrelaxobcsinner,SLrelaxobcsbound,
     &          SNrelaxobcsinner,SNrelaxobcsbound,
     &          seaiceSpongeThickness
#endif

      IF ( .NOT.useOBCS ) THEN
C-    pkg OBCS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.obcs is found
         CALL PACKAGES_UNUSED_MSG( 'useOBCS', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

#ifdef ALLOW_EXCH2
      OBNS_Nx = exch2_xStack_Nx
      OBNS_Ny = exch2_xStack_Ny
      OBEW_Nx = exch2_yStack_Nx
      OBEW_Ny = exch2_yStack_Ny
#else
      OBNS_Nx = Nx
      OBNS_Ny = Ny
      OBEW_Nx = Nx
      OBEW_Ny = Ny
#endif

C--   Default flags and values for OBCS
      OB_indexNone  = -99
      OB_indexUnset = 0
      insideOBmaskFile = ' '
      OBNconnectFile   = ' '
      OBSconnectFile   = ' '
      OBEconnectFile   = ' '
      OBWconnectFile   = ' '
      DO i=1,OBNS_Nx
       OB_Jnorth(i) = OB_indexUnset
       OB_Jsouth(i) = OB_indexUnset
      ENDDO
      DO j=1,OBEW_Ny
       OB_Ieast(j) = OB_indexUnset
       OB_Iwest(j) = OB_indexUnset
      ENDDO
      OBCS_indexStatus = 0
      useOrlanskiNorth   =.FALSE.
      useOrlanskiSouth   =.FALSE.
      useOrlanskiEast    =.FALSE.
      useOrlanskiWest    =.FALSE.
      useStevensNorth    =.FALSE.
      useStevensSouth    =.FALSE.
      useStevensEast     =.FALSE.
      useStevensWest     =.FALSE.
      useStevensPhaseVel =.TRUE.
      useStevensAdvection=.TRUE.
      useOBCSsponge      =.FALSE.
      useSeaiceSponge    =.FALSE.
      OBCSsponge_N       =.TRUE.
      OBCSsponge_S       =.TRUE.
      OBCSsponge_E       =.TRUE.
      OBCSsponge_W       =.TRUE.
      OBCSsponge_UatNS   =.TRUE.
      OBCSsponge_UatEW   =.TRUE.
      OBCSsponge_VatNS   =.TRUE.
      OBCSsponge_VatEW   =.TRUE.
      OBCSsponge_Theta   =.TRUE.
      OBCSsponge_Salt    =.TRUE.
      useLinearSponge    =.FALSE.
      useOBCSbalance     =.FALSE.
      OBCS_balanceFacN   = 1. _d 0
      OBCS_balanceFacS   = 1. _d 0
      OBCS_balanceFacE   = 1. _d 0
      OBCS_balanceFacW   = 1. _d 0
      OBCS_u1_adv_T      = 0
      OBCS_u1_adv_S      = 0
      useOBCSprescribe   =.FALSE.
      OBCSfixTopo        =.FALSE.
      OBCS_uvApplyFac    = 1. _d 0
      OBCS_monitorFreq   = monitorFreq
      OBCS_monSelect     = 0
      OBCSprintDiags     = debugLevel.GE.debLevC

      OBNuFile = ' '
      OBNvFile = ' '
      OBNtFile = ' '
      OBNsFile = ' '
      OBNaFile = ' '
      OBNhFile = ' '
      OBNslFile = ' '
      OBNsnFile = ' '
      OBNuiceFile = ' '
      OBNviceFile = ' '
      OBSuFile = ' '
      OBSvFile = ' '
      OBStFile = ' '
      OBSsFile = ' '
      OBSaFile = ' '
      OBShFile = ' '
      OBSslFile = ' '
      OBSsnFile = ' '
      OBSuiceFile = ' '
      OBSviceFile = ' '
      OBEuFile = ' '
      OBEvFile = ' '
      OBEtFile = ' '
      OBEsFile = ' '
      OBEaFile = ' '
      OBEhFile = ' '
      OBEslFile = ' '
      OBEsnFile = ' '
      OBEuiceFile = ' '
      OBEviceFile = ' '
      OBWuFile = ' '
      OBWvFile = ' '
      OBWtFile = ' '
      OBWsFile = ' '
      OBWaFile = ' '
      OBWhFile = ' '
      OBWslFile = ' '
      OBWsnFile = ' '
      OBWuiceFile = ' '
      OBWviceFile = ' '
      OBNetaFile = ' '
      OBSetaFile = ' '
      OBEetaFile = ' '
      OBWetaFile = ' '
      OBNwFile = ' '
      OBSwFile = ' '
      OBEwFile = ' '
      OBWwFile = ' '
      OBNAmFile = ' '
      OBSAmFile = ' '
      OBEAmFile = ' '
      OBWAmFile = ' '
      OBNPhFile = ' '
      OBSPhFile = ' '
      OBEPhFile = ' '
      OBWPhFile = ' '
#ifdef ALLOW_PTRACERS
      DO iTracer = 1, PTRACERS_num
       OBCS_u1_adv_Tr(iTracer) = 0
       OBNptrFile(iTracer) = ' '
       OBSptrFile(iTracer) = ' '
       OBEptrFile(iTracer) = ' '
       OBWptrFile(iTracer) = ' '
      ENDDO
#endif
#ifdef ALLOW_OBCS_TIDES
      DO i = 1, tidalComponents
       tidalPeriod(i) = 0. _d 0
      ENDDO
#endif
C-    retired parameters
      nRetired = 0
      useOBCSYearlyFields = .FALSE.

C     Open and read the data.obcs file
      WRITE(msgBuf,'(A)') ' OBCS_READPARMS: opening data.obcs'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.obcs', 'OBCS_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=OBCS_PARM01)

C-    retired parameter
      IF ( useOBCSYearlyFields ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'OBCS_READPARMS: "useOBCSYearlyFields"',
     &  ' no longer allowed in file "data.obcs"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'OBCS_READPARMS: ',
     &  ' was moved to "data.exf", namelist: "EXF_NML_OBCS"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

#ifdef ALLOW_ORLANSKI
C     Default Orlanski radiation parameters
      CMAX = 0.45 _d 0 /* maximum allowable phase speed-CFL for AB-II */
      cvelTimeScale = 2000.0 _d 0 /* Averaging period for phase speed (s) */
      CFIX = 0.8 _d 0 /* Fixed boundary phase speed in m/s */
      useFixedCEast=.FALSE.
      useFixedCWest=.FALSE.
      IF (useOrlanskiNorth.OR.
     &    useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.
     &    useOrlanskiWest)
     & READ(UNIT=iUnit,NML=OBCS_PARM02)
#endif

#ifdef ALLOW_OBCS_SPONGE
C     Default sponge layer parameters:
C     sponge layer is turned off by default
      spongeThickness = 0
      Urelaxobcsinner = 0. _d 0
      Urelaxobcsbound = 0. _d 0
      Vrelaxobcsinner = 0. _d 0
      Vrelaxobcsbound = 0. _d 0
CML this was the previous default in units of days
CML      spongeThickness = 2
CML      Urelaxobcsinner = 5. _d 0
CML      Urelaxobcsbound = 1. _d 0
CML      Vrelaxobcsinner = 5. _d 0
CML      Vrelaxobcsbound = 1. _d 0
      IF (useOBCSsponge)
     & READ(UNIT=iUnit,NML=OBCS_PARM03)
#endif
#ifdef ALLOW_OBCS_STEVENS
      TrelaxStevens   = 0. _d 0
      SrelaxStevens   = 0. _d 0
      IF (      useStevensNorth .OR. useStevensSouth
     &     .OR. useStevensEast  .OR. useStevensWest  )
     & READ(UNIT=iUnit,NML=OBCS_PARM04)
#endif
#ifdef ALLOW_OBCS_SEAICE_SPONGE
C     Default seaice sponge layer parameters:
C     seaice sponge layer is turned off by default
      seaiceSpongeThickness = 0
      Arelaxobcsinner  = 0. _d 0
      Arelaxobcsbound  = 0. _d 0
      Hrelaxobcsinner  = 0. _d 0
      Hrelaxobcsbound  = 0. _d 0
      SLrelaxobcsinner = 0. _d 0
      SLrelaxobcsbound = 0. _d 0
      SNrelaxobcsinner = 0. _d 0
      SNrelaxobcsbound = 0. _d 0
      IF (useSeaiceSponge) READ(UNIT=iUnit,NML=OBCS_PARM05)
#endif

      WRITE(msgBuf,'(A)') ' OBCS_READPARMS: finished reading data.obcs'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Close the open data file
      CLOSE(iUnit)

C-    retired parameter
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_READPARMS: reading parameter file "data.obcs"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'some out of date parameters were found in namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OBCS_READPARMS'
      ENDIF

C-    Account for periodicity if negative indices were supplied
      DO j=1,OBEW_Ny
       IF ( OB_Ieast(j) .NE.OB_indexUnset .AND.
     &      OB_Ieast(j) .LT.0 ) OB_Ieast(j) = OB_Ieast(j)+OBEW_Nx+1
      ENDDO
      DO i=1,OBNS_Nx
       IF ( OB_Jnorth(i).NE.OB_indexUnset .AND.
     &      OB_Jnorth(i).LT.0 ) OB_Jnorth(i)=OB_Jnorth(i)+OBNS_Ny+1
      ENDDO
      IF ( debugLevel.GE.debLevA ) THEN
        CALL WRITE_0D_I( OB_indexUnset, INDEX_NONE,' OB_indexUnset =',
     &                   ' /* unset OB index value (i.e. no OB) */')
c       write(*,*) 'OB Jn =',OB_Jnorth
c       write(*,*) 'OB Js =',OB_Jsouth
c       write(*,*) 'OB Ie =',OB_Ieast
c       write(*,*) 'OB Iw =',OB_Iwest
        WRITE(msgBuf,'(A)') ' Northern OB global indices : OB_Jnorth ='
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_LIST_I( OB_Jnorth, 1, OBNS_Nx, INDEX_I,
     &                    .FALSE., .TRUE., standardMessageUnit )
        WRITE(msgBuf,'(A)') ' Southern OB global indices : OB_Jsouth ='
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_LIST_I( OB_Jsouth, 1, OBNS_Nx, INDEX_I,
     &                    .FALSE., .TRUE., standardMessageUnit )
        WRITE(msgBuf,'(A)') ' Eastern  OB global indices : OB_Ieast ='
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_LIST_I( OB_Ieast, 1, OBEW_Ny, INDEX_J,
     &                    .FALSE., .TRUE., standardMessageUnit )
        WRITE(msgBuf,'(A)') ' Western  OB global indices : OB_Iwest ='
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_LIST_I( OB_Iwest, 1, OBEW_Ny, INDEX_J,
     &                    .FALSE., .TRUE., standardMessageUnit )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C--   Continue master-thread only since global OB indices (+ OB_indexUnset)
C     are not shared (not in common block).

C--   Calculate the tiled OB index arrays OB_Jn/Js/Ie/Iw here from the
C     global indices OB_Jnorth/Jsouth/Ieast/Iwest.
C     Note: This part of the code has been moved from obcs_init_fixed
C     to this routine because the OB_Jn/Js/Ie/Iw index arrays are
C     required by INI_DEPTH (calling OBCS_CHECK_DEPTHS, but only needs
C     valid interior indices) which is called before OBCS_INIT_FIXED.
      DO bj = 1, nSy
       DO bi = 1, nSx

        DO i=1-OLx,sNx+OLx
         OB_Jn(i,bi,bj) = OB_indexNone
         OB_Js(i,bi,bj) = OB_indexNone
        ENDDO

        DO j=1-OLy,sNy+OLy
         OB_Ie(j,bi,bj) = OB_indexNone
         OB_Iw(J,bi,bj) = OB_indexNone
        ENDDO

#ifdef ALLOW_EXCH2

C--   We apply OBCS only inside tile and exchange overlaps later
        tN = W2_myTileList(bi,bj)
C 1. N/S boundaries
C     convert from local y index J to global y index jG
C     for N/S boundaries, we use faces stacked in x direction
        DO i=1,sNx
         iG = exch2_txXStackLo(tN)+i-1
C-    Northern boundaries
         IF ( OB_Jnorth(iG).NE.OB_indexUnset ) THEN
C     loop over local y index J
          DO j=1,sNy+1
           jG = exch2_tyXStackLo(tN)+j-1
           IF ( jG.EQ.OB_Jnorth(iG) ) OB_Jn(i,bi,bj) = j
          ENDDO
         ENDIF
C-    Southern boundaries
         IF ( OB_Jsouth(iG).NE.OB_indexUnset ) THEN
          DO j=0,sNy
           jG = exch2_tyXStackLo(tN)+j-1
           IF ( jG.EQ.OB_Jsouth(iG) ) OB_Js(i,bi,bj) = j
          ENDDO
         ENDIF
        ENDDO
C 2. E/W boundaries
C     convert from local y index J to global y index jG
c     for E/W boundaries, we use faces stacked in y direction
        DO j=1,sNy
         jG = exch2_tyYStackLo(tN)+j-1
C-    Eastern  boundaries
         IF ( OB_Ieast(jG).NE.OB_indexUnset ) THEN
C     loop over local x index I
          DO i=1,sNx+1
           iG = exch2_txYStackLo(tN)+i-1
           IF ( iG.EQ.OB_Ieast(jG) )  OB_Ie(j,bi,bj) = i
          ENDDO
         ENDIF
C-    Western  boundaries
         IF ( OB_Iwest(jG).NE.OB_indexUnset ) THEN
          DO i=0,sNx
           iG = exch2_txYStackLo(tN)+i-1
           IF ( iG.EQ.OB_Iwest(jG) )  OB_Iw(j,bi,bj) = i
          ENDDO
         ENDIF
        ENDDO

C-    OB-index tiled-arrays are set for tile-interior region
         OBCS_indexStatus = 1

#else /* ALLOW_EXCH2 */

        DO j=1-OLy,sNy+OLy
C     convert from local y index J to global y index jG
         jG = myYGlobalLo+(bj-1)*sNy+j-1
C     use periodicity to deal with out of range points caused by the overlaps.
C     they will be excluded by the mask in any case, but this saves array
C     out-of-bounds errors here.
         jGm = 1+MOD( jG-1+Ny , Ny )
C-    Eastern  boundaries
C     OB_Ieast(jGm) allows to put the eastern boundary at variable x locations
         IF ( OB_Ieast(jGm).NE.OB_indexUnset ) THEN
C     loop over local x index I
          DO i=1,sNx+1
           iG = myXGlobalLo+(bi-1)*sNx+i-1
           iGm = 1+MOD( iG-1+Nx , Nx )
           IF ( iG.EQ.OB_Ieast(jGm) )  OB_Ie(j,bi,bj) = i
          ENDDO
         ENDIF
C-    Western  boundaries
         IF ( OB_Iwest(jGm).NE.OB_indexUnset ) THEN
          DO i=0,sNx
           iG = myXGlobalLo+(bi-1)*sNx+i-1
           iGm = 1+MOD( iG-1+Nx , Nx )
           IF ( iG.EQ.OB_Iwest(jGm) )  OB_Iw(j,bi,bj) = i
          ENDDO
         ENDIF
        ENDDO

        DO i=1-OLx,sNx+OLx
         iG = myXGlobalLo+(bi-1)*sNx+i-1
         iGm = 1+MOD( iG-1+Nx , Nx )
C-    Northern boundaries
C     OB_Jnorth(iG) allows to put the northern boundary at variable y locations
         IF ( OB_Jnorth(iGm).NE.OB_indexUnset ) THEN
          DO j=1,sNy+1
           jG = myYGlobalLo+(bj-1)*sNy+j-1
           jGm = 1+MOD( jG-1+Ny , Ny )
           IF ( jG.EQ.OB_Jnorth(iGm) ) OB_Jn(i,bi,bj) = j
          ENDDO
         ENDIF
C-    Southern boundaries
         IF ( OB_Jsouth(iGm).NE.OB_indexUnset ) THEN
          DO j=0,sNy
           jG = myYGlobalLo+(bj-1)*sNy+j-1
           jGm = 1+MOD( jG-1+Ny , Ny )
           IF ( jG.EQ.OB_Jsouth(iGm) ) OB_Js(i,bi,bj) = j
          ENDDO
         ENDIF
        ENDDO

C-    OB-index tiled-arrays are set for interior and overlap regions
        OBCS_indexStatus = 2

#endif /* ALLOW_EXCH2 */

C     bi,bj-loops
       ENDDO
      ENDDO

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
C     and tiled OB indices to be set.
      _BARRIER

#endif /* ALLOW_OBCS */
      RETURN
      END
