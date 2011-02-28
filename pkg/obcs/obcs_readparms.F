C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_readparms.F,v 1.30 2011/02/28 15:22:09 jmc Exp $
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
#include "OBCS.h"
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
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER I, J
      INTEGER bi, bj, iG, jG, iGm, jGm
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

      COMMON/OBCS_GLOBAL/ OB_Jnorth, OB_Jsouth, OB_Ieast, OB_Iwest
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

#ifdef ALLOW_EXCH2
C     buf :: used to exchange OB_Jnorth, ...
      _RS buf(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
#endif
CEOP

      NAMELIST /OBCS_PARM01/
     &          OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     &          useOrlanskiNorth,useOrlanskiSouth,
     &          useOrlanskiEast,useOrlanskiWest,
     &          useStevensNorth,useStevensSouth,
     &          useStevensEast,useStevensWest,
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
#ifdef ALLOW_PTRACERS
     &          OBNptrFile,OBSptrFile,OBEptrFile,OBWptrFile,
#endif
     &          useOBCSsponge, useOBCSbalance, useOBCSprescribe,
     &          OBCS_balanceFacN, OBCS_balanceFacS,
     &          OBCS_balanceFacE, OBCS_balanceFacW,
     &          useOBCSYearlyFields, OBCSfixTopo,
     &          OBCS_monitorFreq, OBCS_monSelect, OBCSprintDiags

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
      DO I=1,OBNS_Nx
       OB_Jnorth(I)=0
       OB_Jsouth(I)=0
      ENDDO
      DO J=1,OBEW_Ny
       OB_Ieast(J)=0
       OB_Iwest(J)=0
      ENDDO
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
      useOBCSbalance     =.FALSE.
      OBCS_balanceFacN   = 1. _d 0
      OBCS_balanceFacS   = 1. _d 0
      OBCS_balanceFacE   = 1. _d 0
      OBCS_balanceFacW   = 1. _d 0
      useOBCSprescribe   =.FALSE.
      useOBCSYearlyFields=.FALSE.
      OBCSfixTopo        =.TRUE.
      OBCS_monitorFreq   = monitorFreq
      OBCS_monSelect     = 0
      OBCSprintDiags     = debugLevel.GE.debLevB

      OBNuFile = ' '
      OBNvFile = ' '
      OBNtFile = ' '
      OBNsFile = ' '
      OBNaFile = ' '
      OBNslFile = ' '
      OBNsnFile = ' '
      OBNuiceFile = ' '
      OBNviceFile = ' '
      OBNhFile = ' '
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
#ifdef ALLOW_PTRACERS
      DO iTracer = 1, PTRACERS_num
       OBNptrFile(iTracer) = ' '
       OBSptrFile(iTracer) = ' '
       OBEptrFile(iTracer) = ' '
       OBWptrFile(iTracer) = ' '
      ENDDO
#endif

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

#ifdef ALLOW_ORLANSKI
C     Default Orlanski radiation parameters
      CMAX = 0.45 _d 0 /* maximum allowable phase speed-CFL for AB-II */
      cvelTimeScale = 2000.0 _d 0 /* Averaging period for phase speed in sec. */
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

      WRITE(msgBuf,'(A)') ' OBCS_READPARMS: finished reading data.obcs'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Close the open data file
      CLOSE(iUnit)

C-    Account for periodicity if negative indices were supplied
      DO J=1,OBEW_Ny
       IF (OB_Ieast(J).LT.0) OB_Ieast(J)=OB_Ieast(J)+OBEW_Nx+1
      ENDDO
      DO I=1,OBNS_Nx
       IF (OB_Jnorth(I).LT.0) OB_Jnorth(I)=OB_Jnorth(I)+OBNS_Ny+1
      ENDDO
      IF ( debugLevel.GE.debLevA ) THEN
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

      _END_MASTER(myThid)
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Calculate the tiled index arrays OB_Jn/Js/Ie/Iw here from the
C     global arrays OB_Jnorth/Jsouth/Ieast/Iwest.
C     Note: This part of the code has been moved from obcs_init_fixed to
C     routine routine because the OB_Jn/Js/Ie/Iw index arrays are
C     required by ini_depth which is called before obcs_init_fixed
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO I=1-Olx,sNx+Olx
         OB_Jn(I,bi,bj)=0
         OB_Js(I,bi,bj)=0
        ENDDO

        DO J=1-Oly,sNy+Oly
         OB_Ie(J,bi,bj)=0
         OB_Iw(J,bi,bj)=0
        ENDDO

#ifdef ALLOW_EXCH2
C       We apply OBCS only inside tile and exchange overlaps later
        tN = W2_myTileList(bi,bj)
C 1. N/S boundaries
        DO J=1,sNy
C   convert from local y index J to global y index jG
c   for N/S boundaries, we use faces stacked in x direction
         jG = exch2_tyXStackLo(tN)+J-1
C   loop over local x index I
         DO I=1,sNx
          iG = exch2_txXStackLo(tN)+I-1
          IF (jG.EQ.OB_Jnorth(iG)) OB_Jn(I,bi,bj)=J
          IF (jG.EQ.OB_Jsouth(iG)) OB_Js(I,bi,bj)=J
         ENDDO
        ENDDO
C 2. E/W boundaries
        DO J=1,sNy
C   convert from local y index J to global y index jG
c   for E/W boundaries, we use faces stacked in y direction
         jG = exch2_tyYStackLo(tN)+J-1
C   loop over local x index I
         DO I=1,sNx
          iG = exch2_txYStackLo(tN)+I-1
          IF (iG.EQ.OB_Ieast(jG))  OB_Ie(J,bi,bj)=I
          IF (iG.EQ.OB_Iwest(jG))  OB_Iw(J,bi,bj)=I
         ENDDO
        ENDDO

#else /* ALLOW_EXCH2 */

        DO J=1-Oly,sNy+Oly
C convert from local y index J to global y index jG
         jG = myYGlobalLo-1+(bj-1)*sNy+J
C use periodicity to deal with out of range points caused by the overlaps.
C they will be excluded by the mask in any case, but this saves array
C out-of-bounds errors here.
         jGm = 1+mod( jG-1+Ny , Ny )
C loop over local x index I
         DO I=1,sNx
          iG = myXGlobalLo-1+(bi-1)*sNx+I
          iGm = 1+mod( iG-1+Nx , Nx )
C OB_Ieast(jGm) allows for the eastern boundary to be at variable x locations
          IF (iG.EQ.OB_Ieast(jGm))  OB_Ie(J,bi,bj)=I
          IF (iG.EQ.OB_Iwest(jGm))  OB_Iw(J,bi,bj)=I
         ENDDO
        ENDDO
        DO J=1,sNy
         jG = myYGlobalLo-1+(bj-1)*sNy+J
         jGm = 1+mod( jG-1+Ny , Ny )
         DO I=1-Olx,sNx+Olx
          iG = myXGlobalLo-1+(bi-1)*sNx+I
          iGm = 1+mod( iG-1+Nx , Nx )
C OB_Jnorth(iGm) allows for the northern boundary to be at variable y locations
          IF (jG.EQ.OB_Jnorth(iGm)) OB_Jn(I,bi,bj)=J
          IF (jG.EQ.OB_Jsouth(iGm)) OB_Js(I,bi,bj)=J
         ENDDO
        ENDDO
#endif /* ALLOW_EXCH2 */

C     bi,bj-loops
       ENDDO
      ENDDO

#ifdef ALLOW_EXCH2
C     exchange with neighbors
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         buf(sNx,J,bi,bj) = OB_Ie(J,bi,bj)
         buf(  1,J,bi,bj) = OB_Iw(J,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_3D_RS( buf, 1, myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         OB_Ie(J,bi,bj) = buf(sNx,J,bi,bj)
         OB_Iw(J,bi,bj) = buf(  1,J,bi,bj)
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO I=1,sNx
         buf(I,sNy,bi,bj) = OB_Jn(I,bi,bj)
         buf(I,  1,bi,bj) = OB_Js(I,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_3D_RS( buf, 1, myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO I=1-Olx,sNx+Olx
         OB_Jn(I,bi,bj) = buf(I,sNy,bi,bj)
         OB_Js(I,bi,bj) = buf(I,  1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_EXCH2 */

#endif /* ALLOW_OBCS */
      RETURN
      END
