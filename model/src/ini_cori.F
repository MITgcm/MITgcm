C $Header: /u/gcmpack/MITgcm/model/src/ini_cori.F,v 1.33 2010/11/12 03:16:16 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CORI

C     !INTERFACE:
      SUBROUTINE INI_CORI( myThid )
C     !DESCRIPTION:
C     Initialise coriolis term.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid
CEOP

C     === Functions ====
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     !LOCAL VARIABLES:
C     bi,bj   :: Tile Indices counters
C     i, j    :: Loop counters
C     facGrid :: Factor for grid to meter conversion
      INTEGER bi,bj
      INTEGER i, j
      _RL facGrid
#ifndef OLD_GRID_IO
      INTEGER myTile, iG, iLen
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif


C     Initialise coriolis parameter
      IF     ( selectCoriMap.EQ.0 ) THEN
C       Constant F case
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = f0
                fCoriG(i,j,bi,bj) = f0
                fCoriCos(i,j,bi,bj)=fPrime
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( selectCoriMap.EQ.1 ) THEN
C       Beta plane case
        facGrid = 1. _d 0
        IF ( usingSphericalPolarGrid
     &     .OR. usingCurvilinearGrid ) facGrid = deg2rad*rSphere
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = f0+beta*_yC(i,j,bi,bj)*facGrid
                fCoriG(i,j,bi,bj) = f0+beta* yG(i,j,bi,bj)*facGrid
                fCoriCos(i,j,bi,bj)=fPrime
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ELSEIF ( selectCoriMap.EQ.2 ) THEN
C       Spherical case
C       Note in this case we assume yC is in degrees.
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  =
     &                 2. _d 0*omega*sin(_yC(i,j,bi,bj)*deg2rad)
                fCoriG(i,j,bi,bj) =
     &                 2. _d 0*omega*sin(yG(i,j,bi,bj)*deg2rad)
                fCoriCos(i,j,bi,bj)=
     &                 2. _d 0*omega*cos(_yC(i,j,bi,bj)*deg2rad)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
c       CALL WRITE_FLD_XY_RL('fCoriC',' ',fCori ,  0,myThid)
c       CALL WRITE_FLD_XY_RL('fCoriG',' ',fCoriG , 0,myThid)
c       CALL WRITE_FLD_XY_RL('fCorCs',' ',fCoriCos,0,myThid)
      ELSE
C       Initialise to zero
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-Oly,sNy+Oly
              DO i=1-Olx,sNx+Olx
                fCori(i,j,bi,bj)  = 0. _d 0
                fCoriG(i,j,bi,bj) = 0. _d 0
                fCoriCos(i,j,bi,bj)=0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF ( selectCoriMap.EQ.3 ) THEN
C     Special custom form: read from files
        CALL READ_REC_XY_RS( 'fCoriC.bin', fCori,   1, 0, myThid )
        CALL READ_REC_XY_RS( 'fCorCs.bin', fCoriCos,1, 0, myThid )
        IF ( .NOT.useCubedSphereExchange ) THEN
         CALL READ_REC_XY_RS('fCoriG.bin', fCoriG,  1, 0, myThid )
        ELSE
#ifdef OLD_GRID_IO
         CALL READ_REC_XY_RS('fCoriG.bin', fCoriG,  1, 0, myThid )
C-       deal with the 2 missing corners (for fCoriG):
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
C-  Notes: this will only works with 6 tiles (1 per face) and
C    with 2 polar faces + 4 equatorials:
           IF (bi.LE.3 .OR. bi.GE.5) THEN
             fCoriG(sNx+1,1,bi,bj) = fCoriG(1,1,bi,bj)
           ELSE
             fCoriG(sNx+1,1,bi,bj) = -fCoriG(1,1,bi,bj)
           ENDIF
           IF (bi.GE.3) THEN
             fCoriG(1,sNy+1,bi,bj) = fCoriG(1,1,bi,bj)
             fCoriG(sNx+1,sNy+1,bi,bj) = fCoriG(sNx+1,1,bi,bj)
           ELSE
             fCoriG(1,sNy+1,bi,bj) = -fCoriG(1,1,bi,bj)
             fCoriG(sNx+1,sNy+1,bi,bj) = -fCoriG(sNx+1,1,bi,bj)
           ENDIF
          ENDDO
         ENDDO
#else  /* OLD_GRID_IO */
         _BEGIN_MASTER(myThid)
         DO bj = 1,nSy
          DO bi = 1,nSx
           iG = bi+(myXGlobalLo-1)/sNx
           myTile = iG
#ifdef ALLOW_EXCH2
           myTile = W2_myTileList(bi,bj)
           iG = exch2_myface(myTile)
#endif
           WRITE(fName,'(2A,I3.3,A)') 'fCoriG','.face',iG,'.bin'
           iLen = ILNBLNK(fName)
           WRITE(msgBuf,'(A,I6,2A)')
     &       ' Reading tile:', myTile, ' from file ', fName(1:iLen)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT , myThid )
#ifdef ALLOW_MDSIO
           CALL MDS_FACEF_READ_RS( fName, readBinaryPrec, 1,
     &                             fCoriG, bi, bj, myThid )
#else /* ALLOW_MDSIO */
           WRITE(msgBuf,'(A)') 'INI_CORI: Needs to compile MDSIO pkg'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R INI_CORI'
#endif /* ALLOW_MDSIO */

          ENDDO
         ENDDO
         _END_MASTER(myThid)
#endif /* OLD_GRID_IO */
        ENDIF

        CALL EXCH_XY_RS( fCori, myThid )
        CALL EXCH_XY_RS( fCoriCos, myThid )
        CALL EXCH_Z_3D_RS( fCoriG, 1, myThid )
      ENDIF

#ifdef ALLOW_MONITOR
      IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

        IF (monitor_stdio) THEN
          mon_write_stdout = .TRUE.
        ELSE
          mon_write_stdout = .FALSE.
        ENDIF
        mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
        IF (useMNC .AND. monitor_mnc) THEN
          DO i = 1,MAX_LEN_MBUF
            mon_fname(i:i) = ' '
          ENDDO
          mon_fname(1:12) = 'monitor_grid'
          CALL MNC_CW_SET_UDIM(mon_fname, 1, myThid)
          mon_write_mnc = .TRUE.
        ENDIF
#endif /*  ALLOW_MNC  */

      ENDIF

      CALL MON_SET_PREF( mon_string_none, myThid )
      CALL MON_PRINTSTATS_RS(1,fCori,'fCori',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriG,'fCoriG',myThid)
      CALL MON_PRINTSTATS_RS(1,fCoriCos,'fCoriCos',myThid)

      IF ( MASTER_CPU_IO(myThid) ) THEN
        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.
      ENDIF
#endif /* ALLOW_MONITOR */

      RETURN
      END
