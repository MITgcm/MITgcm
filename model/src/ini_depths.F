C $Header: /u/gcmpack/MITgcm/model/src/ini_depths.F,v 1.50 2011/07/19 22:33:10 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_DEPTHS
C     !INTERFACE:
      SUBROUTINE INI_DEPTHS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_DEPTHS
C     | o define R_position of Lower and Surface Boundaries
C     *==========================================================*
C     |atmosphere orography:
C     | define either in term of P_topo or converted from Z_topo
C     |ocean bathymetry:
C     | The depths of the bottom of the model is specified in
C     | terms of an XY map with one depth for each column of
C     | grid cells. Depths do not have to coincide with the
C     | model levels. The model lopping algorithm makes it
C     | possible to represent arbitrary depths.
C     | The mode depths map also influences the models topology
C     | By default the model domain wraps around in X and Y.
C     | This default doubly periodic topology is "supressed"
C     | if a depth map is defined which closes off all wrap
C     | around flow.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iG, jG  :: Global coordinate index
C     bi, bj  :: Tile indices
C     i, j    :: Loop counters
C     oldPrec :: Temporary used in controlling binary input dataset precision
C     msgBuf  :: Informational/error message buffer
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  i, j
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (usingPCoords .AND. bathyFile .NE. ' '
     &                 .AND. topoFile  .NE. ' ' ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_DEPTHS: both bathyFile & topoFile are specified:',
     &  ' select the right one !'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_DEPTHS'
      ENDIF

C------
C   0) Initialize R_low and Ro_surf (define an empty domain)
C------
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          R_low(i,j,bi,bj)   = 0. _d 0
          Ro_surf(i,j,bi,bj) = 0. _d 0
          topoZ(i,j,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Need to synchronize here before doing master-thread IO
C     this is done within IO routines => no longer needed
c     _BARRIER

C------
C   1) Set R_low = the Lower (in r sense) boundary of the fluid column :
C------
      IF (usingPCoords .OR. bathyFile .EQ. ' ') THEN
C- e.g., atmosphere : R_low = Top of atmosphere
C-            ocean : R_low = Bottom
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           R_low(i,j,bi,bj) = rF(Nr+1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE

#ifdef ALLOW_MNC
        IF (useMNC .AND. mnc_read_bathy) THEN
          CALL MNC_CW_ADD_VNAME('bathy', 'Cen_xy_Hn__-__-', 3,4, myThid)
          CALL MNC_FILE_CLOSE_ALL_MATCHING(bathyFile, myThid)
          CALL MNC_CW_SET_UDIM(bathyFile, 1, myThid)
          CALL MNC_CW_SET_CITER(bathyFile, 2, -1, -1, -1, myThid)
          CALL MNC_CW_SET_UDIM(bathyFile, 1, myThid)
          CALL MNC_CW_RS_R('D',bathyFile,0,0,'bathy',R_low, myThid)
          CALL MNC_FILE_CLOSE_ALL_MATCHING(bathyFile, myThid)
          CALL MNC_CW_DEL_VNAME('bathy', myThid)
        ELSE
#endif /*  ALLOW_MNC  */
C Read the bathymetry using the mid-level I/O package read_write_rec
C The 0 is the "iteration" argument. The 1 is the record number.
          CALL READ_REC_XY_RS( bathyFile, R_low, 1, 0, myThid )
C Read the bathymetry using the mid-level I/O package read_write_fld
C The 0 is the "iteration" argument. The ' ' is an empty suffix
c       CALL READ_FLD_XY_RS( bathyFile, ' ', R_low, 0, myThid )
C Read the bathymetry using the low-level I/O package
c       CALL MDSREADFIELD( bathyFile, readBinaryPrec,
c    &                     'RS', 1, R_low, 1, myThid )

#ifdef ALLOW_MNC
        ENDIF
#endif /*  ALLOW_MNC  */


      ENDIF
C- end setup R_low in the interior

C- fill in the overlap (+ BARRIER):
      _EXCH_XY_RS(R_low, myThid )

      IF ( debugLevel.GE.debLevC ) THEN
c      PRINT *, ' Calling plot field', myThid
       CALL PLOT_FIELD_XYRS( R_low, 'Bottom depths (ini_depths)',
     &                       -1, myThid )
      ENDIF
c     CALL WRITE_FLD_XY_RS( 'R_low' ,' ', R_low, 0,myThid)

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C------
C   2) Set R_surf = Surface boundary: ocean surface / ground for the atmosphere
C------

      IF ( usingPCoords .AND. bathyFile.NE.' ' ) THEN
C------ read directly Po_surf from bathyFile (only for backward compatibility)

        CALL READ_REC_XY_RS( bathyFile, Ro_surf, 1, 0, myThid )

      ELSEIF ( topoFile.EQ.' ' ) THEN
C------ set default value:

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            Ro_surf(i,j,bi,bj) = rF(1)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSE
C------ read from file:

C- read surface topography (in m) from topoFile (case topoFile.NE.' '):

        CALL READ_REC_XY_RS( topoFile, topoZ, 1, 0, myThid )
        _EXCH_XY_RS( topoZ, myThid )

        IF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
C----
C   Convert Surface Geopotential to (reference) Surface Pressure
C   according to Tref profile, using same discretisation as in calc_phi_hyd
C----
c         CALL WRITE_FLD_XY_RS( 'topo_Z',' ',topoZ,0,myThid)

          CALL INI_P_GROUND( 2, topoZ,
     O                       Ro_surf,
     I                       myThid )

C         This I/O is now done in write_grid.F
c         CALL WRITE_FLD_XY_RS( 'topo_P',' ',Ro_surf,0,myThid)

        ELSEIF ( buoyancyRelation.EQ.'OCEANICP' ) THEN

          WRITE(msgBuf,'(A,A)') 'S/R INI_DEPTHS: ',
     &     'from topoFile (in m) to ref.bottom pressure: Not yet coded'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R INI_DEPTHS'

        ELSE
C----
C   Direct Transfer to Ro_surf (e.g., to specify upper ocean boundary
C    below an ice-shelf - NOTE - actually not yet implemented )
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
              Ro_surf(i,j,bi,bj) = topoZ(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO

        ENDIF

C------ end case "read topoFile"
      ENDIF

C----- fill in the overlap (+ BARRIER):
      _EXCH_XY_RS(Ro_surf, myThid )

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C------
C   3) Close the Domain (special configuration).
C------
      IF (usingPCoords) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iG = myXGlobalLo-1+(bi-1)*sNx+i
           jG = myYGlobalLo-1+(bj-1)*sNy+j
C          Test for eastern edge
c          IF ( iG .EQ. Nx )  Ro_surf(i,j,bi,bj) = 0.
C          Test for northern edge
c          IF ( jG .EQ. Ny )  Ro_surf(i,j,bi,bj) = 0.
C- Domain : Symetric % Eq. & closed at N & S boundaries:
c          IF ( usingSphericalPolarGrid .AND.
c    &          ABS(yC(i,j,bi,bj)).GE.ABS(ygOrigin) )
c    &       Ro_surf(i,j,bi,bj) = rF(Nr+1)
           IF ( usingSphericalPolarGrid .AND.
     &          ABS(yC(i,j,bi,bj)).GE.90. )
     &       Ro_surf(i,j,bi,bj) = rF(Nr+1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iG = myXGlobalLo-1+(bi-1)*sNx+i
           jG = myYGlobalLo-1+(bj-1)*sNy+j
C          Test for eastern edge
c          IF ( iG .EQ. Nx )  R_low(i,j,bi,bj) = 0.
C          Test for northern edge
c          IF ( jG .EQ. Ny )  R_low(i,j,bi,bj) = 0.
C- Domain : Symetric % Eq. & closed at N & S boundaries:
c          IF ( usingSphericalPolarGrid .AND.
c    &          ABS(yC(i,j,bi,bj)).GE.ABS(ygOrigin) )
c    &       R_low(i,j,bi,bj) = rF(1)
           IF ( usingSphericalPolarGrid .AND.
     &          ABS(yC(i,j,bi,bj)).GE.90. )
     &       R_low(i,j,bi,bj) = rF(1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      IF ( debugLevel.GE.debLevC ) THEN
       _BARRIER
       CALL PLOT_FIELD_XYRS( Ro_surf,
     &           'Surface reference r-position (ini_depths)',
     &           -1, myThid )
      ENDIF
c     CALL WRITE_FLD_XY_RS('Ro_surf',' ',Ro_surf,0,myThid)

C--   Everyone else must wait for the depth to be loaded
C-    note: not necessary since all single-thread IO above are followed
C           by an EXCH (with BARRIER) + BARRIER within IO routine
c     _BARRIER

#ifdef ALLOW_OBCS
      IF ( useOBCS ) THEN
C     check for inconsistent topography along boundaries and fix it
       CALL OBCS_CHECK_DEPTHS( myThid )
C     update the overlaps
       _EXCH_XY_RS( R_low, myThid )
      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_EXCH2
C     Check domain boundary (e.g., in case of missing tiles)
      CALL EXCH2_CHECK_DEPTHS( R_low, Ro_surf, myThid )
#endif

      RETURN
      END
