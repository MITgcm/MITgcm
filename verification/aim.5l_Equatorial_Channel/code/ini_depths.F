C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/ini_depths.F,v 1.6 2001/09/27 23:42:16 jmc Exp $
C $Name:  $

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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables in common ==
C     Hloc  - Temporary array used to read surface topography
C             has to be in common for multi threading    
      COMMON / LOCAL_INI_DEPTHS / Hloc
      _RS Hloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy) 
C     == Local variables ==
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     I,J,K
C     oldPrec - Temporary used in controlling binary input dataset precision
C     msgBuf    - Informational/error meesage buffer 
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I, J
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (groundAtK1 .AND. bathyFile .NE. ' '
     &               .AND. topoFile  .NE. ' ' ) THEN
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
          R_low(i,j,bi,bj) = 0.
          Ro_surf(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C------
C   1) Set R_low = the Lower (in r sense) boundary of the fluid column :
C------
      IF (groundAtK1 .OR. bathyFile .EQ. ' ') THEN
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
        _BEGIN_MASTER( myThid )
C Read the bathymetry using the mid-level I/O pacakage read_write_rec
C The 0 is the "iteration" argument. The 1 is the record number.
        CALL READ_REC_XY_RS( bathyFile, R_low, 1, 0, myThid )
C Read the bathymetry using the mid-level I/O pacakage read_write_fld
C The 0 is the "iteration" argument. The ' ' is an empty suffix
c       CALL READ_FLD_XY_RS( bathyFile, ' ', R_low, 0, myThid )
C Read the bathymetry using the low-level I/O package
c       CALL MDSREADFIELD( bathyFile, readBinaryPrec,
c    &                     'RS', 1, R_low, 1, myThid )
        _END_MASTER(myThid)

      ENDIF
C- end setup R_low in the interior

C- fill in the overlap :
      _EXCH_XY_R4(R_low, myThid )

c     CALL PLOT_FIELD_XYRS(R_low,'Bottom depths (ini_depths)',1,myThid)
c     _BEGIN_MASTER( myThid )
c     CALL WRITE_FLD_XY_RS( 'R_low' ,' ', R_low, 0,myThid) 
c     _END_MASTER(myThid)

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C------
C   2) Set R_surf = Surface boundary: ocean surface / ground for the atmosphere
C------

      IF ( groundAtK1 .AND. bathyFile.NE.' ' ) THEN
C------ read directly Po_surf from bathyFile (only for backward compatibility) 

        _BEGIN_MASTER( myThid )
        CALL READ_REC_XY_RS( bathyFile, Ro_surf, 1, 0, myThid )
        _END_MASTER(myThid)
        _BARRIER

      ELSEIF ( topoFile.EQ.' ' ) THEN
C------ set default value:

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            Ro_surf(i,j,bi,bj) = Ro_SeaLevel
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSE
C------ read from file:

C- read surface topography (in m) from topoFile (case topoFile.NE.' '):
        _BEGIN_MASTER( myThid )
        CALL READ_REC_XY_RS( topoFile, Hloc, 1, 0, myThid )
        _END_MASTER(myThid)
        _BARRIER

        IF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
C----
C   Convert Surface Geopotential to (reference) Surface Pressure 
C   according to Tref profile, using same discretisation as in calc_phi_hyd
C----
c         _BEGIN_MASTER( myThid )
c         CALL WRITE_FLD_XY_RS( 'topo_Z',' ',Hloc,0,myThid) 
c         _END_MASTER(myThid)

          CALL INI_P_GROUND( Hloc, Ro_surf, myThid )

          _BARRIER
          _BEGIN_MASTER( myThid )
          CALL WRITE_FLD_XY_RS( 'topo_P',' ',Ro_surf,0,myThid) 
          _END_MASTER(myThid)

        ELSE
C----
C   Direct Transfer to Ro_surf :
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
              Ro_surf(i,j,bi,bj) = Hloc(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO

        ENDIF

C------ end case "read topoFile"
      ENDIF

C----- fill in the overlap : 
      _EXCH_XY_R4(Ro_surf, myThid )

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C------
C   3) Close the Domain (special configuration).
C------
      IF (groundAtK1) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
C          Test for eastern edge
c          IF ( iG .EQ. Nx )  Ro_surf(i,j,bi,bj) = 0.
C          Test for northern edge
c          IF ( jG .EQ. Ny )  Ro_surf(i,j,bi,bj) = 0.
c- Domain : Symetric % Eq. & closed at N & S boundaries:
           IF (usingSphericalPolarGrid .AND.
     &         abs(yC(I,J,bi,bj)).GE.-phiMin)
     &       Ro_surf(I,J,bi,bj) = rF(Nr+1)
           IF (usingSphericalPolarGrid .AND. abs(yC(I,J,bi,bj)).GE.90. ) 
     &       Ro_surf(I,J,bi,bj) = rF(Nr+1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
C          Test for eastern edge
c          IF ( iG .EQ. Nx )  R_low(i,j,bi,bj) = 0.
C          Test for northern edge
c          IF ( jG .EQ. Ny )  R_low(i,j,bi,bj) = 0.
c- Domain : Symetric % Eq. & closed at N & S boundaries:
           IF (usingSphericalPolarGrid .AND.
     &         abs(yC(I,J,bi,bj)).GE.-phiMin)
     &       R_low(I,J,bi,bj) = Ro_SeaLevel
           IF (usingSphericalPolarGrid .AND. abs(yC(I,J,bi,bj)).GE.90. ) 
     &       R_low(I,J,bi,bj) = Ro_SeaLevel
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

c     _BEGIN_MASTER( myThid )
c     CALL WRITE_FLD_XY_RS('Ro_surf',' ',Ro_surf,0,myThid) 
c     _END_MASTER(myThid)

      RETURN
      END
