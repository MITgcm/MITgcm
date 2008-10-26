C $Header: /u/gcmpack/MITgcm/model/src/write_grid.F,v 1.25 2008/10/26 11:09:46 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: WRITE_GRID
C     !INTERFACE:
      SUBROUTINE WRITE_GRID(
     I                 myThid )

C     !DESCRIPTION:
C     Writes the model geometry and grid arrays to file(s) using which
C     ever I/O package is available/active.  This includes horizontal
C     and vertical grid and finite volume modulating factors (hFacs).
C     Essentailly, it dumps the contents of GRID.h to disk.

C     !CALLING SEQUENCE:
C     INITIALIZE_FIXED
C       |
C       |-- WRITE_GRID

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid

C     !LOCAL VARIABLES:
C     tmpfld :: Temporary array used to compute & write Total Depth ;
C               loaded in multi-threaded way but written by single (master)
C               thread => needs to be in common to be shared by all threads
      COMMON / LOCAL_WRITE_GRID / tmpfld
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj
      LOGICAL writegrid_mdsio
#ifdef ALLOW_MDSIO
      INTEGER k
      _RS tmpVar(Nr+1)
#endif
#ifdef ALLOW_MNC
      CHARACTER*(4) bfname
#endif
CEOP

C     Calculate the fluid thickness in R coordinates as seen by the
C     model.  This may differ from what the user specified due to
C     partial cells, etc.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Total fluid column thickness (r_unit) :
            tmpfld(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO
      _BARRIER

      writegrid_mdsio = .TRUE.
#ifdef ALLOW_MNC
      IF ( useMNC ) THEN
        writegrid_mdsio = outputTypesInclusive .OR. .NOT.writegrid_mnc
      ENDIF
#endif /* ALLOW_MNC */

      IF ( writegrid_mdsio ) THEN

C     Write horizontal grid arrays
        CALL WRITE_FLD_XY_RS( 'XC',' ',xC,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'YC',' ',yC,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'XG',' ',xG,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'YG',' ',yG,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'RAC',' ',rA,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'RAW',' ',rAw,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'RAS',' ',rAs,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'RAZ',' ',rAz,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'DXG',' ',dxG,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'DYG',' ',dyG,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'DXC',' ',dxC,-1,myThid)
        CALL WRITE_FLD_XY_RS( 'DYC',' ',dyC,-1,myThid)
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
          CALL WRITE_FLD_XY_RS( 'AngleCS',' ',angleCosC,-1,myThid)
          CALL WRITE_FLD_XY_RS( 'AngleSN',' ',angleSinC,-1,myThid)
        ENDIF
C     Write 3D geometry arrays
        CALL WRITE_FLD_XY_RS( 'Depth',' ',tmpfld,-1,myThid)
        CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC, 0,myThid)
        CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW, 0,myThid)
        CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS, 0,myThid)
        IF (buoyancyRelation .EQ. 'ATMOSPHERIC')
     &     CALL WRITE_FLD_XY_RS( 'topo_P',' ',Ro_surf,-1,myThid)

#ifdef ALLOW_MDSIO
C     Write vertical grid arrays
        CALL MDSWRITEVEC_LOC_RS('RC', writeBinaryPrec, Nr, rC,
     &                                0,0, 1, -1, myThid)
        CALL MDSWRITEVEC_LOC_RS('RF', writeBinaryPrec,1+Nr, rF,
     &                                0,0, 1, -1, myThid)
        CALL MDSWRITEVEC_LOC_RS('DRC',writeBinaryPrec, Nr, drC,
     &                                0,0, 1, -1, myThid)
        CALL MDSWRITEVEC_LOC_RS('DRF',writeBinaryPrec, Nr, drF,
     &                                0,0, 1, -1, myThid)
C--  comment out the "if": also usefull for the Ocean (to get the full Presure)
c       IF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
        DO k=1,Nr+1
          tmpVar(k) = phiRef(2*k-1)
        ENDDO
        CALL MDSWRITEVEC_LOC_RS('PHrefF', writeBinaryPrec, 1+Nr,
     &                            tmpVar, 0, 0, 1, -1, myThid)
        DO k=1,Nr
          tmpVar(k) = phiRef(2*k)
        ENDDO
        CALL MDSWRITEVEC_LOC_RS('PHrefC', writeBinaryPrec, Nr,
     &                            tmpVar, 0, 0, 1, -1, myThid)
c       ENDIF
#endif /* ALLOW_MDSIO */

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF ( useMNC .AND. writegrid_mnc ) THEN

        _BEGIN_MASTER( myThid )
        bfname='grid'

C       Write the GRID.h variables to a file
        CALL MNC_CW_SET_UDIM(bfname, 0, myThid)
        CALL MNC_CW_SET_CITER(bfname, 2, -1, -1, -1, myThid)
        CALL MNC_CW_SET_UDIM(bfname, 1, myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'RC',rC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'RF',rF,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'RU',rF(2),myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'RL',rF,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'drC',drC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'drF',drF,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'XC',xC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'YC',yC,myThid)
c       CALL MNC_CW_RS_W('D',bfname,0,0,'XU',xG,myThid)
c       CALL MNC_CW_RS_W('D',bfname,0,0,'YU',yC,myThid)
c       CALL MNC_CW_RS_W('D',bfname,0,0,'XV',xC,myThid)
c       CALL MNC_CW_RS_W('D',bfname,0,0,'YV',yG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'XG',xG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'YG',yG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dxC',dxC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dyC',dyC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dxF',dxF,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dyF',dyF,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dxG',dxG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dyG',dyG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dxV',dxV,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'dyU',dyU,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'rA',rA,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'rAw',rAw,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'rAs',rAs,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'rAz',rAz,myThid)
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
          CALL MNC_CW_RS_W('D',bfname,0,0,'AngleCS',angleCosC,myThid)
          CALL MNC_CW_RS_W('D',bfname,0,0,'AngleSN',angleSinC,myThid)
        ENDIF
        CALL MNC_CW_RS_W('D',bfname,0,0,'fCori',fCori,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'fCoriG',fCoriG,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'R_low',R_low,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'Ro_surf',Ro_surf,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'Depth',tmpfld,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'HFacC',HFacC,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'HFacW',HFacW,myThid)
        CALL MNC_CW_RS_W('D',bfname,0,0,'HFacS',HFacS,myThid)

        _END_MASTER(myThid)

      ENDIF
#endif /* ALLOW_MNC */

      RETURN
      END
