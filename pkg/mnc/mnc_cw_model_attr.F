C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_model_attr.F,v 1.16 2011/05/23 01:08:22 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_SET_GATTR

C     !INTERFACE:
      SUBROUTINE MNC_CW_SET_GATTR(
     I     fname,
     I     bi, bj, tnum,
     I     myThid )

C     !DESCRIPTION:
C     This subroutine uses the MNC convenience wrapper layer to write
C     per-tile grid (grid.xxxxx.nc) files containing all of the grid
C     information including locations, spacing, areas, \textit{etc}.

C     !USES:
      implicit none
#include "BUILD_INFO.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MNC_PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
C     Functions
      integer ILNBLNK

C     !INPUT PARAMETERS:
      integer myThid, bi,bj, tnum
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer ilnb

      CALL MNC_FILE_REDEF(fname, myThid)

C     Global attributes
      ilnb = ILNBLNK(the_run_name)
      IF (ilnb .GT. 0) THEN
        CALL MNC_FILE_ADD_ATTR_STR(fname, 'the_run_name',
     &       the_run_name, myThid )
      ENDIF

#ifdef THISVER
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'MITgcm_version',
     &     THISVER ,
     &     myThid )
#endif
#ifdef THISUSER
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'build_user',
     &     THISUSER ,
     &     myThid )
#endif
#ifdef THISHOST
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'build_host',
     &     THISHOST ,
     &     myThid )
#endif
#ifdef THISDATE
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'build_date',
     &     THISDATE ,
     &     myThid )
#endif

      CALL MNC_FILE_ADD_ATTR_STR(fname, 'MITgcm_URL',
     &     'http://mitgcm.org', myThid )
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'MITgcm_tag_id',
     &     MNC_TAG_ID, myThid )
      CALL MNC_FILE_ADD_ATTR_DBL(fname, 'MITgcm_mnc_ver',
     &     1, 0.9D0, myThid )

C     Grid info included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(fname,'tile_number', 1, tnum, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,'bi', 1, bi, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,'bj', 1, bj, myThid)
C jmc: xC0 & yC0 are not initialised + their type does not match (RS type, not DBL)
c     CALL MNC_FILE_ADD_ATTR_DBL(fname,'xC0', 1, xC0, myThid)
c     CALL MNC_FILE_ADD_ATTR_DBL(fname,'yC0', 1, yC0, myThid)
c     CALL MNC_FILE_ADD_ATTR_DBL(fname,'gravitySign',
c    &     1, gravitySign, myThid )
c     CALL MNC_FILE_ADD_ATTR_DBL(fname,'rkFac', 1, rkFac, myThid)

#ifdef ALLOW_EXCH2
C     W2/exch2 information
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_nNeighbours', 1, exch2_nNeighbours(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_neighbourId', exch2_nNeighbours(tnum),
     &     exch2_neighbourId(1,tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_pij',
     &     4*exch2_nNeighbours(tnum), exch2_pij(1,1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oi',
     &     exch2_nNeighbours(tnum), exch2_oi(1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oj',
     &     exch2_nNeighbours(tnum), exch2_oj(1,tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_isNedge', 1, exch2_isNedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_isSedge', 1, exch2_isSedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_isEedge', 1, exch2_isEedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_isWedge', 1, exch2_isWedge(tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_myFace', 1, exch2_myFace(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_txGlobalo', 1, exch2_txGlobalo(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_tyGlobalo', 1, exch2_tyGlobalo(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_tBasex', 1, exch2_tBasex(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,
     &     'exch2_tBasey', 1, exch2_tBasey(tnum), myThid)
#endif

C     Model parameters (SIZE.h) included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'sNx', 1, sNx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'sNy', 1, sNy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'OLx', 1, OLx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'OLy', 1, OLy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nSx', 1, nSx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nSy', 1, nSy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nPx', 1, nPx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nPy', 1, nPy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Nx', 1, Nx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Ny', 1, Ny, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Nr', 1, Nr, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

