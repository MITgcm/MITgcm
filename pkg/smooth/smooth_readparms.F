C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_readparms.F,v 1.5 2014/05/28 00:13:16 jmc Exp $
C $Name:  $

#include "SMOOTH_OPTIONS.h"

      subroutine smooth_readparms( myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_readparms
C     | o Routine that reads the pkg/smooth namelist from data.smooth
C     *==========================================================*

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

#include "SMOOTH.h"

c     == routine arguments ==

      integer myThid

c     == local variables ==
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, num_file, num_var
      integer smoothOpNb

c     == end of interface ==

c--   Read the namelist input.
      namelist /smooth_nml/
     &                   smooth2Dnbt,
     &                   smooth2Dtype,
     &                   smooth2Dsize,
     &                   smooth2D_Lx0,
     &                   smooth2D_Ly0,
     &                   smooth2Dfilter,
     &                   smooth3Dnbt,
     &                   smooth3DtypeH,
     &                   smooth3DsizeH,
     &                   smooth3DtypeZ,
     &                   smooth3DsizeZ,
     &                   smooth3D_Lx0,
     &                   smooth3D_Ly0,
     &                   smooth3D_Lz0,
     &                   smooth3Dfilter

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useSMOOTH ) THEN
C-    pkg SMOOTH is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.smooth is found
         CALL PACKAGES_UNUSED_MSG( 'useSMOOTH', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

c--     Set default values.
      DO smoothOpNb=1,smoothOpNbMax

      smooth2Dnbt(smoothOpNb)=0
      smooth2D_Lx0(smoothOpNb)=0. _d 0
      smooth2D_Ly0(smoothOpNb)=0. _d 0
      smooth2Dtype(smoothOpNb)=0
      smooth2Dsize(smoothOpNb)=0
      smooth2Dfilter(smoothOpNb)=0

      smooth3Dnbt(smoothOpNb)=0
      smooth3D_Lx0(smoothOpNb)=0. _d 0
      smooth3D_Ly0(smoothOpNb)=0. _d 0
      smooth3D_Lz0(smoothOpNb)=0. _d 0
      smooth3DtypeH(smoothOpNb)=0
      smooth3DsizeH(smoothOpNb)=0
      smooth3DtypeZ(smoothOpNb)=0
      smooth3DsizeZ(smoothOpNb)=0
      smooth3Dfilter(smoothOpNb)=0

      ENDDO

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'PROFILES_READPARMS: opening data.smooth'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.smooth', 'SMOOTH_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(  iUnit, nml = smooth_nml )

        WRITE(msgBuf,'(2A)') 'PROFILES_READPARMS: ',
     &       'finished reading data.smooth'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
