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
     &                   smooth2DmaskName,
     &                   smooth3Dnbt,
     &                   smooth3DtypeH,
     &                   smooth3DsizeH,
     &                   smooth3DtypeZ,
     &                   smooth3DsizeZ,
     &                   smooth3D_Lx0,
     &                   smooth3D_Ly0,
     &                   smooth3D_Lz0,
     &                   smooth3Dfilter,
     &                   smooth3DmaskName

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

c--   Set default values.
      DO smoothOpNb=1,smoothOpNbMax
        smooth2Dnbt(smoothOpNb)=0
        smooth2D_Lx0(smoothOpNb)=0. _d 0
        smooth2D_Ly0(smoothOpNb)=0. _d 0
        smooth2Dtype(smoothOpNb)=0
        smooth2Dsize(smoothOpNb)=0
        smooth2Dfilter(smoothOpNb)=0
        smooth2DmaskName(smoothOpNb)='maskC'

        smooth3Dnbt(smoothOpNb)=0
        smooth3D_Lx0(smoothOpNb)=0. _d 0
        smooth3D_Ly0(smoothOpNb)=0. _d 0
        smooth3D_Lz0(smoothOpNb)=0. _d 0
        smooth3DtypeH(smoothOpNb)=0
        smooth3DsizeH(smoothOpNb)=0
        smooth3DtypeZ(smoothOpNb)=0
        smooth3DsizeZ(smoothOpNb)=0
        smooth3Dfilter(smoothOpNb)=0
        smooth3DmaskName(smoothOpNb)='maskC'
      ENDDO

C--   Read settings from model parameter file "data.smooth".
      WRITE(msgBuf,'(A)') 'SMOOTH_READPARMS: opening data.smooth'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.smooth', 'SMOOTH_READPARMS',
     O                          iUnit,
     I                          myThid )

      READ(  iUnit, nml = smooth_nml )

      WRITE(msgBuf,'(2A)') 'SMOOTH_READPARMS: ',
     &       'finished reading data.smooth'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C--   Print pkg/smooth settings to standard output:
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// pkg/smooth configuration'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      DO smoothOpNb=1,smoothOpNbMax
        IF (smooth2Dtype(smoothOpNb).NE.0) THEN
          WRITE(msgBuf,'(A,I2,I6,2f6.0,A)') 'smooth 2D parameters: ',
     &       smoothOpNb,smooth2Dnbt(smoothOpNb),
     &       smooth2D_Lx0(smoothOpNb),smooth2D_Ly0(smoothOpNb),
     &       smooth2DmaskName(smoothOpNb)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , 1)
        ENDIF
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
        IF ((smooth3DtypeZ(smoothOpNb).NE.0).OR.
     &      (smooth3DtypeH(smoothOpNb).NE.0)) then
          WRITE(msgBuf,'(A,I2,I6,3f6.0,A)') 'smooth 3D parameters: ',
     &       smoothOpNb,smooth3Dnbt(smoothOpNb),
     &       smooth3D_Lx0(smoothOpNb),smooth3D_Ly0(smoothOpNb),
     &       smooth3D_Lz0(smoothOpNb),
     &       smooth3DmaskName(smoothOpNb)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , 1)
        ENDIF
      ENDDO

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// End of pkg/smooth config. summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      _END_MASTER( myThid )

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
