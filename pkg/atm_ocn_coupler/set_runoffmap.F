C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/set_runoffmap.F,v 1.4 2013/12/02 22:03:08 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: SET_RUNOFFMAP

C !INTERFACE:
      SUBROUTINE SET_RUNOFFMAP( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SET_RUNOFFMAP
C     | o define runoff mapping from atmos. grid (land) to
C     |   ocean grid
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "CPL_PARAMS.h"
#include "CPL_MAP2GRIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit    :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
      INTEGER n, ijo, ija
      INTEGER lengthName, lengthRec, iRec
      Real*8  r8seg(3)
      Real*8  tmpfld(3,ROsize), rAc(Nx_ocn*Ny_ocn)
CEOP

      WRITE(msgUnit,'(2A)') 'SET_RUNOFFMAP: ','entering'

C-    Initialize to zero :
        DO n=1,ROsize
          ijROocn(n)=0
          ijROatm(n)=0
          arROmap(n)=0.
        ENDDO

        nROmap = runOffMapSize
c       lengthName=ILNBLNK( runOffMapFile ) ! eesup/src/utils.F not compiled here
        lengthName=0
        DO n=1,LEN( runOffMapFile )
         IF ( runOffMapFile(n:n).NE.' ' ) lengthName=n
        ENDDO
        WRITE(msgUnit,'(3A,I6)')
     &     ' runOffMapFile =>>', runOffMapFile(1:lengthName),
     &   '<<= , runOffMapSize=', runOffMapSize
        IF ( lengthName.EQ.0 ) nROmap=0
        IF ( nROmap.EQ.0 ) THEN
          WRITE(msgUnit,'(2A,I9,A)') 'SET_RUNOFFMAP: ',
     &                'nothing to set (nROmap=', nROmap, ' )'
          RETURN
        ENDIF
        IF ( nROmap.GT.ROsize ) THEN
          WRITE(msgUnit,'(2A)') '*** ERROR *** SET_RUNOFFMAP: ',
     &                          'runOffMapSize exceeds ROsize'
          STOP 'ABNORMAL END: S/R SET_RUNOFFMAP'
        ENDIF

C-    Read area catchment from file ;
      WRITE(msgUnit,'(2A)') 'SET_RUNOFFMAP: ','reading runOffMapFile'
c       lengthRec=3*nROmap*WORDLENGTH*2
c       OPEN(88, FILE=runOffMapFile(1:lengthName), STATUS='OLD',
c    &       ACCESS='direct', RECL=lengthRec )
c       READ(88,rec=1) tmpfld
        lengthRec=3*WORDLENGTH*2
        OPEN(88, FILE=runOffMapFile(1:lengthName), STATUS='OLD',
     &       ACCESS='direct', RECL=lengthRec )
        DO n=1,nROmap
         iRec = n
         READ(88,rec=iRec) r8seg
         tmpfld(1,n) = r8seg(1)
         tmpfld(2,n) = r8seg(2)
         tmpfld(3,n) = r8seg(3)
        ENDDO
        CLOSE(88)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( 3*nROmap, tmpfld )
#endif
c       n=nROmap
c       WRITE(msgUnit,'(A,3I5,F11.6)') 'ROmap:',n,nint(tmpfld(1,n)),
c    &                            NINT(tmpfld(2,n)),tmpfld(3,n)*1.d-9

C-    Read (ocean) grid cell area from file ;
      WRITE(msgUnit,'(2A)') 'SET_RUNOFFMAP: ','reading OCN grid area'
        lengthRec=Nx_ocn*Ny_ocn*WORDLENGTH*2
        OPEN(88, FILE='RA.bin', STATUS='OLD',
     &       ACCESS='direct', RECL=lengthRec )
        iRec = 1
        READ(88,rec=iRec) rAc
        CLOSE(88)
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( Nx_ocn*Ny_ocn, rAc )
#endif
c       WRITE(msgUnit,*) 'rAc=', rAc(1), rAc(17), rAc(17+16*Nx_ocn)

C----------------------------------------------------------

C-    Define mapping :
        DO n=1,nROmap
          ija = NINT(tmpfld(1,n))
          ijo = NINT(tmpfld(2,n))
          IF ( ija.LT.1 .OR. ija.GT.Nx_atm*Ny_atm ) THEN
            WRITE(msgUnit,'(2A)') '*** ERROR *** SET_RUNOFFMAP: ',
     &                            'ijROatm out of range !'
            STOP 'ABNORMAL END: S/R SET_RUNOFFMAP'
          ENDIF
          ijROatm(n) = ija
          IF ( ijo.LT.1 .OR. ijo.GT.Nx_ocn*Ny_ocn ) THEN
            WRITE(msgUnit,'(2A)') '*** ERROR *** SET_RUNOFFMAP: ',
     &                            'ijROocn out of range !'
            STOP 'ABNORMAL END: S/R SET_RUNOFFMAP'
          ELSEIF ( rAc(ijo).GT.0. ) THEN
            arROmap(n) = tmpfld(3,n)/rAc(ijo);
          ELSE
            arROmap(n) = 0.
          ENDIF
          ijROocn(n) = ijo
        ENDDO

C-      print to check :
        n = 1
        WRITE(msgUnit,'(A,3I5,F9.6)') ' check ROmap:',
     &                          n,ijROatm(n),ijROocn(n),arROmap(n)
        n = nROmap
        WRITE(msgUnit,'(A,3I5,F9.6)') ' check ROmap:',
     &                          n,ijROatm(n),ijROocn(n),arROmap(n)

      WRITE(msgUnit,'(2A,I9,A)') 'SET_RUNOFFMAP: ',
     &                     'done (nROmap=', nROmap, ' )'

      RETURN
      END
