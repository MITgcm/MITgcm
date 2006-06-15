C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/set_runoffmap.F,v 1.1 2006/06/15 23:05:26 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SET_RUNOFFMAP
C     *==========================================================*
C     | SUBROUTINE SET_RUNOFFMAP
C     | o define runoff mapping from atmos. grid (land) to
C     |   ocean grid
C     *==========================================================*
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "CPL_MAP2GRIDS.h"

C     == Routine arguments ==
CEndOfInterface

C     == Local variables ==
      INTEGER n, ijo, ija
      INTEGER lengthName, lengthRec, iRec
      Real*8  r8seg(3)
      Real*8  tmpfld(3,ROsize), rAc(Nx_ocn*Ny_ocn)

C-    Initialize to zero :
        DO n=1,ROsize
          ijROocn(n)=0
          ijROatm(n)=0
          arROmap(n)=0.
        ENDDO

C-    Read (ocean) grid cell area from file ;
        lengthRec=Nx_ocn*Ny_ocn*WORDLENGTH*2
        OPEN(88, FILE='RA.bin', STATUS='OLD',
     &       ACCESS='direct', RECL=lengthRec )
         iRec = 1
         READ(88,rec=iRec) rAc
         CLOSE(88)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( Nx_ocn*Ny_ocn, rAc )
#endif
c       write(6,*) 'rAc=', rAc(1), rAc(17), rAc(17+16*Nx_ocn)

c       lengthName=ILNBLNK( runoffmapFile ) ! eesup/src/utils.F not compiled here
        lengthName=0
        DO n=1,LEN( runoffmapFile )
         IF ( runoffmapFile(n:n).NE.' ' ) lengthName=n
        ENDDO
        write(6,'(3A,I6)') ' runoffmapFile =>>',
     &        runoffmapFile(1:lengthName),'<<= , nROmap=',nROmap
        IF ( lengthName.EQ.0 ) nROmap=0
        IF ( nROmap.EQ.0 ) RETURN

C-    Read area catchment from file ;
c       lengthRec=3*nROmap*WORDLENGTH*2
c       OPEN(88, FILE=runoffmapFile(1:lengthName), STATUS='OLD',
c    &       ACCESS='direct', RECL=lengthRec )
c        READ(88,rec=1) tmpfld
        lengthRec=3*WORDLENGTH*2
        OPEN(88, FILE=runoffmapFile(1:lengthName), STATUS='OLD',
     &       ACCESS='direct', RECL=lengthRec )
        DO n=1,nROmap
         iRec = n
         READ(88,rec=iRec) r8seg
         tmpfld(1,n)=r8seg(1)
         tmpfld(2,n)=r8seg(2)
         tmpfld(3,n)=r8seg(3)
        ENDDO
        CLOSE(88)
#ifdef _BYTESWAPIO
         CALL MDS_BYTESWAPR8( 3*nROmap, tmpfld )
#endif
c       n=nROmap
c       write(6,'(A,3I5,F11.6)') 'ROmap:',n,nint(tmpfld(1,n)),
c    &                            NINT(tmpfld(2,n)),tmpfld(3,n)*1.d-9

C----------------------------------------------------------

C-    Define mapping :
        DO n=1,nROmap
          ija=NINT(tmpfld(1,n))
          ijo=NINT(tmpfld(2,n))
          IF ( ija.LT.1 .OR. ija.GT.Nx_atm*Ny_atm ) THEN
            WRITE(0,*)'SET_RUNOFFMAP: ijROatm out of range !'
            STOP 'ABNORMAL END: S/R ATM_TO_OCN_MAPRUNOFF'
          ENDIF
          ijROatm(n)=ija
          IF ( ijo.LT.1 .OR. ijo.GT.Nx_ocn*Ny_ocn ) THEN
            write(0,*)'SET_RUNOFFMAP: ijROocn out of range !'
            STOP 'ABNORMAL END: S/R SET_RUNOFFMAP'
          ELSEIF ( rAc(ijo).GT.0. ) THEN
            arROmap(n)=tmpfld(3,n)/rAc(ijo);
          ENDIF
          ijROocn(n)=ijo
        ENDDO

C-      print to check :
        n=1
        write(6,'(A,3I5,F9.6)') ' ROmap:',
     &                          n,ijROatm(n),ijROocn(n),arROmap(n)
        n=nROmap
        write(6,'(A,3I5,F9.6)') ' ROmap:',
     &                          n,ijROatm(n),ijROocn(n),arROmap(n)

      RETURN
      END
