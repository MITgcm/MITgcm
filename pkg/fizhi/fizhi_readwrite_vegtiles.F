C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_readwrite_vegtiles.F,v 1.20 2009/06/28 01:05:41 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIZHI_WRITE_VEGTILES
C     !INTERFACE:
      SUBROUTINE FIZHI_WRITE_VEGTILES(fn,pickupflg,myTime,myIter,myThid)

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

      EXTERNAL ILNBLNK
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) fn
      INTEGER pickupflg
      _RL myTime
      INTEGER myIter
      INTEGER myThid

CEOP
C     !LOCAL VARIABLES:
      CHARACTER*1 prec
      CHARACTER*80 bnam
      character*(80) dataFName
      integer ilst
      integer i,k,n
      integer ig,jg,tn,iunit
      integer length_of_rec
      integer bi,bj,irec,fileprec
      Real*8 r8seg(nchp)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO i = 1,80
        bnam(i:i) = ' '
      ENDDO
      ilst = ILNBLNK(fn)
      if (pickupflg.eq.0) then
        prec = 'D'
        fileprec = 64
        WRITE(bnam,'(a,a)') 'pickup_vegtiles.', fn(1:ilst)
      else
        prec = 'D'
        fileprec = 64
        WRITE(bnam,'(a,a)') 'state_vegtiles.', fn(1:ilst)
      endif

#ifdef ALLOW_MNC
      IF (useMNC.AND. pickup_write_mnc) THEN

C       Write fizhi veg-space variables using the MNC package
        CALL MNC_CW_SET_UDIM(bnam, 1, myThid)
        CALL MNC_CW_RL_W_S('D',bnam,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',bnam,0,0,'iter',myIter,myThid)

C       fizhi_coms.h
        CALL MNC_CW_RL_W(prec,bnam,0,0,'ctmt', ctmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'xxmt', xxmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'yymt', yymt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'zetamt', zetamt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'xlmt', xlmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'khmt', khmt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tke', tke, myThid)

C       fizhi_land_coms.h
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tcanopy', tcanopy, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'tdeep', tdeep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'ecanopy', ecanopy, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetshal', swetshal, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetroot', swetroot, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'swetdeep', swetdeep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'snodep', snodep, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'capac', capac, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chlt', chlt, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chlon', chlon, myThid)
        CALL MNC_CW_I_W('I',bnam,0,0,'igrd', igrd, myThid)

C       fizhi_earth_coms.h
        CALL MNC_CW_I_W('I',bnam,0,0,'ityp', ityp, myThid)
        CALL MNC_CW_RL_W(prec,bnam,0,0,'chfr', chfr, myThid)

      ENDIF
#endif /*  Not ALLOW_MNC sequence */


      call MDSFINDUNIT( iunit, mythid )
      length_of_rec=MDS_RECLEN( fileprec, nchp, mythid )

      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_EXCH2
       tn = W2_myTileList(bi,bj)
       iG = tn
       jG = 1
#else
       iG = bi+(myXGlobalLo-1)/sNx
       jG = bj+(myYGlobalLo-1)/sNy
       tn = (jG - 1)*(nPx*nSx) + iG
#endif /* ALLOW_EXCH2 */

       write(dataFname(1:80),'(a,2a,i3.3,a,i3.3,a)')
     &  'pickup_vegtiles.',fn(1:ilst),'.',iG,'.',jG,'.data'
       open( iUnit, file=dataFName, status='unknown',
     &       access='direct', recl=length_of_rec )

C First write single-level turbulence fields
       do n = 1,nchp
        r8seg(n) = ctmt(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       write(iunit,rec=1) r8seg

       do n = 1,nchp
        r8seg(n) = xxmt(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       write(iunit,rec=2) r8seg

       do n = 1,nchp
        r8seg(n) = yymt(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       write(iunit,rec=3) r8seg

       do n = 1,nchp
        r8seg(n) = zetamt(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       write(iunit,rec=4) r8seg

C And now write Multi-level turbulence fields
       do k = 1,Nrphys
        do n = 1,nchp
         r8seg(n) = xlmt(n,k,bi,bj)
        enddo
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        irec = 4 + 0*Nrphys + k
        write(iunit,rec=irec) r8seg
       enddo

       do k = 1,Nrphys
        do n = 1,nchp
         r8seg(n) = khmt(n,k,bi,bj)
        enddo
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        irec = 4 + 1*Nrphys + k
        write(iunit,rec=irec) r8seg
       enddo

       do k = 1,Nrphys
        do n = 1,nchp
         r8seg(n) = tke(n,k,bi,bj)
        enddo
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        irec = 4 + 2*Nrphys + k
        write(iunit,rec=irec) r8seg
       enddo

C And finally, write land surface fields
       do n = 1,nchp
        r8seg(n) = tcanopy(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 1
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = tdeep(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 2
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = ecanopy(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 3
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = swetshal(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 4
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = swetroot(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 5
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = swetdeep(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 6
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = snodep(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 7
       write(iunit,rec=irec) r8seg

       do n = 1,nchp
        r8seg(n) = capac(n,bi,bj)
       enddo
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       irec = 4 + 3*Nrphys + 8
       write(iunit,rec=irec) r8seg

       close(iunit)

C End of bi bj loop
      enddo
      enddo

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIZHI_READ_VEGTILES
C     !INTERFACE:
      SUBROUTINE FIZHI_READ_VEGTILES(Iter,prec,myThid)

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

      EXTERNAL ILNBLNK
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*1 prec
      INTEGER Iter
      INTEGER myThid

CEOP
C     !LOCAL VARIABLES:
      CHARACTER*80 fn
      CHARACTER*80 bnam
      integer ilst
      character*(80) dataFName
      integer i,k,n
      integer ig,jg,tn,iunit
      integer length_of_rec
      integer bi,bj,irec,fileprec
      Real*8 r8seg(nchp)

      DO i = 1,80
        bnam(i:i) = ' '
      ENDDO
      WRITE(fn,'(a,I10.10)') 'pickup_vegtiles.',Iter
      ilst = ILNBLNK(fn)
      WRITE(bnam,'(a,I10.10)') 'pickup_vegtiles.',Iter
      fileprec = 64

#ifdef ALLOW_MNC
      IF (useMNC.AND. pickup_write_mnc) THEN

C       Write fizhi veg-space variables using the MNC package
        CALL MNC_FILE_CLOSE_ALL_MATCHING(bnam, myThid)
        CALL MNC_CW_SET_UDIM(bnam, 1, myThid)

C       fizhi_coms.h
        CALL MNC_CW_RL_R(prec,bnam,0,0,'ctmt', ctmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'xxmt', xxmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'yymt', yymt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'zetamt', zetamt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'xlmt', xlmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'khmt', khmt, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tke', tke, myThid)

C       fizhi_land_coms.h
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tcanopy', tcanopy, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'tdeep', tdeep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'ecanopy', ecanopy, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetshal', swetshal, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetroot', swetroot, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'swetdeep', swetdeep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'snodep', snodep, myThid)
        CALL MNC_CW_RL_R(prec,bnam,0,0,'capac', capac, myThid)

      ENDIF
#endif /*  Not ALLOW_MNC sequence */

      call MDSFINDUNIT( iunit, mythid )
      length_of_rec=MDS_RECLEN( fileprec, nchp, mythid )

      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_EXCH2
       tn = W2_myTileList(bi,bj)
       iG = tn
       jG = 1
#else
       iG = bi+(myXGlobalLo-1)/sNx
       jG = bj+(myYGlobalLo-1)/sNy
       tn = (jG - 1)*(nPx*nSx) + iG
#endif /* ALLOW_EXCH2 */

       write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fn(1:ilst),'.',iG,'.',jG,'.data'
       print *,' Opening ',dataFName
       open( iUnit, file=dataFName, status='old',
     &       access='direct', recl=length_of_rec )

       irec = 0
C First read single-level turbulence fields
       read(iunit,rec=1) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        ctmt(n,bi,bj) = r8seg(n)
       enddo

       read(iunit,rec=2) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        xxmt(n,bi,bj) = r8seg(n)
       enddo

       read(iunit,rec=3) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        yymt(n,bi,bj) = r8seg(n)
       enddo

       read(iunit,rec=4) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        zetamt(n,bi,bj) = r8seg(n)
       enddo

C And now read Multi-level turbulence fields
       do k = 1,Nrphys
        irec = 4 + 0*Nrphys + k
        read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        do n = 1,nchp
         xlmt(n,k,bi,bj) = r8seg(n)
        enddo
       enddo

       do k = 1,Nrphys
        irec = 4 + 1*Nrphys + k
        read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        do n = 1,nchp
         khmt(n,k,bi,bj) = r8seg(n)
        enddo
       enddo

       do k = 1,Nrphys
        irec = 4 + 2*Nrphys + k
        read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( nchp, r8seg )
#endif
        do n = 1,nchp
         tke(n,k,bi,bj) = r8seg(n)
        enddo
       enddo

C And finally, read land surface fields
       irec = 4 + 3*Nrphys + 1
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        tcanopy(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 2
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        tdeep(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 3
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        ecanopy(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 4
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        swetshal(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 5
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        swetroot(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 6
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        swetdeep(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 7
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        snodep(n,bi,bj) = r8seg(n)
       enddo

       irec = 4 + 3*Nrphys + 8
       read(iunit,rec=irec) r8seg
#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR8( nchp, r8seg )
#endif
       do n = 1,nchp
        capac(n,bi,bj) = r8seg(n)
       enddo

       close(iunit)

C End of bi bj loop
      enddo
      enddo


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
