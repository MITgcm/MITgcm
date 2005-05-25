C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_out.F,v 1.14 2005/05/25 04:03:09 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_OUT

C     !INTERFACE:
      SUBROUTINE  DIAGNOSTICS_OUT(
     I     listnum,
     I     myIter,
     I     myTime,
     I     myThid )

C     !DESCRIPTION:
C     Write output for diagnostics fields.
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      INTEGER Nrphys
      PARAMETER (Nrphys=0)
#endif


C     !INPUT PARAMETERS:
C     listnum :: Diagnostics list number being written
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER listnum, myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j, k, m, n, bi, bj
      CHARACTER*8 parms1
      CHARACTER*3 mate_index
      INTEGER mate, mVec
      _RL qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL undef, getcon
      EXTERNAL getcon
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER ilen

      INTEGER ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL glf
#ifdef ALLOW_MNC
      INTEGER ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      CHARACTER*(5) ctmp
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
      CHARACTER*(NLEN) dn_blnk
      _RS ztmp(Nr+Nrphys)
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit= standardMessageUnit
      undef = getcon('UNDEF')
      glf = globalFiles
      WRITE(suff,'(I10.10)') myIter
      ilen = ILNBLNK(fnames(listnum))
      WRITE( fn, '(A,A,A)' ) fnames(listnum)(1:ilen),'.',suff(1:10)

#ifdef ALLOW_MNC
      IF (useMNC .AND. diag_mnc) THEN
        DO i = 1,MAX_LEN_FNAM
          diag_mnc_bn(i:i) = ' '
        ENDDO
        DO i = 1,NLEN
          dn_blnk(i:i) = ' '
        ENDDO
        WRITE( diag_mnc_bn, '(A)' ) fnames(listnum)(1:ilen)

C       Update the record dimension by writing the iteration number
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
        CALL MNC_CW_RL_W_S('D',diag_mnc_bn,0,0,'T',myTime,myThid)
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)

        dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        WRITE(dn(1),'(a,i6.6)') 'Zmd', nlevels(listnum)
        dim(1) = nlevels(listnum)
        ib(1)  = 1
        ie(1)  = nlevels(listnum)

        CALL MNC_CW_ADD_GNAME('diag_levels', 1, 
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('diag_levels', 'diag_levels', 
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('diag_levels','description',
     &       'Idicies of vertical levels within the source arrays',
     &       myThid)
        
        CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &       'diag_levels', levs(1,listnum), myThid)

        CALL MNC_CW_DEL_VNAME('diag_levels', myThid)
        CALL MNC_CW_DEL_GNAME('diag_levels', myThid)

C       Now define:  Zmdxxxxxx, Zudxxxxxx, Zldxxxxxx
        ctmp(1:5) = 'mul  '
        DO i = 1,3
          dn(1)(1:NLEN) = dn_blnk(1:NLEN)
          WRITE(dn(1),'(3a,i6.6)') 'Z',ctmp(i:i),'d',nlevels(listnum)
          CALL MNC_CW_ADD_GNAME(dn(1), 1, dim, dn, ib, ie, myThid)
          CALL MNC_CW_ADD_VNAME(dn(1), dn(1), 0,0, myThid)

C         The following three ztmp() loops should eventually be modified
C         to reflect the fractional nature of levs(j,l) -- they should
C         do something like:
C            ztmp(j) = rC(INT(FLOOR(levs(j,l)))) 
C                      + ( rC(INT(FLOOR(levs(j,l)))) 
C                          + rC(INT(CEIL(levs(j,l)))) )
C                        / ( levs(j,l) - FLOOR(levs(j,l)) )
C         for averaged levels.
          IF (i .EQ. 1) THEN
            DO j = 1,nlevels(listnum)
              ztmp(j) = rC(NINT(levs(j,listnum)))
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the mid point',
     &           myThid)
          ELSEIF (i .EQ. 2) THEN
            DO j = 1,nlevels(listnum)
              ztmp(j) = rF(NINT(levs(j,listnum)) + 1)
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the upper point',
     &           myThid)
          ELSEIF (i .EQ. 3) THEN
            DO j = 1,nlevels(listnum)
              ztmp(j) = rF(NINT(levs(j,listnum)))
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the lower point',
     &           myThid)
          ENDIF
          CALL MNC_CW_RS_W('D',diag_mnc_bn,0,0, dn(1), ztmp, myThid)
          CALL MNC_CW_DEL_VNAME(dn(1), myThid)
          CALL MNC_CW_DEL_GNAME(dn(1), myThid)
        ENDDO

      ENDIF
#endif /*  ALLOW_MNC  */

      DO n = 1,nfields(listnum)
        m = jdiag(n,listnum)
        parms1 = gdiag(m)(1:8)
        IF ( idiag(m).NE.0 .AND. parms1(5:5).NE.'D' ) THEN
C--     Start processing 1 Fld :

          IF ( ndiag(m).EQ.0 ) THEN
C-        Empty diagnostics case :

            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &        '- WARNING - from DIAGNOSTICS_OUT at iter=', myIter
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I4,3A,I3,2A)')
     &       '- WARNING -   diag.#',m, ' : ',flds(n,listnum), 
     &       ' (#',n,' ) in outp.Stream: ',fnames(listnum)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I2,A)')
     &       '- WARNING -   has not been filled (ndiag=',ndiag(m),' )'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A)')
     &       'WARNING DIAGNOSTICS_OUT  => write ZEROS instead'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            _END_MASTER( myThid )
            DO bj = myByLo(myThid), myByHi(myThid)
              DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listnum)
                  DO j = 1-OLy,sNy+OLy
                    DO i = 1-OLx,sNx+OLx
                      qtmp1(i,j,k,bi,bj) = 0. _d 0
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDDO

          ELSE
C-        diagnostics is not empty :

            IF ( myThid.EQ.1 ) 
     &             WRITE(ioUnit,2000) m,cdiag(m),ndiag(m),gdiag(m)

            IF ( parms1(5:5).EQ.'C' ) THEN
C             Check for Mate of a Counter Diagnostic
C             --------------------------------------
              mate_index = parms1(6:8)
              READ (mate_index,'(I3)') mate
              IF ( myThid.EQ.1 )
     &                 WRITE(ioUnit,2003) cdiag(m),mate,cdiag(mate)
            ELSE
              mate = 0

C             Check for Mate of a Vector Diagnostic
C             -------------------------------------
              IF ( parms1(1:1).EQ.'U' .OR. parms1(1:1).EQ.'V' ) THEN
                mate_index = parms1(6:8)
                READ (mate_index,'(I3)') mVec
                IF ( idiag(mVec).NE.0 ) THEN
                  IF ( myThid.EQ.1 )
     &                 WRITE(ioUnit,2001) cdiag(m),mVec,cdiag(mVec)
                ELSE
                  IF ( myThid.EQ.1 )
     &                 WRITE(ioUnit,2002) cdiag(m),mVec,cdiag(mVec)
                ENDIF
              ENDIF
            ENDIF

            DO bj = myByLo(myThid), myByHi(myThid)
             DO bi = myBxLo(myThid), myBxHi(myThid)
              DO k = 1,nlevels(listnum)
                CALL GETDIAG(
     I                       levs(k,listnum),undef,
     O                       qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                       m,mate,bi,bj,myThid)
              ENDDO
             ENDDO
            ENDDO

C-        end of empty diag / not empty block
          ENDIF

#ifdef ALLOW_MDSIO
C         Prepare for mdsio optionality
          IF (diag_mdsio) THEN
            IF (fflags(listnum)(1:1) .EQ. ' ') THEN
C             This is the old default behavior
              CALL mdswritefield_new(fn,writeBinaryPrec,glf,'RL',
     &             Nr+Nrphys,nlevels(listnum),qtmp1,n,myIter,myThid)
            ELSEIF (fflags(listnum)(1:1) .EQ. 'R') THEN
C             Force it to be 32-bit precision
              CALL mdswritefield_new(fn,precFloat32,glf,'RL',
     &             Nr+Nrphys,nlevels(listnum),qtmp1,n,myIter,myThid)
            ELSEIF (fflags(listnum)(1:1) .EQ. 'D') THEN
C             Force it to be 64-bit precision
              CALL mdswritefield_new(fn,precFloat64,glf,'RL',
     &             Nr+Nrphys,nlevels(listnum),qtmp1,n,myIter,myThid)
            ENDIF
          ENDIF
#endif /*  ALLOW_MDSIO  */

#ifdef ALLOW_MNC
          IF (useMNC .AND. diag_mnc) THEN

            _BEGIN_MASTER( myThid )

            DO ii = 1,CW_DIMS
              d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
              dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
            ENDDO

C           Note that the "d_cw_name" variable is a hack that hides a
C           subtlety within MNC.  Basically, each MNC-wrapped file is
C           caching its own concept of what each "grid name" (that is, a
C           dimension group name) means.  So one cannot re-use the same
C           "grid" name for different collections of dimensions within a
C           given file.  By appending the "m" values to each name, we
C           guarantee uniqueness within each MNC-produced file.
            WRITE(d_cw_name,'(a,i6.6)') 'd_cw_',m

C           XY dimensions
            dim(1)       = sNx + 2*OLx
            dim(2)       = sNy + 2*OLy
            ib(1)        = OLx + 1
            ib(2)        = OLy + 1
            IF (gdiag(m)(2:2) .EQ. 'M') THEN 
              dn(1)(1:2) = 'X'
              ie(1)      = OLx + sNx
              dn(2)(1:2) = 'Y'
              ie(2)      = OLy + sNy
            ELSEIF (gdiag(m)(2:2) .EQ. 'U') THEN
              dn(1)(1:3) = 'Xp1'
              ie(1)      = OLx + sNx + 1
              dn(2)(1:2) = 'Y'
              ie(2)      = OLy + sNy
            ELSEIF (gdiag(m)(2:2) .EQ. 'V') THEN
              dn(1)(1:2) = 'X'
              ie(1)      = OLx + sNx
              dn(2)(1:3) = 'Yp1'
              ie(2)      = OLy + sNy + 1
            ELSEIF (gdiag(m)(2:2) .EQ. 'Z') THEN
              dn(1)(1:3) = 'Xp1'
              ie(1)      = OLx + sNx + 1
              dn(2)(1:3) = 'Yp1'
              ie(2)      = OLy + sNy + 1
            ENDIF
            
C           Z is special since it varies
            WRITE(dn(3),'(a,i6.6)') 'Zd', nlevels(listnum)
            IF ( (gdiag(m)(10:10) .EQ. 'R')
     &           .AND. (gdiag(m)(9:9) .EQ. 'M') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zmd', nlevels(listnum)
            ENDIF
            IF ( (gdiag(m)(10:10) .EQ. 'R')
     &           .AND. (gdiag(m)(9:9) .EQ. 'L') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zld', nlevels(listnum)
            ENDIF
            IF ( (gdiag(m)(10:10) .EQ. 'R')
     &           .AND. (gdiag(m)(9:9) .EQ. 'U') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zud', nlevels(listnum)
            ENDIF
            dim(3) = Nr+Nrphys
            ib(3)  = 1
            ie(3)  = nlevels(listnum)

C           Time dimension
            dn(4)(1:1) = 'T'
            dim(4) = -1
            ib(4)  = 1
            ie(4)  = 1

            CALL MNC_CW_ADD_GNAME(d_cw_name, 4, 
     &             dim, dn, ib, ie, myThid)
            CALL MNC_CW_ADD_VNAME(cdiag(m), d_cw_name, 
     &             4,5, myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),'description',
     &             tdiag(m),myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),'units',
     &             udiag(m),myThid)

            IF ((fflags(listnum)(1:1) .EQ. ' ')
     &           .OR. (fflags(listnum)(1:1) .EQ. 'R')) THEN
              CALL MNC_CW_RL_W('R',diag_mnc_bn,0,0,
     &             cdiag(m), qtmp1, myThid)
            ELSEIF (fflags(listnum)(1:1) .EQ. 'D') THEN
              CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &             cdiag(m), qtmp1, myThid)
            ENDIF
            
            CALL MNC_CW_DEL_VNAME(cdiag(m), myThid)
            CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

            _END_MASTER( myThid )

          ENDIF
#endif /*  ALLOW_MNC  */

C--     end of Processing Fld # n
        ENDIF
      ENDDO

 2000 format(1x,'Computing Diagnostic # ',i3,2x,a8,5x,
     &     'Counter:',i8,3x,'Parms: ',a16)
 2001 format(1x,'          Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' exists ')
 2002 format(1x,'          Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' not enabled')
 2003 format(1x,'      use Counter Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8)
      
      RETURN 
      END
                                                                    
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
