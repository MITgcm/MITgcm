C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagout.F,v 1.19 2004/09/22 02:50:49 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGOUT

C     !INTERFACE:
      SUBROUTINE  DIAGOUT(
     I     myThid,
     I     myIter,
     I     listnum)

C     !DESCRIPTION:
C     Write output for diagnostics fields.
      
C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      integer Nrphys
      parameter (Nrphys=0)
#endif

#include "diagnostics_SIZE.h"
#include "diagnostics.h"

C     !INPUT PARAMETERS:
      integer myThid, myIter, listnum
CEOP

      integer i, j, k, m, n, bi, bj
      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer mate
      _RL qtmp1(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr+Nrphys,Nsx,Nsy)
      _RL qtmp2(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr+Nrphys,Nsx,Nsy)
      _RL undef, getcon
      external getcon
      integer ilnblnk
      external ilnblnk
      integer ilen

      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )

      CHARACTER*(MAX_LEN_FNAM) pref
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(80) fn
      logical glf
#ifdef ALLOW_MNC
      integer ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn

      integer CW_DIMS, NLEN
      parameter ( CW_DIMS = 10 )
      parameter ( NLEN    = 80 )
      integer dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      character*(NLEN) dn(CW_DIMS)
      character*(NLEN) dn_blnk
#endif /*  ALLOW_MNC  */

      undef = getcon('UNDEF')
      glf = globalFiles
      WRITE(suff,'(I10.10)') myIter
      pref = fnames(listnum)
      ilen=ilnblnk( pref )
      WRITE( fn, '(A,A,A)' ) pref(1:ilen),'.',suff(1:10)

#ifdef ALLOW_MNC
      IF (useMNC .and. diag_use_mnc) THEN
        DO i = 1,MAX_LEN_FNAM
          diag_mnc_bn(i:i) = ' '
        ENDDO
        DO i = 1,NLEN
          dn_blnk(i:i) = ' '
        ENDDO
        WRITE( diag_mnc_bn, '(A,A)' ) 'diag.', pref(1:ilen)

C       Update the record dimension by writing the iteration number
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
        CALL MNC_CW_I_W('I',diag_mnc_bn,0,0,'iter', myIter, myThid)
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)

        dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        write(dn(1),'(a,i6.6)') 'Zd', nlevels(listnum)
        dim(1) = nlevels(listnum)
        ib(1)  = 1
        ie(1)  = nlevels(listnum)

        CALL MNC_CW_ADD_GNAME('diag_levels', 1, 
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('level_indicies', 'diag_levels', 
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('level_indicies',1,'description',
     &       'Idicies of vertical levels within the data source arrays',
     &       myThid)
        
        CALL MNC_CW_RL_W('I',diag_mnc_bn,0,0,
     &       'level_indicies', levs(1,listnum), myThid)

        CALL MNC_CW_DEL_VNAME('level_indicies', myThid)
        CALL MNC_CW_DEL_GNAME('diag_levels', myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

      do n = 1,nfields(listnum)
        do m = 1,ndiagt
          if( flds(n,listnum).eq.cdiag(m) .and. idiag(m).ne.0 ) then
            parms1 = gdiag(m)
            if (ndiag(m).ne.0.and.parse1(5).ne.'D') then
              if( myThid.eq.1 ) 
     &             write(6,2000) m,cdiag(m),ndiag(m),gdiag(m)
              if (parse1(5).ne.'C') then

                do k = 1,nlevels(listnum)
                  call getdiag ( myThid,levs(k,listnum),m,undef,qtmp1)
                enddo

C               Check for Mate of a Vector Diagnostic
C               -------------------------------------
                if ( parse1(1).eq.'U' .or. parse1(1).eq.'V' ) then
                  read (mate_index,100) mate
                  if( idiag(mate).ne.0 ) then
                    if( myThid.eq.1 )
     &                   write(6,2001) cdiag(m),mate,cdiag(mate)
                  else
                    if( myThid.eq.1 )
     &                   write(6,2002) cdiag(m),mate,cdiag(mate)
                  endif
                endif
                
              else

C               Check for Mate of a Counter Diagnostic
C               --------------------------------------
                read (mate_index,100) mate
                do k = 1,nlevels(listnum)
                  call getdiag2(myThid,levs(k,listnum),m,undef,qtmp1)
                  call getdiag2(myThid,levs(k,listnum),mate,undef,qtmp2)
                  do bj = myByLo(myThid), myByHi(myThid)
                    do bi = myBxLo(myThid), myBxHi(myThid)
                      do j = 1,sNy
                        do i = 1,sNx
                          if (qtmp2(i,j,k,bi,bj).ne.0.) then
                            qtmp1(i,j,k,bi,bj) = 
     &                           qtmp1(i,j,k,bi,bj) / qtmp2(i,j,k,bi,bj)
                          else
                            qtmp1(i,j,k,bi,bj) = undef
                          endif
                        enddo
                      enddo
                    enddo
                  enddo
                enddo

              endif
            endif

#ifdef ALLOW_MDSIO
C           Prepare for mdsio optionality
C           IF (useMDSIO) THEN
            IF (diag_use_mdsio) THEN
              call mdswritefield_new(fn,64,glf,'RL',Nr+Nrphys,
     &             nlevels(listnum),qtmp1,n,myIter,myThid)
            ENDIF
C           ENDIF
#endif /*  ALLOW_MDSIO  */

#ifdef ALLOW_MNC
            IF (useMNC .and. diag_use_mnc) THEN

              _BEGIN_MASTER( myThid )

              do ii = 1,CW_DIMS
                dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
              enddo

              dn(1)(1:2) = 'Xd'
              dim(1) = sNx + 2*OLx
              ib(1)  = OLx + 1
              ie(1)  = OLx + sNx
              dn(2)(1:2) = 'Yd'
              dim(2) = sNy + 2*OLy
              ib(2)  = OLy + 1
              ie(2)  = OLy + sNy

C             Z is special since it varies
              write(dn(3),'(a,i6.6)') 'Zd', nlevels(listnum)
              dim(3) = Nr+Nrphys
              ib(3)  = 1
              ie(3)  = nlevels(listnum)

              CALL MNC_CW_ADD_GNAME('diag_cw_temp', 3, 
     &             dim, dn, ib, ie, myThid)
              CALL MNC_CW_ADD_VNAME(cdiag(m), 'diag_cw_temp', 
     &             4,5, myThid)
              CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),1,'description',
     &             tdiag(m),myThid)
              CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),1,'units',
     &             udiag(m),myThid)

              CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &             cdiag(m), qtmp1, myThid)

              CALL MNC_CW_DEL_VNAME(cdiag(m), myThid)
              CALL MNC_CW_DEL_GNAME('diag_cw_temp', myThid)

              _END_MASTER( myThid )

            ENDIF
#endif /*  ALLOW_MNC  */

          endif
        enddo
      enddo

 100  format(i3)
 2000 format(1x,'Computing Diagnostic # ',i3,2x,a8,5x,'Counter: ',
     &     i3,6x,'Parms: ',a16)
 2001 format(1x,'          Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' exists ')
 2002 format(1x,'        Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' not enabled')
      
      return
      end
                                                                    
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
