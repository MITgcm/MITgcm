C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diag_readparms.F,v 1.17 2004/10/20 18:54:21 molod Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAG_READPARMS

C     !INTERFACE:
      SUBROUTINE DIAG_READPARMS(myThid)

C     !DESCRIPTION:
C     Read Diagnostics Namelists to specify output sequence.
      
C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "diagnostics_SIZE.h"
#include "diagnostics.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      character*(MAX_LEN_MBUF) msgBuf
      integer ku
      integer l,n,m,ndim
      _RL undef, getcon
      parameter ( ndim = 100 )       ! Max Number of Levels,Fields,Lists
      integer frequency(ndim)        ! Frequency of Output (hhmmss)
      _RL levels (ndim,ndim)         ! List Output Levels
      character*8 fields(ndim,ndim)  ! List Output Fields
      character*8 filename(ndim)     ! List Output Filename
      character*8 blank
      LOGICAL diag_mdsio, diag_mnc
      INTEGER mnc_lev_used(ndim)
      integer levrest, ll

      namelist / diagnostics_list /
     &     frequency, levels, fields, filename,
     &     diag_mdsio, diag_mnc

C     Initialize and Read Diagnostics Namelist
      _BEGIN_MASTER(myThid)

      undef = getcon('UNDEF')
      blank  = '        '

      do n = 1,ndim
        frequency(n) = 0
        do m = 1,ndim
          levels (n,m) = undef
          fields (n,m) = blank
        enddo
      enddo

      diag_mdsio = .true.
      diag_mnc   = .false.

#ifdef ALLOW_MNC
      IF (useMNC .and. diag_use_mnc) THEN
        do n = 1,ndim
          mnc_lev_used(n) = 0
        enddo
      ENDIF
#endif /*  ALLOW_MNC  */
      
      WRITE(msgBuf,'(A)') ' DIAG_READPARMS: opening data.diagnostics'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)
      
      CALL OPEN_COPY_DATA_FILE('data.diagnostics', 'DIAG_READPARMS',
     .                          ku,myThid )
      read  (ku,NML=diagnostics_list)
      close (ku)


C     Initialise diag_choices common block
      nlists = 0
      do n = 1,numlists
        freq(n) = 0
        nlevels(n) = 0
        nfields(n) = 0
        fnames(n) = blank
        do m = 1,numperlist
          levs(m,n) = 0
          flds(m,n) = '        '
        enddo
      enddo

C     Fill Diagnostics Common Block with Namelist Info
      diag_use_mdsio = diag_mdsio
      diag_use_mnc   = diag_mnc
      IF (diag_mnc .AND. (.NOT. outputTypesInclusive)) THEN
        diag_use_mdsio = .FALSE.
      ENDIF
      
      do n = 1,numlists
        if (frequency(n) .ne. 0) then
          nlists = nlists + 1
          freq(n)    = frequency(n)
          fnames(n)  = filename (n)
          nlevels(n) = 0
          nfields(n) = 0
          do m=1,ndim
            if (levels(m,n) .ne. undef) then
              nlevels(n) = nlevels(n) + 1
            endif
            if (fields(m,n) .ne. blank) nfields(n) = nfields(n) + 1
          enddo
          if (levels(1,n) .ne. undef) then
            do m=1,nlevels(n)
              levs(m,n) = levels(m,n)
            enddo
          else
            nlevels(n) = Nr
            do m=1,nlevels(n)
              levs(m,n) = m
            enddo
          endif
          do m=1,nfields(n)
            flds(m,n) = fields(m,n)
          enddo
        endif
      enddo

C     Echo History List Data Structure
      do n = 1,nlists
        WRITE(msgBuf,'(2a)') 'Creating Output Stream: ',fnames(n)
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
        WRITE(msgBuf,*) 'Frequency: ',freq(n)
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
	do L=1,nlevels(n),20
        levrest = min(nlevels(n),20)
        WRITE(msgBuf,*) 'Levels:    ',(levs(LL,n),LL=L,L+levrest-1)
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
	enddo
        WRITE(msgBuf,*) 'Fields:   ',(' ',flds(l,n),l=1,nfields(n))
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
      enddo

      _END_MASTER(myThid)

      return
      end
