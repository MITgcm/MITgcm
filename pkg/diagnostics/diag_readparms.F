C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diag_readparms.F,v 1.9 2004/07/06 03:55:53 edhill Exp $
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
      LOGICAL use_mdsio, use_mnc
      INTEGER mnc_lev_used(ndim)

      namelist / diagnostics_list /
     &     frequency, levels, fields, filename,
     &     use_mdsio, use_mnc

C **********************************************************************
C ****           Initialize and Read Diagnostics Namelist           ****
C **********************************************************************
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

      use_mdsio = .true.
      use_mnc   = .false.

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

C **********************************************************************
C ****         Initialise diag_choices common block                 ****
C **********************************************************************

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

C **********************************************************************
C ****         Fill Diagnostics Common Block with Namelist Info     ****
C **********************************************************************

      diag_use_mdsio = use_mdsio
      diag_use_mnc   = use_mnc

      do n = 1,numlists
        if (frequency(n) .ne. 0) then
          nlists = nlists + 1
          freq(n)    = frequency(n)
          fnames(n)  = filename (n)
          nlevels(n) = 0
          nfields(n) = 0
          do m=1,ndim
            if (levels(m,n) .ne. undef) nlevels(n) = nlevels(n) + 1
            if (fields(m,n) .ne. blank) nfields(n) = nfields(n) + 1
          enddo
          do m=1,nlevels(n)
            levs(m,n) = levels(m,n)
          enddo
          do m=1,nfields(n)
            flds(m,n) = fields(m,n)

#ifdef ALLOW_MNC
            IF (useMNC .and. diag_use_mnc) THEN
              CALL DIAG_MNC_ADD_GNAME(
     &             nlevels(n), ndim, mnc_lev_used, myThid)
            ENDIF
#endif /*  ALLOW_MNC  */

          enddo
        endif
      enddo

#ifdef ALLOW_MNC
      IF (useMNC .and. diag_use_mnc) THEN
C       CALL MNC_CW_ADD_GNAME()
      ENDIF
#endif /*  ALLOW_MNC  */


C **********************************************************************
C ****               Echo History List Data Structure               ****
C **********************************************************************

      do n = 1,nlists
        WRITE(msgBuf,'(2a)') 'Creating Output Stream: ',fnames(n)
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
        WRITE(msgBuf,*) 'Frequency: ',freq(n)
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
        WRITE(msgBuf,*) 'Levels:    ',(levs(l,n),l=1,nlevels(n))
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
        WRITE(msgBuf,*) 'Fields:   ',(' ',flds(l,n),l=1,nfields(n))
        CALL PRINT_MESSAGE(
     &       msgBuf, standardMessageUnit, SQUEEZE_RIGHT, mythid)
      enddo

      _END_MASTER(myThid)

      return
      end


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: DIAG_MNC_ADD_GNAME

C     !INTERFACE:
      SUBROUTINE DIAG_MNC_ADD_GNAME(nlev, ndim, mnc_lev_used, myThid)

C     !DESCRIPTION:
C     Create MNC Gnames for the diagnostics output.
      
C     !USES:
      implicit none
#include "SIZE.h"

C     !INPUT PARAMETERS:
      integer nlev, ndim, myThid
      integer mnc_lev_used(nlev)
CEOP

C     !LOCAL VARIABLES:
      integer i, NCHAR
      parameter ( NCHAR = 150 )
      character*(100) gname

      do i = 1,ndim
        if (nlev .eq. mnc_lev_used(i)) then
          return
        endif
      enddo

      do i = 1,NCHAR
        gname(i:i) = ' '
      enddo

      write(gname, '(a)') 

      return
      end

