C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_readparms.F,v 1.3 2004/06/10 20:53:19 molod Exp $
C $Name:  $

      subroutine fizhi_readparms (myThid)
C***********************************************************************
C  Purpose
C  -------
C    Read Fizhi Namelist and Get the Model Date and Time from File
C
C  Arguments  Description
C  ----------------------
C     myThid ... Process(or) number
C***********************************************************************
      implicit none

#include "CPP_OPTIONS.h"
#include "chronos.h"
#include "EEPARAMS.h"

      integer myThid

c Declare Local Variables
c -----------------------
      character*(MAX_LEN_MBUF) msgBuf
      integer ku
      integer nymdbegin, nhmsbegin
      integer nymdcurrent, nhmscurrent

      namelist / fizhi_list / nymdbegin, nhmsbegin

C **********************************************************************
C ****           Initialize and Read Fizhi Namelist                 ****
C **********************************************************************

      WRITE(msgBuf,'(A)') ' FIZHI_READPARMS: opening data.fizhi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.fizhi', 'FIZHI_READPARMS',
     .                          ku,myThid )
      read  (ku,NML=fizhi_list)
      close (ku)

C **********************************************************************
C ****      Read Supplemental Ascii File with Current Time Info     ****
C **********************************************************************

      open(ku,file='datetime',form='formatted')
      read(10,1000)nymdcurrent,nhmscurrent
 1000 format(i8,2x,i6)

C **********************************************************************
C **** Fill chronos Common Block with Namelist and Other File Info  ****
C **********************************************************************

      nymd0 = nymdbegin
      nhms0 = nhmsbegin
      nymd = nymdcurrent
      nhms = nhmscurrent

C **********************************************************************
C ****               Echo Date and Time Info                        ****
C **********************************************************************

      _BEGIN_MASTER(myThid)
      print *, '  Begin Date ',nymd0,'   Begin Time ',nhms0
      print *, 'Current Date ',nymd,' Current Time ',nhms
      _END_MASTER(myThid)

      return
      end
