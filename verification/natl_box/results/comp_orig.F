C     Compare KPPhbl_01_01.00000_00720_240 to KPPhbl_orig.
C     for North Atlantic test domain.
C     Maximum difference between two files is reported.
C     On SGI compile using "f77 -bytereclen comp_jpl.f"

      program comp_orig

      implicit none

      integer nx, ny, nrec
      parameter (nx=20, ny=16, nrec=4)

      integer lbuffer1, lbuffer2, i, j, k, irec
      character fni*80, fno*80
      real*4 fld1(nx,ny), fld2(nx,ny), maxdiff

      fni='KPPhbl.001.001.data'
      fno='KPPhbl_orig'
      lBuffer1=nx*ny*4
      lBuffer2=nx*ny*4
      open(1,file=fni,access='direct',recl=lBuffer1)
      open(2,file=fno,access='direct',recl=lBuffer2)

      irec=0
      maxdiff=0.
      do k=1,nrec
         irec=irec+1
         read(1,rec=irec) ((fld1(i,j),i=1,nx),j=1,ny)
         read(2,rec=irec) ((fld2(i,j),i=1,nx),j=1,ny)
         do i=1,nx
            do j=1,ny
               maxdiff=max(maxdiff,abs(fld1(i,j)-fld2(i,j)))
            enddo
         enddo
      enddo
      close(1)
      close(2)

      if(maxdiff.lt.5e-4) then
         print*,'North Atlantic test passed.  KPPhbl file is unchanged.'
      else
         print*,'WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!'
         print*,'North Atlantic test failed.  KPPhbl file has changed.'
      endif
      print*,'Maximum difference is',maxdiff,' m.'

      stop
      end
