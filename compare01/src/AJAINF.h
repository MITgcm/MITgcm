C $Id: AJAINF.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C     Global data holding "C-D" scheme information. 
C     uAja - "D" grid U velocity ( at V point on C grid ).
C     vAja - "D" grid V velocity ( at U point on C grid )
C     PNM1 - pressure from previous time step.
C     uBxByNM1 - u spatially averaged to C grid v point from previous time step.
C     vBxByNM1 - v spatially averaged to C grid u point from previous time step.
C
      COMMON /CDVARS/ uAja, vAja, PSNM1, uNM1, vNM1, PHNM1
      REAL uAja(_I3(Nz,Nx,Ny))
CMF$LAYOUT uAja(:SERIAL,:NEWS,:NEWS) 
      REAL vAja(_I3(Nz,Nx,Ny))
CMF$LAYOUT vAja(:SERIAL,:NEWS,:NEWS) 
      REAL PSNM1(Nx,Ny)
CMF$LAYOUT PSNM1(:SERIAL,:NEWS,:NEWS) 
      REAL PHNM1(_I3(Nz,Nx,Ny))
CMF$LAYOUT PHNM1(:SERIAL,:NEWS,:NEWS) 
      REAL uNM1(_I3(Nz,Nx,Ny))
CMF$LAYOUT uNM1(:SERIAL,:NEWS,:NEWS) 
      REAL vNM1(_I3(Nz,Nx,Ny))
CMF$LAYOUT vNM1(:SERIAL,:NEWS,:NEWS) 
