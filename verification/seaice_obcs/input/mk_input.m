% Generate input files for MITgcm/verification/seaice_obcs
% the pkg/seaice + pkg/obcs verification experiment
% by carving them out of verification/lab_sea/tr_run.salt_plume

% readbin.m, writebin.m, and myint2str.m
% are in MITgcm/utils/matlab/cs_grid/read_cs

% first run lab_sea/tr_run.salt_plume with
%  endTime=43200., dumpFreq = 3600.

% preamble
%cd MITgcm/verification/seaice_obcs/input
nx=20; ny=16; nz=23; nt=11; ix=8:17; iy=4:11;
pn='../../lab_sea/tr_run.salt_plume/';

% pickup.0000000001.data
tmp=readbin([pn 'pickup.0000000001.data'],[nx ny 187],1,'real*8');
writebin('pickup.0000000001.data',tmp(ix,iy,:),1,'real*8');

% pickup_seaice.0000000001.data
tmp=readbin([pn 'pickup_seaice.0000000001.data'],[nx ny 8],1,'real*8');
writebin('pickup_seaice.0000000001.data',tmp(ix,iy,:),1,'real*8');

% U/V/T/S lateral boundary conditions
for f={'T','S','U','V'}
    x1=ix(1); x2=ix(end); y1=iy(1); y2=iy(end);
    if strcmp(f{1},'U'), x1=ix(2); end
    if strcmp(f{1},'V'), y1=iy(2); end
    for t=0:nt
        fn=[pn f{1} '.' myint2str(t,10) '.data'];
        if t==0
            fn=[pn f{1} '.' myint2str(1,10) '.data'];
        end
        tmp=readbin(fn,[nx ny nz]);
        for d={'N','S','E','W'}
            fo=['OB' d{1} lower(f{1}) '.seaice_obcs'];
            switch d{1}
              case 'N', writebin(fo,tmp(ix,y2,:),1,'real*4',t);
              case 'S', writebin(fo,tmp(ix,y1,:),1,'real*4',t);
              case 'E', writebin(fo,tmp(x2,iy,:),1,'real*4',t);
              case 'W', writebin(fo,tmp(x1,iy,:),1,'real*4',t);
            end
        end
    end
end

% sea ice lateral boundary conditions
fld={'AREA','HEFF','HSALT','HSNOW','UICE','VICE'};
nme={'a','h','sl','sn','uice','vice'};
for f=1:length(fld)
    x1=ix(1); x2=ix(end); y1=iy(1); y2=iy(end);
    if strcmp(fld{f},'UICE'), x1=ix(2); end
    if strcmp(fld{f},'VICE'), y1=iy(2); end
    for t=0:nt
        fldf=fld{f};
        if f==3
            fldf='SITRACER02';
        end
        fn=[pn fldf '.' myint2str(t,10) '.data'];
        if t==0
            fn=[pn fldf '.' myint2str(1,10) '.data'];
        end
        tmp=readbin(fn,[nx ny]);
        for d={'N','S','E','W'}
            fo=['OB' d{1} nme{f} '.seaice_obcs'];
            switch d{1}
              case 'N', writebin(fo,tmp(ix,y2,:),1,'real*4',t-1);
              case 'S', writebin(fo,tmp(ix,y1,:),1,'real*4',t-1);
              case 'E', writebin(fo,tmp(x2,iy,:),1,'real*4',t-1);
              case 'W', writebin(fo,tmp(x1,iy,:),1,'real*4',t-1);
            end
        end
    end
end

% bathymetry
tmp=readbin([pn 'bathy.labsea1979'],[nx ny]);
writebin('bathy.seaice_obcs',tmp(ix,iy));
