nx=20; ny=16; nz=23; nt=14; ts=10; ix=8:17; iy=4:11;
pn='../../lab_sea/tr_run.salt_plume/';

tmp=readbin([pn 'bathy.labsea1979'],[nx ny]);
writebin('bathy.seaice_obcs',tmp(ix,iy));

for f={'T','S','U','V'}
    fn=[pn f{1} '.0000000000.data'];
    tmp=readbin(fn,[nx ny nz]);
    writebin([f{1} '.seaice_obcs'],tmp(ix,iy,:));
end

for f={'AREA','HSNOW','HSALT','HEFF'}
    fn=[pn f{1} '.0000000000.data'];
    tmp=readbin(fn,[nx ny]);
    writebin([f{1} '.seaice_obcs'],tmp(ix,iy));
end

for f={'tair','qa','u10m','v10m','prate','flo','fsh','SSS_monthly'}
    fn=[pn f{1} '.labsea1979'];
    tmp=readbin(fn,[nx ny nt]);
    writebin([f{1} '.seaice_obcs'],tmp(ix,iy,:));
end

for f={'T','S','U','V'}
    x1=ix(1); x2=ix(end); y1=iy(1); y2=iy(end);
    if strcmp(f{1},'U'), x1=ix(2); end
    if strcmp(f{1},'V'), y1=iy(2); end
    for t=0:ts
        fn=[pn f{1} '.' myint2str(t,10) '.data'];
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

fld={'AREA','HEFF','HSALT','HSNOW','UICE','VICE'};
nme={'a','h','sl','sn','uice','vice'};
for f=1:length(fld)
    x1=ix(1); x2=ix(end); y1=iy(1); y2=iy(end);
    if strcmp(fld{f},'UICE'), x1=ix(2); end
    if strcmp(fld{f},'VICE'), y1=iy(2); end
    for t=0:ts
        fn=[pn fld{f} '.' myint2str(t,10) '.data'];
        tmp=readbin(fn,[nx ny]);
        for d={'N','S','E','W'}
            fo=['OB' d{1} nme{f} '.seaice_obcs'];
            switch d{1}
              case 'N', writebin(fo,tmp(ix,y2,:),1,'real*4',t);
              case 'S', writebin(fo,tmp(ix,y1,:),1,'real*4',t);
              case 'E', writebin(fo,tmp(x2,iy,:),1,'real*4',t);
              case 'W', writebin(fo,tmp(x1,iy,:),1,'real*4',t);
            end
        end
    end
end
