
nc=32; ncp=nc+1;

inpName='tile.mitgrid';
rDir='./';
vvg=zeros(ncp,ncp,16);

for n=1:6,
%-- read :
%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
 namV=sprintf('LONC.%3.3i.bin',n);
 var=rdda(namV,[nc nc],1,'real*8','b'); vvg(1:nc,1:nc,1)=var;
 namV=sprintf('LATC.%3.3i.bin',n);
 var=rdda(namV,[nc nc],1,'real*8','b'); vvg(1:nc,1:nc,2)=var;
 namV='DXF.bin'; 
 var=rdda(namV,[nc nc],1,'real*8','b'); vvg(1:nc,1:nc,3)=var;
 namV='DYF.bin';
 var=rdda(namV,[nc nc],1,'real*8','b'); vvg(1:nc,1:nc,4)=var;
 namV='RA.bin';
 var=rdda(namV,[nc nc],1,'real*8','b'); vvg(1:nc,1:nc,5)=var;
%--
 namV=sprintf('LONG.%3.3i.bin',n);
 var=rdda(namV,[ncp ncp],1,'real*8','b'); vvg(:,:,6)=var;
 namV=sprintf('LATG.%3.3i.bin',n);
 var=rdda(namV,[ncp ncp],1,'real*8','b'); vvg(:,:,7)=var;
 namV='DXV.bin';
 var=rdda(namV,[ncp ncp],1,'real*8','b'); vvg(:,:,8)=var;
 namV='DYU.bin';
 var=rdda(namV,[ncp ncp],1,'real*8','b'); vvg(:,:,9)=var;
 namV='RAZ.bin';
 var=rdda(namV,[ncp ncp],1,'real*8','b'); vvg(:,:,10)=var;
%--
 namV='DXC.bin';
 var=rdda(namV,[ncp nc],1,'real*8','b'); vvg(:,1:nc,11)=var;
 namV='DYC.bin';
 var=rdda(namV,[nc ncp],1,'real*8','b'); vvg(1:nc,:,12)=var;
 namV='RAW.bin';
 var=rdda(namV,[ncp nc],1,'real*8','b'); vvg(:,1:nc,13)=var;
 namV='RAS.bin';
 var=rdda(namV,[nc ncp],1,'real*8','b'); vvg(1:nc,:,14)=var;
 namV='DXG.bin';
 var=rdda(namV,[nc ncp],1,'real*8','b'); vvg(1:nc,:,15)=var;
 namV='DYG.bin';
 var=rdda(namV,[ncp nc],1,'real*8','b'); vvg(:,1:nc,16)=var;
%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
 namF=sprintf([rDir,inpName(1:4),'%3.3i',inpName(5:end)],n);
 fid=fopen(namF,'w','b');
 fwrite(fid,vvg,'real*8');
 fclose(fid); 
 s=prod(size(vvg));
 fprintf(['write: ',namF,' : size: %i (%ix%ix%i)\n'],s,ncp,ncp,s/ncp/ncp);
end
