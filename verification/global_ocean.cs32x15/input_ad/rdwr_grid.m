
nc=32; ncp=nc+1;

outpName='grid_cs32';
inpName='tile.mitgrid';

%- load angle Cos & Sin:
angFil=['proj_cs',int2str(nc),'_2uEvN.bin'];
anCs=rdda(angFil,[nc 6 nc],1,'real*8','b');
anSn=rdda(angFil,[nc 6 nc],2,'real*8','b');
anCs=permute(anCs,[1 3 2]);
anSn=permute(anSn,[1 3 2]);

for n=1:6,
%-- read :
 namF=sprintf([inpName(1:4),'%3.3i',inpName(5:end)],n);
 fid=fopen(namF,'r','b');
 var=fread(fid,'real*8');
 fclose(fid);
 s=size(var,1);
 fprintf(['read: ',namF,' : size: %i (%ix%ix%i)\n'],s,ncp,ncp,s/ncp/ncp);
 k=s/ncp/ncp;
 var=reshape(var,[ncp ncp k]);
%-- Add angle:
 vv2=zeros(ncp,ncp,k+2); vv2(:,:,[1:k])=var;
 vv2([1:nc],[1:nc],k+1)=anCs(:,:,n);
 vv2([1:nc],[1:nc],k+2)=anSn(:,:,n);
%-- write:
 namW=sprintf([outpName,'.face%3.3i.bin'],n);
 fid=fopen(namW,'w','b');
 fwrite(fid,vv2,'real*8');
 fclose(fid);
 fprintf([' write to file: ',namW,' %i 2D.var(%ix%i)\n'],k+2,ncp,ncp);
end
 
 
