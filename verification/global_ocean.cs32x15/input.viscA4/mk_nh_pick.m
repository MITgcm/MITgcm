
nit=86400;
namInp='pickup';
namInp=[namInp,'_nh'];

wrfile=sprintf([namInp,'.%10.10i'],nit);

nc=32;nr=15;
va=zeros(nc*6,nc,nr,2);

fid=fopen(wrfile,'w','b');
fwrite(fid,va,'real*8');
fclose(fid);

fprintf([' ==> write file: ',wrfile,'\n']);
