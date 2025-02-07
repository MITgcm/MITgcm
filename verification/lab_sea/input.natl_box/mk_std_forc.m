
nx=20; ny=16; nit=62;

nrc=nx*ny*nit;
%- check size:
!ls -l kf_fu kf_fv
fprintf('expected size to read : %i\n',nrc*4)

%- read wind stress
namF='kf_fu'; fprintf([' read file: ',namF,' ...']);
fid=fopen(namF,'r','b'); tx=fread(fid,nrc,'real*4'); fclose(fid);
% tx=rdda(namF,[nx ny nit],1,'real*4','b');
fprintf(' done\n');

namF='kf_fv'; fprintf([' read file: ',namF,' ...']);
fid=fopen(namF,'r','b'); ty=fread(fid,nrc,'real*4'); fclose(fid);
%ty=rdda(namF,[nx ny nit],1,'real*4','b');
fprintf(' done\n');

%- from "usual" old convention to MITgcm sign convention: flux > 0 is upward
%  reverse sign of tx,ty
tx=-tx;
ty=-ty;

namF='kf_tx'; fprintf([' write tx to file: ',namF,' ...']);
fid=fopen(namF,'w','b'); fwrite(fid,tx,'real*4'); fclose(fid);
fprintf(' done\n');

namF='kf_ty'; fprintf([' write ty to file: ',namF,' ...']);
fid=fopen(namF,'w','b'); fwrite(fid,ty,'real*4'); fclose(fid);
fprintf(' done\n');

