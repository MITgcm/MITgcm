
%- convert binary input file from native ieee to (ieee) big-endian:
fName='float_pos.input';

imax=9;
[fid,errMsg]=fopen(fName,'r','n');
if ~isempty(errMsg),
 fprintf(['ERROR in "rdsqbin" : try to open file=',fnam,'\n']);
 fprintf(['==> ',errMsg]); fprintf('\n\n');
 return
end

r1=fread(fid,imax,'real*8');
np=r1(1);

fprintf('read %i reccords ...',np);
rA=fread(fid,imax*np,'real*8');
fprintf(' OK\n');
fclose(fid);
rA=reshape(rA,[imax np]);

var=zeros(imax,np+1); 
var(:,1)=r1;
var(:,2:np+1)=rA;

namfil='float_pos.inp_b';
fid=fopen(namfil,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);


