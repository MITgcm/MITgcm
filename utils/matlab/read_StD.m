function [nIt,rList,tim,vvA,listV,listK]=read_StD(namF,sufx,listV);
% [nIt,rList,tim,vvA,listV,listK]=read_StD(namF,sufx,listV);
%
% read ASCII stat-Diags output files (after splitted by script extract_StD)
%
% input: listV = list of fileds to read in ; if ='all_flds' => read all fields
%        namF  = prefix of all file names to read (after extract_StD)
%        sufx  = suffix of all file names to read (after extract_StD)
% output:
%  nIt      = number of time reccords
%  rList    = list of region number
%  tim(:,1) = iterations number ; tim(:,2) = time in simulation
%  listV    = list of fields
%  listK    = list of level numbers
%  vvA      = 5 dims output array:
%           ( kLev, time_rec, region_rec, [ave,std,min,max,vol], var_rec )

% $Header: /u/gcmpack/MITgcm_contrib/jmc_script/read_StD.m,v 1.6 2019/04/01 22:04:43 jmc Exp $
% $Name:  $

%- Remove insignificant whitespace:
%sufx=strtrim(char(sufx)); % <-- only with matlab-7 or more recent
sufx=strrep(char(sufx),' ','');
namfhd=[namF,'_head','.',sufx];
namfil=[namF,'_Iter','.',sufx];
 fprintf(['read ',sufx,' :']);
rf=-1; if strcmp(char(listV),'all_flds'), rf=0; end

%- read from Header file:
%   frequency ; list of regions ; list of levels ; (+ list of Var, if rf=0)
fid=fopen(namfhd,'r');
D=dir(namfhd);
if size(D,1) == 1,
 fprintf(' head');
else error(['file: ',namfhd,' not found']); return; end
flag=1; l=0; k=3;
while flag > 0
 tline=fgetl(fid);
 if ischar(tline), l=l+1;
   %disp(tline);
    if tline(2:12) == ' frequency ',
     frq=sscanf(tline(17:end),'%f');
     k=k-1;
    end
    if tline(2:10) == ' Regions ',
     rList=sscanf(tline(17:end),'%i');
     k=k-1;
    end
    if tline(2:15) == ' Nb of levels ',
     kList=sscanf(tline(17:end),'%i');
     k=k-1;
    end
    if rf >=0 & tline(2:10) == ' Fields  ',
      tmp=[tline(17:end),' ']; i1=0;
      for i=1:length(tmp),
        if isspace(tmp(i)),
          if i1 > 0 & i > i1,
            if rf == 0, listV=cellstr(tmp(i1:i-1));
            else listV(rf+1)=cellstr(tmp(i1:i-1)); end
            i1=i+1; rf=rf+1;
          else i1=i+1;
          end
        end
      end
    end
    if tline(2:15) == ' end of header',
     flag=0;
    end
 else flag=-1; end
end
fclose(fid);
if rf > 0,
%- rename fields (consistent with script "extract_StD"):
   for j=1:rf,
     var1=char(listV(j));
     switch var1
     case 'ETAN'    ,  var2='Eta';
     case 'ETANSQ'  ,  var2='Et2';
     case 'THETA'   ,  var2='T';
     case 'SALT'    ,  var2='S';
     case 'UVEL'    ,  var2='U';
     case 'VVEL'    ,  var2='V';
     case 'WVEL'    ,  var2='W';
     case 'PHIHYD'  ,  var2='Phi';
     case 'UVELSQ'  ,  var2='U2';
     case 'VVELSQ'  ,  var2='V2';
     case 'WVELSQ'  ,  var2='W2';
     case 'THETASQ' ,  var2='T2';
     otherwise     var2=var1;
     end
     listV(j)=cellstr(var2);
     if strcmp(var1,var2), fprintf(' %s\n',var2);
          else fprintf(' %s --> %s\n',var1,var2); end
   end
%  listV
end
if flag ~= 0 | k > 0, frq,rList,kList, end
if flag ~= 0, error(['not normal end after reading ',int2str(l),' lines']); end
if k > 0, error(['missing ',int2str(k),' flds (freq,regions,levels)']); end
if flag ~= 0 | k > 0, frq=0; rList=0; kList=0; return; end
nReg=size(rList,1);
nbV=size(listV,2);
%fprintf('\n kList=');fprintf(' %i',kList);fprintf('\n');
%if rf > 0, for l=1:nbV, fprintf([' ',char(listV(l))]); end; fprintf('\n'); end
if ( rf > 0 & nbV < length(kList) ) | nbV > length(kList),
 fprintf('\n Warning: nbV= %i but (only) %i in kList\n',nbV,length(kList))
end
%- set Max Nb of levels: n3d
n3d=max(kList); if n3d > 1, n3d=1+n3d ; end

%- load Iter:
D=dir(namfil);
if size(D,1) == 1,
 fprintf(' & Iter'); var=load(namfil);
else fprintf(['file: ',namfil,' not found => EXIT \n']); return; end
nIt=size(var,1);

%- initialize output & save Iter:
tim=zeros(nIt,2); vvA=zeros(n3d,nIt,nReg,5,nbV);
msgA=' '; msgB=' ';
tim(:,1)=var;

dIt=ones(1,nIt);
if nIt > 1, dIt(2:nIt)=tim(2:nIt,1)-tim(1:nIt-1,1); dIt(1)=dIt(2); end
% fprintf(' (dIt: %i %i)',min(dIt(1:nIt)),max(dIt(1:nIt)));
%- take the inverse : will be used to correct the volume
%            (frq<0 : no volume correction if snap-shot)
if frq > 0, dIt=1./max(1,dIt); else dIt=ones(1,nIt); end

%- build time:
delT=0;
if nIt > 1, delT=(tim(nIt,1)-tim(1,1))/(nIt-1); delT=max(0,delT); end
if delT > 0,
  delT=abs(frq)/delT;
  delta=delT-round(delT);
  delta=abs(delta*(tim(2,1)-tim(1,1)));
  if delta <= 0.5, delT=round(delT); end
end
fprintf(': nReg= %i ; delta.T= %6.1f ; freq= %8.0f',nReg,delT,frq);
if delT > 0, fprintf(' (= %8.3f it)',frq/delT); end
fprintf('\n')

if delT > 0,
 tim(:,2)=delT*tim(:,1);
else
 tim(:,2)=tim(:,1);
end
 fprintf(' + var:'); var=load(namfil);

%---------
for nv=1:nbV,
  namV=char(listV(nv));
 [vv1,nk,msg]=read_StD_1v(namF,namV,sufx,nReg,nIt,dIt);
  if nk > 0, fprintf('_%i',nk); elseif nk < 0, msgA=sprintf([msgA,msg]);
  else msgB=sprintf([msgB,msg]) ; end
  if nk > n3d,
    error([' too many levels(=',int2str(nk),' > ',int2str(n3d), ...
           ' reading field: ',namV]);
  end
  if nk > 0, vvA(1:nk,:,:,:,nv)=vv1; end
  if nv == 1, listK=nk; else listK=[listK nk]; end
end
fprintf(' <= end \n');
if length(msgA) ~= 1, fprintf(['  not found:',msgA,'\n']) ; end
if length(msgB) ~= 1, fprintf(['  dim Pb:',msgB,'\n']) ; end
if n3d == 0, n3d=1; vvA=zeros(n3d,nIt,nReg,5,nbV); else
  s3d=max(listK); if n3d > s3d, vvA=vvA(1:s3d,:,:,:,:); end
end

return

function [vvm,nk,msg] = read_StD_1v(namF,nmV,sufx,nReg,nit,dit)
 msg=' '; nk=0; fprintf([' ',nmV]);
 namfil=[namF,'_',nmV,'.',sufx]; D=dir(namfil);
 if size(D,1) == 1,
   var=load(namfil);
   nk=1+max(var(:,1));
   if size(var,1) == nk*nReg*nit,
     vv1=reshape(var(:,2:6),[nk*nReg nit 5]);
%--  put back Integrated Volume (was cumulated over the time period) in 5:
     vv1(:,:,5)=vv1(:,:,5).*(ones(nk*nReg,1)*dit);
     vv1=reshape(vv1,[nk nReg nit 5]);
     vvm=permute(vv1,[1 3 2 4]);
   else
    %fprintf(['\n ERROR in Dim, var=',nmV,' :']); fprintf(' %i',size(var));
    %fprintf(' ; nk,nReg,nit= %i %i %i\n',nk,nReg,nit);
     msg=sprintf([' in ',nmV,' : %i <> %ix%ix%i'],size(var,1),nk,nReg,nit); nk=0;
     vvm=zeros(1,nit,nReg,5);
     return
   end
 else msg=sprintf([msg,namfil,' ']); nk=-1; vvm=zeros(1,nit,nReg,5);
 end
return
