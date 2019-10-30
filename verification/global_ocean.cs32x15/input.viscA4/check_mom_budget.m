%- Uses few MITgcm matlab scripts that can be found in MITgcm/utils/matlab/
%   (rdmds.m & load_grid.m) and in MITgcm/utils/matlab/cs_grid/ (split_C_cub.m)
%  Suggestion: uncomment the 2 folowing lines to access them:
% addpath ../../../utils/matlab/
% addpath ../../../utils/matlab/cs_grid

if size(who('kpr'),1) > 0,
 fprintf('kpr is defined and = %i \n',kpr);
else
 fprintf('kpr undefined ; set to 1 \n'); kpr=1 ;
end

%rDir='../tr_run.thsice/'; nit=36010; deltaT=86400;
 rDir='../tr_run.viscA4/'; nit=86405; deltaT=3600;

%-----
%gDir='grid64/';
 gDir=rDir; %- for grid files

ii=strfind(rDir,'/'); i1=1; i2=length(rDir)-1;
if length(ii) > 1, i1=1+ii(1); i2=ii(2)-1; end
 titexp=[' Glob.Oce ',rDir(i1:i2),' ; ']; titexp=strrep(titexp,'_','\_');
 MnIter=30*86400/deltaT;
 tit0=sprintf(' mn=%2i',nit/MnIter);

 namF='momDiag'; namFs='srfDiag';
%namFs='surfDiag';
tit0=namF;

%--------------------------------------------------
if kpr > 0,
%-- Read in grif-files. Needed with older output to:
%   a) compute surface pressure gradient which was not accounted for in 'Um_dPHdx'
%      but is now included in 'Um_dPhiX'
%   b) compute Non-Hydrostatic pressure gradient (if Non-Hyd) which was not accounted
%      for in 'Um_dPHdx' but is now included in 'Um_dPhiX'
%   c) form vertical viscous flux 'VISrI_Um', to compute tendency from implicit vertical
%      viscosity (with explicit bottom drag) since diagnostic 'Um_ImplD' was not there
 G=load_grid(gDir,0,180,360);
% fieldnames(G)
 nc=G.dims(2); nr=G.dims(3); nPxy=G.dims(1)*G.dims(2); nPp2=nPxy+2;
 ncx=6*nc; np1=nc+1; globArea=sum(G.rAc(:));
%--
 yg2=zeros(nPp2,1); yg2([1:nPxy],1)=reshape(G.yG,[nPxy 1]);
 xg2=zeros(nPp2,1); xg2([1:nPxy],1)=reshape(G.xG,[nPxy 1]);
 rAz2=zeros(nPp2,1); rAz2([1:nPxy],1)=reshape(G.rAz,[nPxy 1]);
%-- add missing corner:
 xg2(nPxy+1)=xg2(1); yg2(nPxy+1)=yg2(1+2*nc); rAz2(nPxy+1)=rAz2(1);
 xg2(nPxy+2)=xg2(1+3*nc); yg2(nPxy+2)=yg2(1); rAz2(nPxy+2)=rAz2(1);

 mskW=min(ceil(G.hFacW),1); mskS=min(ceil(G.hFacS),1);
%-------
end

%-----------------------
%- set constant: gravity, rhoConst
rhoConst=1035.; gravity=9.81;
%--------------------------------------------------

n3d=nr;
if kpr > 0,

%-- Read in 2-D diagnostics file "namFs" and 3-D diagnostics file "namF":
  if isempty(namFs), namS=''; nV2d=0;
  else
   [v3d,iter,M]=rdmds([rDir,namFs],nit);
   eval(M) ; f2dList=fldList; namS=char(f2dList) ; nV2d=size(namS,1);
  end
  [v4d,iter,M]=rdmds([rDir,namF],nit);
  eval(M) ; namV=char(fldList) ; nV=size(namV,1);
  if nV2d == 0, f2dList=fldList; end

 jdps=find(strcmp(f2dList,'PHI_SURF'));
 if isempty(jdps), jdps=0;
  jdps=find(strcmp(f2dList,'ETAN    '));
  if isempty(jdps), jdps=0; else
   eta=v3d(:,:,jdps);
  %b0s=rdmds([gDir,'Bo_surf']); var=b0s.*eta;
   var=gravity*eta;
  end
 else var=v3d(:,:,jdps); end
 if jdps ~= 0,
   v6t=split_C_cub(var);
   var=v6t(2:np1,:,:)-v6t(1:nc,:,:); dpx=var(:,2:np1,:);
   dpx=reshape(permute(dpx,[1 3 2]),[ncx nc]);
   var=v6t(:,2:np1,:)-v6t(:,1:nc,:); dpy=var(2:np1,:,:);
   dpy=reshape(permute(dpy,[1 3 2]),[ncx nc]);
   dpx=-dpx./G.dXc; dpy=-dpy./G.dYc;
 end

 fileName=sprintf('%s.%10.10i.%s',[rDir,'pnhDiag'],nit+1,'data');
 if isfile(fileName),
   fprintf('  -- loading file: %s ...',fileName);
   var=rdmds([rDir,'pnhDiag'],nit+1);
   fprintf(' done\n');
 else
   jnh=find(strcmp(fldList,'PHI_NH  '));
   if isempty(jnh), jnh=0; else var=v4d(:,:,:,jnh); end
 end
 if jnh ~= 0,
   v6t=split_C_cub(var);
   var=v6t(2:np1,:,:,:)-v6t(1:nc,:,:,:); dpNHx=var(:,2:np1,:,:);
   dpNHx=reshape(permute(dpNHx,[1 4 2 3]),[ncx nc nr]);
   var=v6t(:,2:np1,:,:)-v6t(:,1:nc,:,:); dpNHy=var(2:np1,:,:,:);
   dpNHy=reshape(permute(dpNHy,[1 4 2 3]),[ncx nc nr]);
    var=repmat(G.dXc,[1 1 nr]);
   dpNHx=-dpNHx./var; dpNHx=dpNHx.*mskW;
    var=repmat(G.dYc,[1 1 nr]);
   dpNHy=-dpNHy./var; dpNHy=dpNHy.*mskS;
 end

 jeta=find(strcmp(f2dList,'ETAN    '));
 if isempty(jeta), jeta=0; else
  var=v3d(:,:,jeta).*G.rAc;
   v6t=split_C_cub(var);
   var=v6t(2:np1,:,:)+v6t(1:nc,:,:); vbx=0.5*var(:,2:np1,:);
   vbx=reshape(permute(vbx,[1 3 2]),[ncx nc]);
   var=v6t(:,2:np1,:)+v6t(:,1:nc,:); vby=0.5*var(2:np1,:,:);
   vby=reshape(permute(vby,[1 3 2]),[ncx nc]);
   vbx=vbx./G.rAw; vby=vby./G.rAs;
   var=G.depth;
   v6t=split_C_cub(var);
   var=min(v6t(2:np1,:,:),v6t(1:nc,:,:)); hhx=var(:,2:np1,:);
   hhx=reshape(permute(hhx,[1 3 2]),[ncx nc]);
   var=min(v6t(:,2:np1,:),v6t(:,1:nc,:)); hhy=var(2:np1,:,:);
   hhy=reshape(permute(hhy,[1 3 2]),[ncx nc]);
%-- when using z* with  older output, need to account for column vertical streaching
%   in computation of vertical viscosity tendency form vertical viscous flux 'VISrI_Um'
   rFacW=hhx; rFacW(find(hhx==0.))=-1; rFacW=vbx./rFacW;
              rFacW(find(hhx==0.))=0; rFacW=rFacW+ones(ncx,nc);
   rFacS=hhy; rFacS(find(hhy==0.))=-1; rFacS=vby./rFacS;
              rFacS(find(hhy==0.))=0; rFacS=rFacS+ones(ncx,nc);
 end

end

%-----------------------------------------------------------------------------------------
fmtStats='Var= %s : Min,Max,Avr,StD= %12.5e %12.5e %12.5e %12.5e\n';
%fmtSum0='    Sum Tend : Min,Max,Avr,StD= %12.5e %12.5e %12.5e %12.5e\n';
fmtSum1='  Sum Tend: Avr,StD= %12.5e %12.5e ';
fmtSum2='; Residual= %12.5e %12.5e +\n';

 gUdp=zeros(ncx,nc,nr); gVdp=gUdp; titUdp=' ? '; titVdp=' ? ';
 j1=find(strcmp(fldList,'Um_dPhiX'));
 j2=find(strcmp(fldList,'Vm_dPhiY'));
if isempty(j1) & isempty(j2), jdph=0;
  j1=find(strcmp(fldList,'Um_dPHdx'));
  j2=find(strcmp(fldList,'Vm_dPHdy'));
  if ~isempty(j1), jdph=j1; else
     if ~isempty(j2), jdph=j2; end; end
  if jdph ~= 0 & jdps ~= 0,
    gUdp=repmat(dpx,[1 1 nr]).*mskW;
    gVdp=repmat(dpy,[1 1 nr]).*mskS;
  end
  if jnh ~= 0, gUdp=gUdp+dpNHx; gVdp=gVdp+dpNHy; end
else
  if isempty(j1), jdph=j2; else jdph=j1; end
end
if jdph ~= 0,
 if ~isempty(j1), gUdp=gUdp+squeeze(v4d(:,:,:,j1)); titUdp=char(fldList(j1)); end
 if ~isempty(j2), gVdp=gVdp+squeeze(v4d(:,:,:,j2)); titVdp=char(fldList(j2)); end
 if jdps ~= 0,
   titUdp=[titUdp(1:end-1),upper(titUdp(end))];
   titVdp=[titVdp(1:end-1),upper(titVdp(end))];
  %fprintf(' titUdp: >%s< ; titVdp: >%s<\n',titUdp,titVdp);
 end
end

%-- Tendencies from implicit vertical viscous fluxes
% Note: will be used to close momentum budget
%   a) if using older output (since 'Um_ImplD' was not there);
%   b) and using implicit viscosity but without implicit bottom friction
% In the latest case (selectImplicitDrag=2,) cannot close the budget using older output
 j=find(strcmp(fldList,'VISrI_Um'));
if isempty(j), juNz=0; else
 fprintf('  --  Tendencies from vertically visc. fluxes --\n');
  juNz=j; var=squeeze(v4d(:,:,:,j));
 %- compute tendency from minus div of vert. fluxes:
  div=var; div(:,:,1:nr-1)=div(:,:,1:nr-1)-var(:,:,2:nr);
   ddz=G.hFacW.*repmat(reshape(G.dRf,[1 1 nr]),[ncx nc 1]);
   rdz=ddz; rdz(find(ddz==0))=-1;
   rdz=1./rdz; rdz(find(ddz==0))=0;
  div=div.*rdz;
  div=div./repmat(G.rAw,[1 1 nr]);
  div=div./repmat(rFacW,[1 1 nr]);
  gUnuZ=-div;
  titv=char(fldList(j)); var=gUnuZ;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
%--
  j=find(strcmp(fldList,'Um_ImplD'));
 if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 var=var-gUnuZ; titv='Diff:2-1';
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 end
 fprintf('\n');
end

 j=find(strcmp(fldList,'VISrI_Vm'));
if isempty(j), jvNz=0; else
  jvNz=j; var=squeeze(v4d(:,:,:,j));
 %- compute tendency from minus div of vert. fluxes:
  div=var; div(:,:,1:nr-1)=div(:,:,1:nr-1)-var(:,:,2:nr);
   ddz=G.hFacS.*repmat(reshape(G.dRf,[1 1 nr]),[ncx nc 1]);
   rdz=ddz; rdz(find(ddz==0))=-1;
   rdz=1./rdz; rdz(find(ddz==0))=0;
  div=div.*rdz;
  div=div./repmat(G.rAs,[1 1 nr]);
  div=div./repmat(rFacS,[1 1 nr]);
  gVnuZ=-div;
  titv=char(fldList(j)); var=gVnuZ;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
%--
  j=find(strcmp(fldList,'Vm_ImplD'));
 if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 var=var-gVnuZ; titv='Diff:2-1';
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 end
 fprintf('\n');
end
%juNz=0; jvNz=0;

%- Here we check that vertical integral of implicit vertical viscous tendency
%   match either bottom drag (if using implicit bottom drag) or simply zero.
j1=find(strcmp(fldList,'Um_ImplD'));
j2=find(strcmp(f2dList,'botTauX '));
if ~isempty(j1) & ~isempty(j2),
 fprintf('  --  Vertically integrated tendencies --\n');
 j=j2;
  titv=char(f2dList(j)); var=v3d(:,:,j); bTauX=var;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 j=j1;
  titv=char(fldList(j1)); var=squeeze(v4d(:,:,:,j));
 %- vertically integrated:
  ddz=G.hFacW.*repmat(reshape(G.dRf,[1 1 nr]),[ncx nc 1]);
 var=var.*ddz; var=sum(var,3); var=var*rhoConst;
  var=var.*rFacW;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 var=var-bTauX; titv='Diff:2-1';
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 fprintf('\n');
end

j1=find(strcmp(fldList,'Vm_ImplD'));
j2=find(strcmp(f2dList,'botTauY '));
if ~isempty(j1) & ~isempty(j2),
 j=j2;
  titv=char(f2dList(j)); var=v3d(:,:,j); bTauY=var;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 j=j1;
  titv=char(fldList(j1)); var=squeeze(v4d(:,:,:,j));
 %- vertically integrated:
  ddz=G.hFacS.*repmat(reshape(G.dRf,[1 1 nr]),[ncx nc 1]);
 var=var.*ddz; var=sum(var,3); var=var*rhoConst;
  var=var.*rFacS;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 var=var-bTauY; titv='Diff:2-1';
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 fprintf('\n');
end

%fprintf('\n');
%------
fprintf('  --  Check Mom budget, exp: %s, files: %s & %s, it= %i\n',rDir,namF,namFs,nit);

j=find(strcmp(fldList,'TOTUTEND'));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j))/86400;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr; StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 dUtot=var;
end

%- For each term: a) print some stats of this term ;
%    b) add to other tendency and print stats of the sum
%    c) substract the sum from total tendency (-> residual) and print stats

j=find(strcmp(fldList,'Um_dPhiX'));
if isempty(j), j=find(strcmp(fldList,'Um_dPHdx')); end
if ~isempty(j),
  titv=titUdp; var=gUdp;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 gUtot=var;
end

j=find(strcmp(fldList,'Um_Advec'));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gUtot=gUtot+var; var=gUtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dUtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'Um_Ext  '));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gUtot=gUtot+var; var=gUtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dUtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'Um_Diss '));
if ~isempty(j), jDexp=j;
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gUtot=gUtot+var; var=gUtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dUtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end
j=find(strcmp(fldList,'Um_ImplD'));
if ~isempty(j),
  var=squeeze(v4d(:,:,:,j));
elseif juNz ~=0,
  j=juNz; var=gUnuZ;
else j=0; end
if j ~= 0,
  titv=char(fldList(j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gUtot=gUtot+var; var=gUtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dUtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'AB_gU   '));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gUtot=gUtot+var; var=gUtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dUtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

fprintf('\n');

j=find(strcmp(fldList,'TOTVTEND'));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j))/86400;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
 dVtot=var;
end

j=find(strcmp(fldList,'Vm_dPhiY'));
if isempty(j), j=find(strcmp(fldList,'Vm_dPHdy')); end
if ~isempty(j),
 %titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  titv=titVdp; var=gVdp;
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=var;
end

j=find(strcmp(fldList,'Vm_Advec'));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=gVtot+var; var=gVtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dVtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'Vm_Ext  '));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=gVtot+var; var=gVtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dVtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'Vm_Diss '));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=gVtot+var; var=gVtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dVtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end
j=find(strcmp(fldList,'Vm_ImplD'));
if ~isempty(j),
  var=squeeze(v4d(:,:,:,j));
elseif jvNz ~=0,
  j=jvNz; var=gVnuZ;
else j=0; end
if j ~= 0,
  titv=char(fldList(j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=gVtot+var; var=gVtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dVtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

j=find(strcmp(fldList,'AB_gV   '));
if ~isempty(j),
  titv=char(fldList(j)); var=squeeze(v4d(:,:,:,j));
  mnV=min(var(:)); MxV=max(var(:)); Avr=mean(var(:));
  vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtStats,titv,mnV,MxV,Avr,StD);
gVtot=gVtot+var; var=gVtot;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum1,Avr,StD); var=dVtot-var;
  Avr=mean(var(:)); vv2=var.*var; StD=mean(vv2(:))-Avr*Avr;  StD=sqrt(StD);
  fprintf(fmtSum2,Avr,StD);
end

return
