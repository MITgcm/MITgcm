% Generate input files for MITgcm/verification/so_box_biogeo/
%  ( Southern-Ocean box with Biochemistry, using OBCs )
% by a) extracting bathy and forcing from ../run_glob/ where files have been
%       linked from verification/tutorial_global_oce_biogeo/input/
%    b) extracting initial conditions and OBCs from a 2-years run
%       (done here in ../run_glob/) using model parameters from ../inp_global.

%- kwr(1) = 2 : write Region mask to get matching Stat-Diags in Global Ocean run
%- kwr(2) = 2 : write forcing files for S.Ocean box
%- kwr(3) = 2 : write initial State for S.Ocean box
%- kwr(4) = 2 : write OBCS files for S.Ocean box
kwr=[1 2 2 2]; prec='real*4';

rDir='../run_glob/'; %- output dir of global ocean 2 yrs run
iDir='../run_glob/'; %- input  dir of global ocean 2 yrs run

G=load_grid(rDir);
nx=G.dims(1); ny=G.dims(2); nr=G.dims(3);
xax=G.xAxC; yax=G.yAxC;
xaf=G.xAxU; yaf=G.yAxV;

it0=5184000;
itMon=60; %- nb of iter in 1 month

nbx=42; nby=20;
i1=85; i2=i1+nbx; %- box egdes indices
j1=4;  j2=j1+nby; %- box egdes indices
xl=[xaf(i1) xaf(i1) xaf(i2) xaf(i2) xaf(i1)];
yl=[yaf(j1) yaf(j2) yaf(j2) yaf(j1) yaf(j1)];
ib1=i1; ib2=i2-1; jb1=j1; jb2=j2-1; %- box limit for grid-pts center

%-- OB location: tracer pts:
iobW=ib1; iobE=ib2; jobN=jb2;
%-- OB location: velocity, normal component:
iuW=iobW+1 ; iuE=iobE ; jvN=jobN ;
xob=[xaf(iuW) xaf(iuW) xaf(iuE) xaf(iuE)];
yob=[yaf(j1)  yaf(jvN) yaf(jvN) yaf(j1) ];

h=G.depth;

%- Create a Region mask (corresponding to S.Ocean Box interior)
%    to get matching Stat-Diags in Global Ocean run
if kwr(1) > 0,
% regional mask: 3 lat. band (=1,2,3) (y-boundary=-/+ 23) + S.Ocean Box (=0):
  maskDiag=ones(nx,ny);
  maskDiag(iobW+1:iobE-1,jb1:jobN-1)=0.;
  maskDiag(find(G.yC > -23.))=2.;
  maskDiag(find(G.yC > +23.))=3.;
  namfb='regMask_SOceanBox.bin';
  if kwr(1) > 1,
   vv2=maskDiag;
   fprintf(' write file: %-25s (%i %i)',namfb,size(vv2));
   fid=fopen(namfb,'w','b'); fwrite(fid,var,prec); fclose(fid);
   fprintf('\n');
  end
end %- if kwr(1) > 0

figure(1);clf;
var=-h; var(find(var==0))=NaN; ccB=[-54 1]*100;
if kwr(1) > 0,
  var=maskDiag; var(find(h==0))=NaN; ccB=[-1 4];
end
imagesc(xax,yax,var');set(gca,'YDir','normal');
caxis(ccB);
change_colmap(-1);
colorbar
Lbx=line(xl,yl); set(Lbx,'Color',[0 0 0],'LineWidth',2);
Lob=line(xob,yob); set(Lob,'Color',[1 0 0],'LineWidth',2);
grid;

%---------
%- extract box part from 2-D & 3-D fields
% a) forcing (2-D bin file, sng or 12 Rec):
namInpF={'bathy','lev_monthly_salt','lev_monthly_temp', ...
         'shi_empmr_year','shi_qnet','tren_taux','tren_tauy', ...
         'tren_speed','fice','sillev1','silicate_3D_12m'};
if kwr(2) > 0
 fprintf('==== Processing Forcing Files ====\n');

 for n=1:length(namInpF),
  namfg=[char(namInpF(n)),'.bin'];
  namfb=[char(namInpF(n)),'_box.bin'];
  if isempty(dir([iDir,namfg])),
   fprintf(' => skip file: %-21s (not found)\n',namfg);
  else
   fprintf(' process file: %-21s',namfg);
   nRec=12;
   if strcmp(char(namInpF(n)),'bathy'), nRec=1; end
   if strcmp(char(namInpF(n)),'silicate_3D_12m'), nRec=nr*nRec; end
   vv1=rdda([iDir,namfg],[nx ny nRec],1,prec,'b');
   vv2=vv1(ib1:ib2,jb1:jb2,:);
  %size(vv1),size(vv2)
   if kwr(2) > 1,
    fprintf(' --> %-25s (',namfb); fprintf(' %i',size(vv2)); fprintf(' )');
    fid=fopen(namfb,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
   end
   fprintf('\n');
  end
 end
end %- if kwr(2) > 0

% b) initial state
% take snap-shot after 1 yr run:
it1=it0+12*itMon;
namState={'Eta','T','S','U','V'};
if kwr(3) > 0
 fprintf('==== Processing Initial State ====\n');

 for n=1:length(namState)+5,
  if n <= length(namState),
   namfg=[char(namState(n))];
   namfb=[char(namState(n)),'_ini.bin'];
  else
   itr= n-length(namState);
   namfg=sprintf('%s%2.2i','PTRACER',itr);
   namfb=sprintf('%s%2.2i%s','Trac',itr,'_ini.bin');
  end
  fprintf(' process file: %-21s',namfg);
  vv1=rdmds([rDir,namfg],it1);
  if length(size(vv1)) == 2,
   vv2=vv1(ib1:ib2,jb1:jb2);
  else
   vv2=vv1(ib1:ib2,jb1:jb2,:);
  end
  if kwr(3) > 1,
   fprintf(' --> %-25s',namfb);
   fid=fopen(namfb,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
  end
  fprintf('\n');
 end
end %- if kwr(3) > 0

%---------
%- extract OB fields from 3-D output fields
%  take 1rst yr Dec aver as 1rst Rec ; then continue on to 2nd yr: Jan -> Nov
iters=it0+([0:11]+12)*itMon;

if kwr(4) > 0,
 fprintf('==== Processing OB Fields ====\n');
 [vv4,itrs,M]=rdmds([rDir,'dynDiag'],iters);
 eval(M);

 Nit=length(iters);
 rhFacW=G.hFacW; rhFacS=G.hFacS;
 rhFacW(find(rhFacW==0))=-1; rhFacW=1./rhFacW; rhFacW=max(rhFacW,0.);
 rhFacS(find(rhFacS==0))=-1; rhFacS=1./rhFacS; rhFacS=max(rhFacS,0.);
 rhFacW=reshape(rhFacW,[nx*ny*nr 1]);
 rhFacS=reshape(rhFacS,[nx*ny*nr 1]);

 for n=2:length(namState)+5,
  if n > length(namState),
   itr= n-length(namState);
   namVs=sprintf('%s%2.2i','Trac',itr);
   namV=sprintf('%s%2.2i%s','TRAC',itr,'  ');
  else
   namVs =[char(namState(n))];
   if strcmp(namVs,'U') || strcmp(namVs,'V'),
     namV=[namVs,'VELMASS'];
   elseif strcmp(namVs,'T'),
     namV=[namVs,'HETA   '];
   elseif strcmp(namVs,'S'),
     namV=[namVs,'ALT    '];
    %namV='WVELMASS';
   else
     fprintf(' unknow State Var: %s\n',namVs);
   end
  end
  jv=strmatch(namV,fldList);
  if isempty(jv),
   fprintf('not found in fldList: "%s" for Var: "%s"',namV,namVs);
  else
   fprintf(' process OB for: %-6s from %-8s (jv=%2i)',namVs,char(fldList(jv)),jv);
   vv1=squeeze(vv4(:,:,:,jv,:));
   if strcmp(namVs,'U'),
     vv1=reshape(vv1,[nx*ny*nr Nit]);
     vv1=vv1.*(rhFacW*ones(1,Nit));
     vv1=reshape(vv1,[nx ny nr Nit]);
%-- extract OB values:
     vvW=vv1(iuW,jb1:jb2,:,:);
     vvE=vv1(iuE,jb1:jb2,:,:);
     vvN=vv1(ib1:ib2,jobN,:,:);
   elseif strcmp(namVs,'V'),
     vv1=reshape(vv1,[nx*ny*nr Nit]);
     vv1=vv1.*(rhFacS*ones(1,Nit));
     vv1=reshape(vv1,[nx ny nr Nit]);
%-- extract OB values:
     vvW=vv1(iobW,jb1:jb2,:,:);
     vvE=vv1(iobE,jb1:jb2,:,:);
     vvN=vv1(ib1:ib2,jvN,:,:);
   else
   vv2=vv1(ib1:ib2,jb1:jb2,:);
%-- extract Western OB values:
     vvW=vv1(iobW,jb1:jb2,:,:);
%-- extract Eastern OB values:
     vvE=vv1(iobE,jb1:jb2,:,:);
%-- extract Nortern OB values:
     vvN=vv1(ib1:ib2,jobN,:,:);
   end
%--
  end
  if kwr(4) > 1,
   namfb=[namVs,'_obW.bin'];
   fprintf(' --> %s',namfb);
   fid=fopen(namfb,'w','b'); fwrite(fid,vvW,prec); fclose(fid);
   namfb=[namVs,'_obE.bin'];
   fprintf(' , %s',namfb);
   fid=fopen(namfb,'w','b'); fwrite(fid,vvE,prec); fclose(fid);
   namfb=[namVs,'_obN.bin'];
   fprintf(' , %s',namfb);
   fid=fopen(namfb,'w','b'); fwrite(fid,vvN,prec); fclose(fid);
  end
  fprintf('\n');
 end
end %- if kwr(4) > 0

