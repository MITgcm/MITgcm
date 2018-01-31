rDir1='./'; rDir2=rDir1;
%rDir2='res_c00/';
%rDir2='./';

%kdif=0; %- do diff plot (kdif=1) or plot both exp./var/time (kdif=0)
kdif=0; nf=0; k=0; ccB=[0 0]; ccD=[0 0];

%- iters to load
deltaT=3600; iter=[0:5]*24;
%iter=[0 5]*24;
%iter=iter(2:end); %- iter=0 missing for some fields
Nit=length(iter);

%- select variables to plot
%namv1='ice_fract'; ccB=[-.1 1.1];
namv1='ice_iceH'; ccB=[.1 1.1]/2.1; ccD=[-1 1]*0.06; %ccD=[-1 10]/2;
%namv1='ice_Tsrf'; ccB=[-5 .1];
%namv1='T'; ccB=[-.1 0.8]-1.62;
%namv1='AREA'; ccB=[-.1 1.1];
%namv1='HEFF'; ccB=[.1 1.1]/1.1; ccD=[-1 1]*0.06; %ccD=[-1 10]/2;
%namv1='Qsw'; ccB=[-1.2 0.1]*10;
%namv1='UICE'; nf=1; ccB=[-.1 0.7];
%namv1='VICE'; nf=2; ccB=[-1 1]*.21;
namv2=namv1; cc2=ccB;
%namv1='U'; namv2='V'; nf=1; ccB=[-1 6]*.1; cc2=[-1 1]*.2;
%namv2='ice_iceH';
%--------------------------
%namv='UICE'; nf=3;
%namv='U'; k=1;
%namv='KPPdiffKzS'; k=2;

%- select time index to plot
nt1=Nit;
nt2=nt1;
nt1=2; nt2=Nit;

%- load grid-files:
G=load_grid(rDir1,0);
nx=G.dims(1); ny=G.dims(2);
msk1=ceil(G.hFacC(:,:,1));

%- load fields:
v1=rdmds([rDir1,namv1],iter);
v2=rdmds([rDir2,namv2],iter);

%- kconv=1 : to convert in-situ thickness to effective thickness
%  kconv=-1: or the other way
kconv=0;
if kconv==1,
 namv3='ice_fract';
 v3=rdmds([rDir1,namv3],iter);
 v4=rdmds([rDir2,namv3],iter);
 if strcmp(namv1,'ice_iceH'), v1=v1.*v3; namv1='Ice-Vol'; end
 if strcmp(namv2,'ice_iceH'), v2=v2.*v4; namv2='Ice-Vol'; end
end
if kconv==-1,
 namv3='AREA';
 v3=rdmds([rDir1,namv3],iter);
 v4=rdmds([rDir2,namv3],iter);
 v3(find(v3==0))=-1; v3=1./v3; v3=max(v3,0);
 v4(find(v4==0))=-1; v4=1./v4; v4=max(v4,0);
 if strcmp(namv1,'HEFF'), v1=v1.*v3; namv1='Ice-H'; end
 if strcmp(namv2,'HEFF'), v2=v2.*v4; namv2='Ice-H'; end
end

var=abs(v2-v1); %var(1:5,:)=0;
nDim=length(size(v1)); if Nit > 1, nDim=nDim-1; end
if nDim == 2, ccM=max(max(var)); k=0;
else ccM=max(max(max(var))); end
if max(ccM) > 0, fprintf(' %e',ccM) ; fprintf('\n'); end
%return

titex1=rDir1(1:end-1); titex1=strrep(titex1,'_','\_');
titex2=rDir2(1:end-1); titex2=strrep(titex2,'_','\_');
titva1=namv1; titva1=strrep(titva1,'_','\_');
titva2=namv2; titva2=strrep(titva2,'_','\_');
titim1=sprintf('t= %4.1f d',iter(nt1)*deltaT/86400);
titim2=sprintf('t= %4.1f d',iter(nt2)*deltaT/86400);

xax=[1:nx]-.5 ; yax=[1:ny]-.5;
xtxt=xax(1); ytxt=yax(1)-5*(yax(2)-yax(1));
xydp=[50 20]; xyp0=[50 20]+nf*xydp; xysp=[500 700]; xydp=[100 40];

nf=nf+1; xyp0=xyp0+xydp;
figure(nf); set(nf,'position',[xyp0 xysp]);clf;
subplot(211);
%for nt=1:Nit,
 if k > 0, var=v1(:,:,k,nt1); else var=v1(:,:,nt1); end
 if strcmp(namv1,'U') & strcmp(namv2,'V'),
  var=var.*var; vtmp=var;
    vtmp(1:nx-1,:)=vtmp(1:nx-1,:)+var(2:nx,:); vtmp(nx,:)=vtmp(nx,:)+var(1,:);
  var=v2(:,:,nt2); var=var.*var; vtmp=vtmp+var;
    vtmp(:,1:ny-1)=vtmp(:,1:ny-1)+var(:,2:ny);
  var=sqrt(vtmp*0.5); titva1='|uVel|';
 end
 var(find(msk1==0))=NaN;
 mnV=min(var(:)); MxV=max(var(:));
 imagesc(xax,yax,var'); set(gca,'YDir','normal');
 if ccB(2) > ccB(1), caxis(ccB/1); end
 change_colmap(-1);
 colorbar;
 grid
 text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 titplot=[titex1,' : ',titva1,' ; ',titim1];
title(titplot);

 if k > 0, var=v2(:,:,k,nt2)-kdif*v1(:,:,k,nt1); else var=v2(:,:,nt2)-kdif*v1(:,:,nt1); end
 var(find(msk1==0))=NaN;
 mnV=min(var(:)); MxV=max(var(:));
subplot(212);
 imagesc(xax,yax,var'); set(gca,'YDir','normal');
 if cc2(2) > cc2(1) & kdif == 0, caxis(cc2); end
 if ccD(2) > ccD(1) & kdif == 1, caxis(ccD); end
 change_colmap(-1);
 colorbar;
 grid
 text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 if kdif == 1,
   %titplot=['Diff: ',titex2,' - ',titex1];
   tt1=' -'; tt2='';
   if ~strcmp(rDir1,rDir2), tt1=[tt1,' ',titex1];  tt2=[tt2,' ',titex2]; end
   if ~strcmp(namv1,namv2), tt1=[tt1,' ',titva1];  tt2=[tt2,' ',titva2]; end
   if nt1 ~= nt2, tt1=[tt1,' ',titim1];  tt2=[tt2,' ',titim2]; end
   titplot=['Diff:',tt2,tt1];
 else titplot=[titex2,' : ',titva2,' ; ',titim2]; end
title(titplot);
put_date;
%return

clin='kbcmrgy';
nf=nf+1; xyp0=xyp0+[xysp(1) 0]; xysp=[400 500];
figure(nf); set(nf,'position',[xyp0 xysp]);clf;
 is=61; is2=21;
%is=67; is=38;
subplot(211);
 if k > 0, var=v1(is,:,k,:); else var=v1(is,:,:); end
 var=reshape(var,[ny Nit]);
 m1d=squeeze(msk1(is,:)); [J]=find(m1d==0);
 for nt=1:Nit,
   jt=1+rem(nt-1,length(clin));
   if nt > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
   var(J,nt)=NaN;
   plot(yax,var(:,nt),ccln);
   if nt==1, hold on; end
 end
 hold off;
 AA=axis; axis([0 ny AA(3:4)]);
 grid
titplot=[titex1,' : ',titva1,' ; is=',int2str(is)];
title(titplot);

subplot(212);
 if kdif == 1, is2=is; end
 if k > 0, var=v2(is2,:,k,:)-kdif*v1(is,:,k,:); else var=v2(is2,:,:)-kdif*v1(is,:,:); end
 var=reshape(var,[ny Nit]);
 m2d=squeeze(msk1(is2,:)); [J]=find(m2d==0);
 for nt=1:Nit,
   jt=1+rem(nt-1,length(clin));
   if nt > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
   var(J,nt)=NaN;
   plot(yax,var(:,nt),ccln);
   if nt==1, hold on; end
 end
 hold off;
 AA=axis; axis([0 ny AA(3:4)]);
 grid
 if kdif == 1, titplot=['Diff: ',titex2,' - ',titex1];
 else titplot=[titex2,' : ',titva2,' ; is=',int2str(is2)]; end
title(titplot);
put_date;
%return

clin='kbcmrgy';
nf=nf+1; xyp0=xyp0+[0 xysp(2)];
figure(nf); set(nf,'position',[xyp0 xysp]);clf;
 js=41; js2=6;
subplot(211);
 if k > 0, var=v1(:,js,k,:); else var=v1(:,js,:); end
 var=reshape(var,[nx Nit]);
 m1d=squeeze(msk1(:,js)); [J]=find(m1d==0);
 for nt=1:Nit,
   jt=1+rem(nt-1,length(clin));
   if nt > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
   var(J,nt)=NaN;
   plot(xax,var(:,nt),ccln);
   if nt==1, hold on; end
 end
 hold off;
 grid
titplot=[titex1,' : ',titva1,' ; js=',int2str(js)];
title(titplot);

subplot(212);
 if kdif == 1, js2=js; end
 if k > 0, var=v2(:,js2,k,:)-kdif*v1(:,js,k,:); else var=v2(:,js2,:)-kdif*v1(:,js,:); end
 var=reshape(var,[nx Nit]);
 m2d=squeeze(msk1(:,js2)); [J]=find(m2d==0);
 for nt=1:Nit,
   jt=1+rem(nt-1,length(clin));
   if nt > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
   var(J,nt)=NaN;
   plot(xax,var(:,nt),ccln);
   if nt==1, hold on; end
 end
 hold off;
 grid
 if kdif == 1, titplot=['Diff: ',titex2,' - ',titex1];
 else titplot=[titex2,' : ',titva2,' ; js=',int2str(js2)]; end
title(titplot);
put_date;
return

for nG=1:2,
 if nG==1, var=v1; titplot=[titex1,' : ',titvar,' ; it=',int2str(nit)];
%  else var=v2; titplot=[titex2,' : ',titvar,' ; it=',int2str(nit)]; end
   else var=v2-v1; titplot=['Diff: ',titex2,' - ',titex1]; end
 subplot(210+nG);

 ccM=max(abs(var));
 if ccM > 0, ccE=floor(log10(ccM)); ccM=ceil(ccM*10^-ccE)*10^ccE; else ccM = 1; end
%ccM=0;
%if nG == 2, ccM=ccM/1000; end
 ccB=[-1 1]*ccM;
plot(xax,var','k');

%mnV=min(min(var)); MxV=max(max(var));
%fprintf('min,Max= %9.5g  , %9.5g \n', mnV, MxV)
%text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))

 title(titplot);
end

