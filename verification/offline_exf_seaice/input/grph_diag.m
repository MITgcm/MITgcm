if size(who('kpr'),1) > 0,
 fprintf('kpr is defined and = %i \n',kpr);
else
 fprintf('kpr undefined ; set to 1 \n'); kpr=1 ;
end

Nexp=1; %- plot results from 1 (in rDir1) or from 2 experiments (in rDir1 & rDir2)
kdif=0; %- if Nexp=2, do diff plot (kdif=1) or plot both exp. field (kdif=0)
deltaT=3600;
rDir1='./'; iter=[1:5]*24;
rDir2=rDir1; ite2=iter;
%rDir2='res_n05/'; %ite2=24;
gDir=rDir1;

namF='iceDiag';
%namF='snapshot'; iter=iter-1; ite2=iter;

ii=strfind(rDir1,'/'); if length(ii) > 1, ii=1+ii(end-1); else ii=1; end
titexp1=rDir1(ii:end-1); titexp1=strrep(titexp1,'_','\_');
ii=strfind(rDir2,'/'); if length(ii) > 1, ii=1+ii(end-1); else ii=1; end
titexp2=rDir2(ii:end-1); titexp2=strrep(titexp2,'_','\_');

G=load_grid(gDir,0);
nx=G.dims(1); ny=G.dims(2);
xc=[1:nx]; xc=xc-mean(xc); yc=[1:ny]-.5;

msk1=squeeze(G.hFacC); msk1=ceil(msk1); msk1=min(msk1,1);

Nit=length(iter);
if kpr > 0,
 clear missingValue ;
 [v4d1,its,M]=rdmds([rDir1,namF],iter); %v4d1=squeeze(v4d1);
 eval(M); namV=char(fldList) ; nV=size(namV,1);
 if Nexp > 1, [v4d2,its,M]=rdmds([rDir2,namF],ite2); end
 jA=0; [J]=find(strcmp(fldList,'SI_Fract')); if length(J) == 1 & jA == 0, jA=J; end
       [J]=find(strcmp(fldList,'SIarea  ')); if length(J) == 1 & jA == 0, jA=J; end
 jH=0; [J]=find(strcmp(fldList,'SI_Thick')); if length(J) == 1 & jH == 0, jH=J; end
 jE=0; [J]=find(strcmp(fldList,'SIheff  ')); if length(J) == 1 & jE == 0, jE=J; end
 if size(who('missingValue'),1) > 0,
   fprintf('take missingValue from meta file:');
   if strcmp(dataprec,'float32'), misVal=single(missingValue); else misVal=missingValue; end
 else
   fprintf('no missingValue defined ; set'); misVal=-999.;
 end
 fprintf(' misVal= %f\n',misVal);
end

% add/replace Effective thickness
nVo=nV;
%- to disable addition/replacement:
%jA=-1;
if jE == 0 & jA*jH > 0,
 jE=12;
 if jE > nV, jE=nV+1;
   var=v4d1; v4d1=zeros([dimList jE Nit]); v4d1(:,:,1:nV,:)=var;
   if Nexp > 1, var=v4d2; v4d2=zeros([dimList jE Nit]); v4d2(:,:,1:nV,:)=var; end
   nV=jE;
 end
 namV(jE,:)='SI_Heff ';
 v4d1(:,:,jE,:)=v4d1(:,:,jA,:).*v4d1(:,:,jH,:);
 if Nexp > 1, v4d2(:,:,jE,:)=v4d2(:,:,jA,:).*v4d2(:,:,jH,:); end
end
if jH == 0 & jA*jE > 0,
 jH=12;
 if jH > nV, jH=nV+1;
   var=v4d1; v4d1=zeros([dimList jE Nit]); v4d1(:,:,1:nV,:)=var;
   if Nexp > 1, var=v4d2; v4d2=zeros([dimList jE Nit]); v4d2(:,:,1:nV,:)=var; end
   nV=jH;
 end
 namV(jH,:)='SI_thick';
 var=v4d1(:,:,jA,:); var(find(var==0))=-1; var=1./var; var=max(var,0);
 v4d1(:,:,jH,:)=v4d1(:,:,jE,:).*var;
 if Nexp > 1,
   var=v4d2(:,:,jA,:); var(find(var==0))=-1; var=1./var; var=max(var,0);
   v4d2(:,:,jH,:)=v4d2(:,:,jE,:).*var;
 end
end

% nt    : select which time index to plot in 2-d
% is,js : select which section to plot
nf=0; nt=Nit; kplot=ones(1,nV);
%kplot(11:nV)=0;
is=61; js=41;

xtxt=xc(1); ytxt=yc(1)-5*(yc(2)-yc(1)); clin='kbcmrgy';
xydp=[50 20]; xyp0=[50 20]+nf*xydp; xysp=[500 700]; xydp=[100 40];

for j=1:nV,
  facM=1.; ccB=[0 0];
  if j <= nVo,
    %- convert Fresh-water flux (from kg/m^2/s & m/s) to [mm/d]:
    if strcmp(fldList(j),'SIfrw2oc') | strcmp(fldList(j),'SIfrwAtm'), facM=86400.; end
    if strcmp(fldList(j),'EXFempmr'), facM=1000*86400.; end
  end
  if kplot(j) == 1,
  nf=nf+1;
 %if kplot(nf) == 1,
   xyp0=xyp0+xydp;
   figure(nf); set(nf,'position',[xyp0 xysp]);clf;
    ns=210;
    ns=ns+1; subplot(ns);
    jg=j;
    titv=namV(jg,:); titv=strrep(titv,'_','\_');
    titime=sprintf('t= %4.1f d',iter(nt)*deltaT/86400);
    var=squeeze(v4d1(:,:,jg,nt)); var(find(var==misVal))=NaN;
    var(find(msk1==0))=NaN;
    var=facM*var; mnV=min(var(:)); MxV=max(var(:));
    if MxV > mnV,
      imagesc(xc,yc,var'); set(gca,'YDir','normal');
      ccB=[mnV MxV] + [-1 1]*(MxV-mnV)/10; caxis(ccB); change_colmap(-1);
      if strcmp(namV(jg,:),'SI_thick'), ccB=[-.1 1.1]; end
      if ccB(2) <= ccB(1), ccB=[mnV MxV] + [-1 1]*(MxV-mnV)/10; end
      caxis(ccB); change_colmap(-1);
     %contourf(xc,yc,var',14);
      BB=colorbar('EastOutside');
     %[cs,h]=contour(yc,yc,var',cc);clabel(cs); isoline0(h);
    else fprintf(' %s , uniform = %e\n',namV(jg,:),MxV); end
    title([titexp1,' ; ',titv,' ; ',titime]);
    text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
    if Nexp > 1, ns=ns+1; subplot(ns);
      vv2=squeeze(v4d2(:,:,jg,nt)); vv2(find(vv2==misVal))=NaN;
      var=facM*vv2-kdif*var;
      var(find(msk1==0))=NaN;
      mnV=min(var(:)); MxV=max(var(:));
      if MxV > mnV,
        imagesc(xc,yc,var'); set(gca,'YDir','normal');
%- note: 2nd plot has same color-range (except if diff plot)
        if (ccB(2)-ccB(1))*(kdif-1) >= 0, ccB=[mnV MxV] + [-1 1]*(MxV-mnV)/10; end
        caxis(ccB); change_colmap(-1);
       %contourf(xc,yc,var',12);
        BB=colorbar('EastOutside');
       %[cs,h]=contour(yc,yc,var',cc);clabel(cs); isoline0(h);
      else fprintf(' %s , uniform = %e\n',namV(jg,:),MxV); end
      if kdif == 1, titplot=['Diff: ',titexp2,' - ',titexp1];
      else titplot=[titexp2,' ; ',titv]; end ; title(titplot);
      text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
    else
      subplot(223);
      var=squeeze(v4d1(is,:,jg,:)); var(find(var==misVal))=NaN;
      var=facM*var; mnV=min(var(:)); MxV=max(var(:));
      m1d=squeeze(msk1(is,:)); [J]=find(m1d==0);
      for n=1:Nit,
        jt=1+rem(n-1,length(clin));
        if n > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
        var(J,n)=NaN;
        plot(yc,var(:,n),ccln);
        if n==1, hold on; end
      end
      hold off; AA=axis; axis([0 ny AA(3:4)]);
      grid
     titplot=[titexp1,' ; ',titv,' ; is=',int2str(is)];
     title(titplot);
      subplot(224);
      var=squeeze(v4d1(:,js,jg,:)); var(find(var==misVal))=NaN;
      var=facM*var; mnV=min(var(:)); MxV=max(var(:));
      m1d=squeeze(msk1(:,js)); [J]=find(m1d==0);
      for n=1:Nit,
        jt=1+rem(n-1,length(clin));
        if n > length(clin); ccln=[clin(jt:jt),'--']; else ccln=[clin(jt:jt),'-'];end
        var(J,n)=NaN;
        plot(xc,var(:,n),ccln);
        if n==1, hold on; end
      end
      hold off;
      grid
     titplot=[titexp1,' ; ',titv,' ; js=',int2str(js)];
     title(titplot);
    end
    put_date;
  end
end

return
