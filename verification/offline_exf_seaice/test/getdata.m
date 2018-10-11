
it=720;

rDir='res_o16/'; sfx='_3c0.bin';
u=rdmds([rDir,'U'],it);
v=rdmds([rDir,'V'],it);
et=rdmds([rDir,'Eta'],it);
u1=u(:,:,1);
v1=v(:,:,1);

namf=['uVel',sfx]; var=u1;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
namf=['vVel',sfx]; var=v1;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
namf=['eta',sfx]; var=et;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);

nx=80; ny=42; nr=3;
xax=[1:nx]-.5 ; yax=[1:ny]-.5;
xtxt=1; ytxt=-2;

nf=1; ccB=[0 0];
figure(nf);clf;
subplot(211);
 var=v1;
 var=et;
 imagesc(xax,yax,var'); set(gca,'YDir','normal');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1);
 colorbar;
 grid
titv=['dir: ',strrep(rDir,'_','\_'),' ; sfx=',strrep(sfx,'_','\_')];
title(titv);

rDir='res_o06/'; sfx='_3c1.bin';
u=rdmds([rDir,'U'],it);
v=rdmds([rDir,'V'],it);
et=rdmds([rDir,'Eta'],it);
u1=u(:,:,1);
v1=v(:,:,1);

namf=['uVel',sfx]; var=u1;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
namf=['vVel',sfx]; var=v1;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
namf=['eta',sfx]; var=et;
fprintf('write to file: %s\n',namf);
fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);

subplot(212);
 var=v1;
 var=et;
 imagesc(xax,yax,var'); set(gca,'YDir','normal');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1);
 colorbar;
 grid
titv=['dir: ',strrep(rDir,'_','\_'),' ; sfx=',strrep(sfx,'_','\_')];
title(titv);

