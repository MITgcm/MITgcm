
%- write a 3 regions mask (based on latitude) for DiagStat output:
%   region 1 = South   ,     yLat <= -24 ;
%   region 2 = Tropics , -24 < yLat < 24 ;
%   region 2 = North   ,  24 <= yLat ;

yc=rdmds('YC');
regMsk=ones(size(yc));

regMsk(find( -24 < yc))=2;
regMsk(find( 24 <= yc ))=3;

namfil='regMask_lat24.bin';
fid=fopen(namfil,'w','b'); fwrite(fid,regMsk,'real*8'); fclose(fid);
fprintf([' write mask to file: ',namfil,'\n']);

return

%- a plot to check:
%rac='ref/';
%load_cs;
%ccB=[0 4]; shift=0; cbV=0; AxBx=[-180 180 -90 90]; kEnv=0;
%var=regMsk;
%figure(1);clf;
%grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);

