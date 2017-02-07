% construct a 1x1 degree file containing ocean basin information
% 0: Land
% 1: Pacific
% 2: Atlantic
% 3: Indian
% 4: Southern
% 5: Arctic
% 6: Mediterranean
% 7: Sea of Japan
% 8: Other

clear all, clf reset
cd /hosts/triton/dm1/dimitri/matlab/plotfun/ocean_basin

load topo
lon=.5:359.5;
lat=-89.5:89.5;
[X Y]=meshgrid(lon,lat);
basin=topo;
clear t*

% Land
basin(find(basin<=0))=-1;
basin(find(basin>0))=0;

% Southern
ix=find(Y<-35&basin==-1);
basin(ix)=4;
ix=find(X>119&X<137&Y>-35&Y<-32&basin==-1);
basin(ix)=4;

% Arctic
ix=find(Y>66&basin==-1);
basin(ix)=5;
ix=find(X>35&X<40&Y>64&Y<66&basin==-1);
basin(ix)=5;

% Atlantic
ix=find(X>277&X<309&Y>66&Y<79&basin==5);
basin(ix)=2;
ix=find(X>262&basin==-1);
basin(ix)=2;
ix=find(X<25&Y>50&basin==-1);
basin(ix)=2;
ix=find(X<20&Y<10&basin==-1);
basin(ix)=2;

% Pacific
ix=find(X>142&basin==-1);
basin(ix)=1;
ix=find(X==280.5&Y==8.5);
basin(ix)=0;
ix=find(X<290&X>90&Y<9&basin==2);
basin(ix)=1;
ix=find(X<275&X>90&Y<16&basin==2);
basin(ix)=1;
ix=find(X>99&Y>-8&basin==-1);
basin(ix)=1;

% Indian
ix=find(X>22&Y<30&basin==-1);
basin(ix)=3;
ix=find(X==137.5&Y==-28.5);
basin(ix)=8;
ix=find(X>22&X<102&Y<7&basin==1);
basin(ix)=3;
ix=find(X>22&X<107&Y<-6&basin==1);
basin(ix)=3;
ix=find(X>22&X<104&Y<-4&basin==1);
basin(ix)=3;

% Mediterranean
ix=find(X>355&Y<38&Y>35&basin==2);
basin(ix)=6;
ix=find(X<42&Y<47&basin==-1);
basin(ix)=6;
ix=find(X==27.5&Y==29.5);
basin(ix)=8;

% Sea of Japan
ix=find(X<142&X>120&Y>43&Y<52&basin==1);
basin(ix)=7;
ix=find(X<140&X>128&Y>35&Y<52&basin==1);
basin(ix)=7;

% Other
ix=find(basin==-1);
basin(ix)=8;

% Separate Southern Ocean into Pacific
% Atlantic, and Indian Ocean Sectors
ix=find((X<20|X>293|(X>291&Y>-68)|(X==292.5&Y==-79.5))&basin==4);
basin(ix)=4.02;
ix=find(X<146&basin==4);
basin(ix)=4.03;
ix=find(basin==4);
basin(ix)=4.01;

mypcolor(lon,lat,basin);
% $$$ caxis([3.9 4.04])
caxis([6 8])
grid

save basin lon lat basin
