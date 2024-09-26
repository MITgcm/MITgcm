% Generate example of OB input files for tidal-component velocity field

% Tidal-component OB input files are real*4 IEEE big-endian binary
% with dimension: OBlength x nTidalComp,
% where OBlength is the length of the open boundary and nTidalComp is the
% number of tidal components to use (i.e., the last non-zero "OBCS_tidalPeriod"
%  from "data.obcs"), not larger than "OBCS_tideCompSize" from "OBCS_PARAMS.h".

% OB[N,S,E,W][am,ph][N,T]File :: Files with boundary conditions,
%                                the letter combinations mean:
%     N/S/E/W   :: northern/southern/eastern/western boundary
%     am/ph     :: tidal amplitude (m/s) / phase (s)
%     N/T       :: for the velocity Normal-component / Tangential-component

% Tidal periods are specified using variable "OBCS_tidalPeriod" in "data.obcs"
% Tidal amplitude is the maximum tidal velocity in m/s.
% Tidal phase indicates time in s of maximum positive tide relative to model Time=0.

%- note: uses writebin.m which can be found in: MITgcm/utils/matlab/cs_grid/read_cs

% create tidal input files
nx=10; ny=8;
nTidalComp=4;
for ob={'N','S','E','W'}
    OBlength=ny;
    if any(strcmp(ob,{'N','S'}))
        OBlength=nx;
    end
    for fld={'am','ph'}
        fnm1=['tidalComp.OB' ob{1} fld{1} 'Nvel.bin'];
        fnm2=['tidalComp.OB' ob{1} fld{1} 'Tvel.bin'];
        var1=zeros(OBlength,nTidalComp); var2=var1;

        % specify (0.03 m/s, 6 hr) for North boundary tidal component 3
        if strcmp(ob,'N')
            if strcmp(fld,'am')
                var1(:,3) = var1(:,3) + 0.03;
            else
                var1(:,3) = var1(:,3) + 6 * 3600;
            end
        end

        % specify ( 0.1 m/s, 2 hr) for South boundary tidal component 1
        if strcmp(ob,'S')
            if strcmp(fld,'am')
                var1(:,1) = var1(:,1) + 0.1;
            else
                var1(:,1) = var1(:,1) + 2 * 3600;
            end
        % also specify Tangential velocity (0.01 m/s, 3 hr)
        % for South boundary tidal component 1
            if strcmp(fld,'am')
                var2(:,1) = var1(:,1) + 0.01;
            else
                var2(:,1) = var2(:,1) + 3 * 3600;
            end
        end

        % specify ( 0.1 m/s, 4 hr) for  East boundary tidal component 2
        if strcmp(ob,'E')
            if strcmp(fld,'am')
                var1(:,2) = var1(:,2) + 0.1;
            else
                var1(:,2) = var1(:,2) + 4 * 3600;
            end
        % also specify Tangential velocity (0.01 m/s, 5 hr)
        % for  East boundary tidal component 2
            if strcmp(fld,'am')
                var2(:,2) = var2(:,2) + 0.01;
            else
                var2(:,2) = var2(:,2) + 5 * 3600;
            end
        end

        % specify (0.02 m/s, 8 hr) for  West boundary tidal component 4
        if strcmp(ob,'W')
            if strcmp(fld,'am')
                var1(:,4) = var1(:,4) + 0.02;
            else
                var1(:,4) = var1(:,4) + 8 * 3600;
            end
        end

        fprintf(' writing bin file: %s ...',fnm1)
        writebin(fnm1,var1)
        fprintf(' done\n')

        if strcmp(ob,'S') || strcmp(ob,'E')
            fprintf(' writing bin file: %s ...',fnm2)
            writebin(fnm2,var2)
            fprintf(' done\n')
        end

    end
end
