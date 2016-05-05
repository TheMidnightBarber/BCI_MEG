% Note: If a full subject needs to be eliminated, make sure they're in
% another folder

clear all
wdir = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data';
cd(wdir);

group = 3;      % 1: Robot, 2: Graphs, 3: All
session = 1;
side = 'left';
band = [15 30];

% Exclusions: sub, ses, block

excl = {'bci15_06',1,2;     % Missing
        'bci15_04',2,1;     % Missing
        'bci15_03',2,2;
        'bci15_05',2,1;
        'bci15_11',2,2;
        'bci15_21',2,1;
        'bci15_24',1,2;
        'bci15_25',2,2};
    
% Output parameters

savenii = 1;
savemat = 1;

% Main loop

bandstr = [int2str(band(1)) '-' int2str(band(2)) 'Hz'];
subs = dir('./bci15_*');
groups = {'Robot','Graphs','Group'};
grouparr = [];

for subno = 1:length(subs)
    
    sub = subs(subno).name;
    fprintf(1,'%s\n',sub);
    
    % Which group are they in?
    
    if mod(str2num(sub(end-1:end)),2), grouparr = [grouparr 1];
    else grouparr = [grouparr 2];
    end
    
    % Determine exclusions
    
    exclmask = strcmp(sub,excl);
    row = find(exclmask(:,1));
    badses = 0; if ~isempty(row), badses = excl{row,2}; end
    badblock = 0; if ~isempty(row), badblock = excl{row,3}; end
    
    skip1 = 0; if session == badses && badblock == 1; skip1 = 1; end
    skip2 = 0; if session == badses && badblock == 2; skip2 = 1; end
    
    % Load data
    
    ds1 = fullfile(sub,['session' int2str(session)],['imagery1_tsss_LP70Hz_250Hz_reref_cleaned-epo_' bandstr '_ERS_' side '.nii']);
    ds2 = fullfile(sub,['session' int2str(session)],['imagery2_tsss_LP70Hz_250Hz_reref_cleaned-epo_' bandstr '_ERS_' side '.nii']);

    if skip1
        dat2 = load_nii(ds2);
        dat(:,:,:,subno) = dat2.img;
    elseif skip2
        dat1 = load_nii(ds1);
        dat(:,:,:,subno) = dat1.img;
    else
        dat1 = load_nii(ds1);
        dat2 = load_nii(ds2);
        dat(:,:,:,subno) = (dat1.img + dat2.img)/2;
    end
        
    
end

% Parse for group
    
if group < 3
    finalsubs = find(grouparr == group);
    dat = dat(:,:,:,finalsubs);
end
    
% Save data
    
outstr = [char(groups(group)) 'Avg_session' int2str(session) '_' side '_' bandstr];
output = dat1;
output.img = mean(dat,4);
if savenii, save_nii(output,['collated_5mm/' outstr '.nii']); end
if savemat, save(['collated_5mm/' outstr '.mat'],'dat'); end