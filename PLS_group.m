%% Set up - Standards comparison between distract and focus

clear all

excl = {'bci15_06',1,2;     % Missing
        'bci15_04',2,1;     % Missing
        'bci15_03',2,2;
        'bci15_05',2,1;
        'bci15_11',2,2;
        'bci15_21',2,1;
        'bci15_24',1,2;
        'bci15_25',2,2};

% Set up some parameters
dataDir = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data/';
subs_robot = {'bci15_01','bci15_03','bci15_05','bci15_07','bci15_11','bci15_19','bci15_21','bci15_25'};
subs_graph = {'bci15_02','bci15_04','bci15_06','bci15_08','bci15_12','bci15_14','bci15_24','bci15_30'};
groups = {'robot','graphs'};
conditions = {'s1_l', 's1_r', 's2_l', 's2_r'};
conditionLabels = {'S1 L', 'S1 R', 'S2 L', 'S2 R'};

% Read a template dataset to get some info
templateDataset = '/Volumes/HDD/Google Drive/Sean/Projects/Comp-Robot/MEG/data/bci15_01/session1/imagery1_tsss_LP70Hz_250Hz_reref_cleaned-epo_15-30Hz_ERS_left.nii';
nii = load_nii(templateDataset);

% Set the size of all dimensions in the PLS
num_vertices = length(reshape(nii.img,[],1));
num_intervals = 1; % size(nii.img,4);
num_subjects = length(subs_robot);
num_conditions = length(conditions);

DSPM_new = permute(nii.img,[1,2,3]);
DSPM_flat = reshape(DSPM_new, num_intervals, []);
unmask = find(DSPM_flat(1,:)~=0);
num_brain_vertices = length(unmask);

% Make an empty 2-D matrix for the combined data (subjects x conditions x
% vertices x time)
PLSdim1 = num_subjects*num_conditions;
PLSdim2 = num_brain_vertices*num_intervals;
plsDat{1} = zeros(PLSdim1,PLSdim2);
plsDat{2} = zeros(PLSdim1,PLSdim2);

%% Populate the PLS matrix

for groupno = 1:2
    for c=1:num_conditions,
        for s=1:num_subjects,
            
            if groupno == 1
                sub = subs_robot{s};
            else
                sub = subs_graph{s};
            end
            
            % Get labels

            hands = {'left','right','left','right'};
            ses_num = ceil(c/2);
            ses_str = int2str(ses_num);
            hand_str = char(hands(c));
            
            % Determine exclusions
    
            exclmask = strcmp(sub,excl);
            row = find(exclmask(:,1));
            badses = 0; if ~isempty(row), badses = excl{row,2}; end
            badblock = 0; if ~isempty(row), badblock = excl{row,3}; end

            skip1 = 0; if ses_num == badses && badblock == 1; skip1 = 1; end
            skip2 = 0; if ses_num == badses && badblock == 2; skip2 = 1; end
            
            % Figure out where the data will go in the 2-D PLS matrix
            PLSdim1_index = s+num_subjects*(c-1);

            % Read in the data for the current subject and condition
            ds1 = [dataDir sub '/session' ses_str '/imagery1_tsss_LP70Hz_250Hz_reref_cleaned-epo_15-30Hz_ERS_' hand_str '.nii'];
            ds2 = [dataDir sub '/session' ses_str '/imagery2_tsss_LP70Hz_250Hz_reref_cleaned-epo_15-30Hz_ERS_' hand_str '.nii'];
            
            fprintf(1, 'Reading in %s\n', ds1);
            
            if skip1
                thisNii = load_nii(ds2);
            elseif skip2
                thisNii = load_nii(ds1);
            else
                dat1 = load_nii(ds1);
                thisNii = load_nii(ds2);
                thisNii.img = (dat1.img + thisNii.img) / 2;
            end
            
            DSPM = thisNii.img;

            % Flatten the data to be vertices by time
            DSPM_new = permute(DSPM,[1,2,3]);
            DSPM_flat = reshape(DSPM_new, 1, []);
            DSPM = permute(DSPM_flat, [2 1]);

            for t=1%:num_intervals,
                % Figure out where the data goes
                thisLatencyDSPM = DSPM(unmask,t);
                PLSdim2_index = 1+num_brain_vertices*(t-1);

                plsDat{groupno}(PLSdim1_index,PLSdim2_index:PLSdim2_index+num_brain_vertices-1) = thisLatencyDSPM;
            end
        end
    end
end

%% PLS analysis

% Set all zeros in the data to be not-a-number
%plsDat(plsDat == 0) = nan;

datMat{1} = plsDat{1};

pls_options.method = 1;
pls_options.num_perm = 512;
pls_options.num_boot = 512;
%pls_options.clim = 99;
result_PLS = pls_analysis(datMat, num_subjects, num_conditions, pls_options);   

figure, 
subplot(2,1,1), bar(result_PLS.v');
title('Latent Variables');
legend(conditionLabels);
subplot(2,1,2), bar(result_PLS.perm_result.sprob);
title('Probabilities for Latent Variables')
set(gcf, 'Color', [1 1 1])

% 473 1876

save /Users/tbardouille/Documents/Work/Data/pattern_recognition_project/nii_data/distract_focus_PLS.mat
