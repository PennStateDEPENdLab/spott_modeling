function [data, y, u] = suuvid_get_data(data_file, id, vo)
data = readtable(data_file,'Delimiter',',','ReadVariableNames',true, 'TreatAsEmpty',{'.','NA'});

n_t = size(data,1); %number of rows
n_actions = max(data{:, 'key'});
observations_to_fit = 1:n_t; %fit all observations by default

y = zeros(n_actions+1, n_t); %last element is no response
for i = 1:n_t
    this_key = data{i, 'key'};
    if this_key > 0
        y(this_key, i) = 1; %populate element of y vector for chosen action
    else
        y(end, i) = 1; %populate no response
    end 
    
end

%right shift reinforcement
new_trial = [1; diff(data{observations_to_fit, 'instrial'})];
choice = data{observations_to_fit, 'key'};
reinforcement = data{observations_to_fit, 'nreward'};

u = [ new_trial, ...
    [ 0; choice(1:end-1) ], ...
    [ 0; reinforcement(1:end-1) ], ...
    data{observations_to_fit, 'tdiff'}, ...
    data{observations_to_fit, 'curkey'}, ...
    ]';

end
