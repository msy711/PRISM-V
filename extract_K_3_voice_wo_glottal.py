import numpy as np
import sys
import os
import glob
import pdb
import librosa
import pandas as pd
import pdb
import pickle
import warnings
warnings.filterwarnings(action='ignore')
##########################################
#foldname = 'data/test_0614'
foldname = 'data'
depress_only = sorted(glob.glob(foldname + '/*.wav'))
print(depress_only)

subject_set = []
for file in depress_only:
    #subject = file.split('.')[0].split('_')[-2]
    subject = file.split(' ')[0].split('_')[-1] + '_' + file.split(' ')[1].split('_')[0]   # "prism_P001 12m_20.wav" format
    subject_set.append(subject)
subject_set = set(subject_set)
##########################################
NUM_PATIENTS = len(subject_set)
##########################################
# subject-file dictionary
subject_list = list(subject_set)
sub_file_dic = {}
for file in depress_only:
    #subject = file.split('.')[0].split('_')[-2]
    subject = file.split(' ')[0].split('_')[-1] + '_' + file.split(' ')[1].split('_')[0]   # "prism_P001 12m_20.wav" format
    if subject not in sub_file_dic.keys():
        sub_file_dic[subject] = []
    sub_file_dic[subject].append(file)

for sub in sub_file_dic.keys():
    print('Subject {}: {} cases'.format(sub, len(sub_file_dic[sub])))
##########################################

#### Spectral - 6 dim
# Spectral features from Librosa library
# No specific hyper-parameters set

def get_spectral(y,sr):
    time  = librosa.get_duration(y=y,sr=sr)
    cent  = librosa.feature.spectral_centroid(y=y,sr=sr)
    cent  = np.log(cent).mean()
    band  = librosa.feature.spectral_bandwidth(y=y,sr=sr)
    band  = np.log(band).mean()
    roll  = librosa.feature.spectral_rolloff(y=y,sr=sr)
    roll  = np.log(roll).mean()
    rmse  = librosa.feature.rms(y=y)
    rmse  = -np.log(rmse).mean()
    onset = librosa.onset.onset_strength(y, sr=sr)
    tempo = float(librosa.beat.tempo(onset, sr=sr))
    return [time,cent,band,roll,rmse,tempo]

#### Formant - 6 dim
# Formant and bandwidth (BW) features (3 dim each)
# Formant for significant frequency in spectrogram, and BW for the distance between freq epochs (shown in envelope)
# Used in phonetics to analyze human utterring

def get_lpc(y,sr):
    lpc = librosa.core.lpc(y,order=16)
    roots_ = np.roots(lpc)
    reals_ = np.real(roots_)
    imags_ = np.imag(roots_)
    roots  = []
    reals  = []
    imags  = []
    for i in range(len(roots_)):
        if imags_[i]>=0:
            roots.append(roots_[i])
            reals.append(reals_[i])
            imags.append(imags_[i])
    angle = np.arctan(np.asarray(imags)/np.asarray(reals))
    freqs = [(sr/(2*np.pi))*z for z in angle]
    bands = [-0.5*(sr/(2*np.pi))*np.log(np.abs(z)) for z in roots]
    formants = []
    bandwids_ = []
    for i in range(len(freqs)):
        if freqs[i] > 90 and bands[i] < 440:
            formants.append(freqs[i])
            bandwids_.append(bands[i])
    index = np.argsort(formants)
    formants.sort()
    formants = np.log(formants)
    bandwids = []
    for i in range(len(bandwids_)):
        bandwids.append(bandwids_[index[i]])
    if len(formants)==0:
        return np.asarray([0,0,0]), np.asarray([0,0,0])
    elif len(formants)==1:
        return np.asarray([0,0,formants[0]]), np.asarray([0,0,bandwids[0]])
    elif len(formants)==2:
        return np.asarray([0,formants[0],formants[1]]), np.asarray([0,bandwids[0],bandwids[1]])
    else:
        return np.asarray(formants[:3]),np.asarray(bandwids[:3])

#### Pitch, Magnitude, Voice portion - 8 dim
def get_pitmag(y,sr,zero_cross):
    pitches, magnitudes = librosa.piptrack(y=y, sr=sr)
    pitches = np.transpose(pitches)
    magnitudes = np.transpose(magnitudes)
    pit_seq = pip_to_pitch(pitches)
    mag_seq = pip_to_pitch(magnitudes)
    delay_p, mag_index, real_z  = find_delay(zero_cross)
    mean_z = np.average(real_z)
    mean_p, error_p, change_p = effective_analysis(pit_seq, mag_index)
    mean_m, error_m, change_m = effective_analysis(mag_seq, mag_index)
    return [mean_p, error_p, change_p, mean_m, error_m, change_m, mean_z, delay_p]

def get_energy(y, sr):
    mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=40)
    ## average based on the case
    mfcc = np.average(np.array(mfcc), axis=1)
    return mfcc

#########################################
# Utility functions for pitch/magnitude feature extraction

def pip_to_pitch(pitches):
    return [np.argmax(t) for t in pitches]

def error_tendency(seq,mean):
    return np.std(seq/mean)

def mean_change(seq,mean):
    if len(seq)>1:
        sum = 0
        for i in range(len(seq)-1):
            sum = sum+np.absolute(seq[i]-seq[i+1])
        sum = np.average(sum)
        sum = sum/mean
        return sum
    else:
        return 0

def find_delay(zero_cross):
    count = 0
    z = np.ones(len(zero_cross))
    for i in range(len(zero_cross)):
        if zero_cross[i] <  np.mean(zero_cross):
            count += 1
            z[i] = 0
    return count/len(zero_cross), z, np.multiply(z,zero_cross)

def effective_analysis(pitches, z):
    eff = []
    for i in range(len(pitches)):
        if z[i]:
            eff.append(pitches[i])
    mean  = np.average(eff)
    error = error_tendency(eff,mean)
    change= mean_change(eff,mean)
    return mean, error, change


# Stack extracted features 
result_depress = []

for sub in sub_file_dic.keys():
    print('Subject {} start!'.format(sub))
    subject_output = []
    subject_mfcc = []
    for file in sub_file_dic[sub]:
        y, sr = librosa.load(file)
        if len(y)>0:
            S = librosa.feature.melspectrogram(y=y,sr=sr,n_mels=128,fmax=8000)
            S = np.transpose(S)
            zero_cross = librosa.feature.zero_crossing_rate(y)[0]
            if len(S)>150:
                spectral = get_spectral(y,sr)
                formants, bandwidth = get_lpc(y,sr)
                piptracks = get_pitmag(y,sr,zero_cross)
                mfcc = get_energy(y, sr)
                subject_output.append(spectral+formants.tolist()+bandwidth.tolist()+piptracks)
                subject_mfcc.append(mfcc.tolist())
    #[sub[-3:]]+ [case]  ## name + case
    subject_output_average = np.average(np.array(subject_output), axis=0)
    subject_mfcc_average = np.average(np.array(subject_mfcc), axis=0)
    
    ## cf. sub.split('_')[0] = subject name, sub.split('_')[1] = case  #cf2) file = 'P001_12m' format
    result_depress.append([sub.split('_')[0]] + [sub.split('_')[1]] + subject_output_average.tolist() + subject_mfcc_average.tolist())
#result_depress = np.array(result_depress)
print('Extract shape: ', np.array(result_depress).shape)
print('# 1. Stacked finished!')

final = np.array(result_depress)
pdb.set_trace()
print('# 6. final data finished!')
with open('after_0621_final.pkl', 'wb') as f:
    pickle.dump(final, f)
print('Type: ', type(final))

mfcc_naming = []
for i in range(40):
    mfcc_naming.append('mfcc_'+str(i))
columns = ['name', 'case_episode', 'time', 'cent', 'band', 'roll', 'rmse', 'tempo', 'formant0', 'formant1', 'formant2', 'bw0', 'bw1', 'bw2', 'mean_p', 'error_p', 'change_p', 'mean_m', 'error_m', 'change_m', 'mean_z', 'delay_p'] + mfcc_naming
print('Column length = ', len(columns))

#df_feature = pd.DataFrame(final, columns = ['name', 'unknown1', 'unknown2', 'case_episode', 'time', 'cent', 'band', 'roll', 'rmse', 'tempo', 'formant0', 'formant1', 'formant2', 'bw0', 'bw1', 'bw2', 'mean_p', 'error_p', 'change_p', 'mean_m', 'error_m', 'change_m', 'mean_z', 'delay_p', 'total_count'])
df_feature = pd.DataFrame(final, columns = columns)
#df_feature.to_excel("data/feature_0525.xlsx")
df_feature.to_excel("feature_0621.xlsx")
print('# 7. df_feature finished!')
print('# 8. All process finished!')