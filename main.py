# Script used for the computations of the paper: Machine Learning in
# Contemporary Orthodontics Author: Maxime Rousseau
#
# Pre-treatment analysis of patient in the scope of orthodontics.  In this
# script we will use deep learning face detection, landmark placement and
# multidimensional data-mining to determine the norms previously established in
# orthodontic litterature.
# (Proffit, p.149-155)
#
# We present novel methods of pre-treatment facial analysis via automated
# facial segmentation and classification via autoencoder.
#
# The aim is to compare and challenge traditional methods of facial classification
# (long/short face, asymmetrical face, etc.) to a more contemporary approach
# (segmentation and deep clustering with autoencoders with transfer learning
# from other face datasets). To identify patterns in facial morphology at the
# beginning of treatment.
#
# The aim is to use our modern method to create a distribution of facial
# morphology that will be easier to interpret than having to go through a long
# list of metrics. We want our model to be able to tell us what if the patient
# is in the norm and if not where is he straying from the norm.

import sys
import os
import json
import pytest
import numpy as np
import torch
from facenet_pytorch import MTCNN
import face_alignment
import cv2
import pandas as pd
from linear import Linear as ln

# load images and prepare for processing using opencv
root_path = os.getcwd()
img_path = os.path.join(root_path, 'db/img/')
img_labels = os.listdir(img_path)
img_list = [ os.path.join(img_path, i) for i in img_labels]
landmarks = list(range(69))
landmarks = [str(i) for i in landmarks[1:69]]
dataset = []

# logging
def sys_log(LABEL, STAGE, STATUS):
    label = LABEL 

    if STATUS == 0:
        status = 'SUCCESS'
    else:
        status = 'ERROR'

    if STAGE == 0:
        stage = 'processing: '
    elif STAGE == 1:
        stage = 'storage: '
    else:
        stage = 'unknown: '

    message = '[LOG] ' + stage + label + ' => ' + status
    print(message)

# metric calculations
def compute_metrics(LDMKS):
    fh = ln(LDMKS[27][0], LDMKS[27][1], LDMKS[8][0], LDMKS[8][1])
    fw = ln(LDMKS[0][0], LDMKS[0][1], LDMKS[16][0], LDMKS[16][1])
    lf = ln(LDMKS[33][0], LDMKS[33][1], LDMKS[8][0], LDMKS[8][1])
    mw = ln(LDMKS[4][0], LDMKS[4][1], LDMKS[12][0], LDMKS[12][1])

    facial_index = fh.euc_dist() / fw.euc_dist()
    lf-fh_index = lf.euc_dist() / fh.euc_dist()
    mw-fw_index = mw.euc_dist() / fw.euc_dist()
    mw-fh_index = mw.euc_dist() / fh.euc_dist()

#NOTE TODO
    f_fifths
    asym

    metrics = [facial_index, lf-fh_index, mw-fw_index, mw-fh_index, f_fifths, asym]
    return metrics


# face dection using mtcnn, landmark placement using face_alignment
def image_processing(img_path):
    img = cv2.cvtColor(cv2.imread(img_path), cv2.COLOR_BGR2RGB)
    img = cv2.resize(img, (160,160))
    detector = MTCNN()
    face = detector.detect(img)
    box = face[0][0]
    bounding_box = [(box[0], box[1], box[2], box[3])]
    fa = face_alignment.FaceAlignment(face_alignment.LandmarksType._2D,
                                      flip_input=False, device='cpu',
                                      face_detector='folder')
    ldmks = fa.get_landmarks(img, detected_faces=bounding_box)
    ldmks = ldmks[0]
    print(ldmks[0][0])
    if ldmks.shape == (68,2):
        sys_log(img_path, 0, 0)
    else:
        sys_log(img_path, 0, 1)
    data = [img_path, ldmks]
    return data

# iteration through images
for img in img_list:
    dataset.append(image_processing(img))

# convert to pandas dataframe and save to csv
