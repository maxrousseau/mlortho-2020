# Script used for the computations of the paper: Machine Learning in
# Contemporary Orthodontics Author: Maxime Rousseau
#
# Pre-treatment analysis of patient in the scope of orthodontics.  In this
# script we will use deep learning face detection, landmark placement and
# multidimensional data-mining to determine the norms previously established in
# orthodontic litterature.
# (Proffit, p.149-155)

import sys
import os
import json
import pytest
import numpy as np
# import tensorflow as tf
from mtcnn import MTCNN
import face_alignment
import cv2
import pandas

# load images and prepare for processing using opencv

root_path = os.getcwd()
img_path = os.path.join(root_path, "db/img/")
img_labels = os.listdir(img_path)
img_list = [ os.path.join(img_path, i) for i in img_labels]



# face dection using mtcnn, landmark placement using face_alignment
def image_processing(img_path):
    img = cv2.cvtColor(cv2.imread(img_path), cv2.COLOR_BGR2RGB)
    detector = MTCNN()
    face = detector.detect_faces(img)
    box = face[0]['box']
    bounding_box = [(box[0], box[1], box[2], box[3])]
    print(bounding_box)
    print(type(bounding_box[0]))

    fa = face_alignment.FaceAlignment(face_alignment.LandmarksType._2D,
                                      flip_input=False, device='cpu', face_detector='folder')
    ldmks = fa.get_landmarks(img, detected_faces=bounding_box)
    print(ldmks)

    return face

print(img_list[1])
image_processing(img_list[1])

