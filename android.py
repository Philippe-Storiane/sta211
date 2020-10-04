# -*- coding: utf-8 -*-
"""
Created on Mon Aug 31 05:23:22 2020

@author: a179415
"""

from argparse import ArgumentParser
import numpy as np
import tensorflow as tf
from sys import argv

from config import Config
from androidmodel import AndroidModel

# preprocessed android distribution
argv.append("--test")
argv.append("C:/Users/philippe/python-projects/Code2Seq/data/baselines/preprocessed/android/android.data03")

# precomputer model
argv.append("--load")
argv.append("C:/Users/philippe/python-projects/Code2Seq/data/models/java-large-model/model_iter52.release")



if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("-d", "--data", dest="data_path",
                        help="path to preprocessed dataset", required=False)
    parser.add_argument("-te", "--test", dest="test_path",
                        help="path to test file", metavar="FILE", required=False)

    parser.add_argument("-s", "--save_prefix", dest="save_path_prefix",
                        help="path to save file", metavar="FILE", required=False)
    parser.add_argument("-l", "--load", dest="load_path",
                        help="path to saved file", metavar="FILE", required=False)
    parser.add_argument('--release', action='store_true',
                        help='if specified and loading a trained model, release the loaded model for a smaller model '
                             'size.')
    parser.add_argument('--predict', action='store_true')
    parser.add_argument('--debug', action='store_true')
    parser.add_argument('--seed', type=int, default=239)
    args = parser.parse_args()

    np.random.seed(args.seed)
    tf.set_random_seed(args.seed)

    if args.debug:
        conf = Config.get_debug_config(args)
    else:
        conf = Config.get_default_config(args)
    print( "decoder size is " + str(conf.DECODER_SIZE) )
    model = AndroidModel(conf)
    print('Created model')
    print( "model decoder size " + str(model.config.DECODER_SIZE))
    results, precision, recall, f1, rouge = model.evaluate()
    print('Accuracy: ' + str(results))
    print('Precision: ' + str(precision) + ', recall: ' + str(recall) + ', F1: ' + str(f1))
    print('Rouge: ', rouge)
    model.close_session()