#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 14 10:39:08 2020

@author: hamishgibbs
"""
from sklearn.ensemble import RandomForestClassifier
from skmultilearn.problem_transform import LabelPowerset
from skmultilearn.cluster import IGraphLabelGraphClusterer, LabelCooccurrenceGraphBuilder, StochasticBlockModel, GraphToolLabelGraphClusterer
from skmultilearn.ensemble import LabelSpacePartitioningClassifier
from skmultilearn.dataset import load_dataset
X_train, y_train, feature_names, label_names = load_dataset('emotions', 'train')
X_test, y_test, _, _ = load_dataset('emotions', 'test')

#%%
base_classifier = RandomForestClassifier(n_estimators=1000)
#%%
graph_builder = LabelCooccurrenceGraphBuilder(
    weighted = True,
    include_self_edges = False
)
#%%
model = StochasticBlockModel(
    nested=False,
    use_degree_correlation=True,
    allow_overlap=False,
    weight_model='real-normal'
)
#%%
problem_transform_classifier = LabelPowerset(classifier=base_classifier,
    require_dense=[False, False])
#%%
clusterer = GraphToolLabelGraphClusterer(graph_builder=graph_builder, model=model)

#%%
classifier = LabelSpacePartitioningClassifier(problem_transform_classifier, clusterer)
#%%
classifier.fit(X_train, y_train)

#%%
predictions = classifier.predict(X_test)
#%%
import networkx as nx
x = nx.to_scipy_sparse_matrix(od_graph(mob[0]))
#%%
classifier.fit(x, y_train)