.. DBRecord documentation master file, created by
   sphinx-quickstart on Thu Jun  2 16:18:58 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to DBRecord's documentation!
====================================

Introduction
------------
DBRecord is a library to model your relational database in a fully type safe, unopinionated manner using Haskell.
This library catches most of the error during compile time.
Lets you refactor your code with great level of confidence.
This library makes no assumption about your database allowing you to map any relational schema without a problem.
DBRecord support generation of migration code.
This library mainly intends to be mapping layer and supports only a minimal querying interface.
More sophosticated querying layer is offered by libraries built on top of DBRecord like,
 :ref:`DBRecord-Opaleye`


.. toctree::
   :maxdepth: 2
	      
   getting-started
   database-mappings
   models
   custom-types
   queries
   migrations



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

