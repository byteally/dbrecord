Database Mappings
=================
.. toctree::
   :maxdepth: 2


database
--------
Define haskell data type to represent your database mapping

.. code-block:: haskell
   :name: db-type

   data UserDB

Mapping to a database is done by writing instance of class :ref:`Database`

.. code-block:: haskell
   :name: db-instance

   instance Database UserDB where
      type Tables UserDB = '[ User
                            , Profile
                            ]
      type Types UserDB  = '[ UserRole
                            ]


The above code declare the UserDB with two table User and Profile and also a mapping for custom datatype

The following aspect of the database can be mapped using this instance.

List of tables that present in the database

.. code-block::
   :name: tables-tf
   type Tables UserDB = '[User, Profile, Address]
   
List of custom types

Schema name of the mappings

List of existing tables that are ignored


tables
------
