
{-# LANGUAGE DataKinds
           , TypeOperators
           , UndecidableInstances
           , OverloadedLabels
           , FlexibleInstances
           , MultiParamTypeClasses
           , DuplicateRecordFields
           , GADTs
           , TypeApplications
           , KindSignatures
           , DeriveGeneric
           , FlexibleContexts
           , FunctionalDependencies
           , ExplicitForAll
           , TypeFamilies
           , ScopedTypeVariables
           , OverloadedStrings
           , GeneralizedNewtypeDeriving
           , RankNTypes #-}
module Test.Chinook.Sqlite.Database where

import DBRecord.Schema
import DBRecord.Internal.DBTypes
import GHC.Generics

import Test.Chinook.Models.MediaType
import Test.Chinook.Models.Track
import Test.Chinook.Models.Employee
import Test.Chinook.Models.PlaylistTrack
import Test.Chinook.Models.Invoice
import Test.Chinook.Models.Customer
import Test.Chinook.Models.InvoiceLine
import Test.Chinook.Models.Album
import Test.Chinook.Models.Genre
import Test.Chinook.Models.Playlist
import Test.Chinook.Models.Artist

data ChinookSqlite
  deriving Generic

instance Database ChinookSqlite where
  type DB     ChinookSqlite = 'Postgres
  type Tables ChinookSqlite = '[ MediaType, Track, Employee, PlaylistTrack, Invoice, Customer, InvoiceLine, Album, Genre, Playlist, Artist]
  type Types ChinookSqlite = '[ ]

instance Table ChinookSqlite MediaType where 
  type PrimaryKey ChinookSqlite MediaType = '["mediaTypeId"]  
  type PrimaryKeyName ChinookSqlite MediaType = 'Just "PK_MediaType"
   
  
  type TableName ChinookSqlite MediaType = "MediaType" 
  type ColumnNames ChinookSqlite MediaType = '[ '("mediaTypeId","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookSqlite Track where 
  type PrimaryKey ChinookSqlite Track = '["trackId"]  
  type PrimaryKeyName ChinookSqlite Track = 'Just "PK_Track"
   
  
  type TableName ChinookSqlite Track = "Track" 
  type ColumnNames ChinookSqlite Track = '[ '("trackId","TrackId"), '("name","Name"), '("albumId","AlbumId"), '("mediaTypeId","MediaTypeId"), '("genreId","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitPrice","UnitPrice")] 
   
  
   
  
  type ForeignKey ChinookSqlite Track = '[ 'RefBy '["mediaTypeId"] MediaType '["mediaTypeId"] "fKTrackMediaTypeId" , 'RefBy '["genreId"] Genre '["genreId"] "fKTrackGenreId" , 'RefBy '["albumId"] Album '["albumId"] "fKTrackAlbumId" ] 
  type ForeignKeyNames ChinookSqlite Track = '[ '("fKTrackMediaTypeId","FK_TrackMediaTypeId"), '("fKTrackGenreId","FK_TrackGenreId"), '("fKTrackAlbumId","FK_TrackAlbumId")]
   
  


instance Table ChinookSqlite Employee where 
  type PrimaryKey ChinookSqlite Employee = '["employeeId"]  
  type PrimaryKeyName ChinookSqlite Employee = 'Just "PK_Employee"
   
  
  type TableName ChinookSqlite Employee = "Employee" 
  type ColumnNames ChinookSqlite Employee = '[ '("employeeId","EmployeeId"), '("lastName","LastName"), '("firstName","FirstName"), '("title","Title"), '("reportsTo","ReportsTo"), '("birthDate","BirthDate"), '("hireDate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey ChinookSqlite Employee = '[ 'RefBy '["reportsTo"] Employee '["employeeId"] "fKEmployeeReportsTo" ] 
  type ForeignKeyNames ChinookSqlite Employee = '[ '("fKEmployeeReportsTo","FK_EmployeeReportsTo")]
   
  


instance Table ChinookSqlite PlaylistTrack where 
  type PrimaryKey ChinookSqlite PlaylistTrack = '["playlistId","trackId"]  
  type PrimaryKeyName ChinookSqlite PlaylistTrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName ChinookSqlite PlaylistTrack = "PlaylistTrack" 
  type ColumnNames ChinookSqlite PlaylistTrack = '[ '("playlistId","PlaylistId"), '("trackId","TrackId")] 
   
  
   
  
  type ForeignKey ChinookSqlite PlaylistTrack = '[ 'RefBy '["trackId"] Track '["trackId"] "fKPlaylistTrackTrackId" , 'RefBy '["playlistId"] Playlist '["playlistId"] "fKPlaylistTrackPlaylistId" ] 
  type ForeignKeyNames ChinookSqlite PlaylistTrack = '[ '("fKPlaylistTrackTrackId","FK_PlaylistTrackTrackId"), '("fKPlaylistTrackPlaylistId","FK_PlaylistTrackPlaylistId")]
   
  


instance Table ChinookSqlite Invoice where 
  type PrimaryKey ChinookSqlite Invoice = '["invoiceId"]  
  type PrimaryKeyName ChinookSqlite Invoice = 'Just "PK_Invoice"
   
  
  type TableName ChinookSqlite Invoice = "Invoice" 
  type ColumnNames ChinookSqlite Invoice = '[ '("invoiceId","InvoiceId"), '("customerId","CustomerId"), '("invoiceDate","InvoiceDate"), '("billingAddress","BillingAddress"), '("billingCity","BillingCity"), '("billingState","BillingState"), '("billingCountry","BillingCountry"), '("billingPostalCode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey ChinookSqlite Invoice = '[ 'RefBy '["customerId"] Customer '["customerId"] "fKInvoiceCustomerId" ] 
  type ForeignKeyNames ChinookSqlite Invoice = '[ '("fKInvoiceCustomerId","FK_InvoiceCustomerId")]
   
  


instance Table ChinookSqlite Customer where 
  type PrimaryKey ChinookSqlite Customer = '["customerId"]  
  type PrimaryKeyName ChinookSqlite Customer = 'Just "PK_Customer"
   
  
  type TableName ChinookSqlite Customer = "Customer" 
  type ColumnNames ChinookSqlite Customer = '[ '("customerId","CustomerId"), '("firstName","FirstName"), '("lastName","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportRepId","SupportRepId")]
  
  type ForeignKey ChinookSqlite Customer = '[ 'RefBy '["supportRepId"] Employee '["employeeId"] "fKCustomerSupportRepId" ] 
  type ForeignKeyNames ChinookSqlite Customer = '[ '("fKCustomerSupportRepId","FK_CustomerSupportRepId")]
   
instance Table ChinookSqlite InvoiceLine where 
  type PrimaryKey ChinookSqlite InvoiceLine = '["invoiceLineId"]  
  type PrimaryKeyName ChinookSqlite InvoiceLine = 'Just "PK_InvoiceLine"
   
  
  type TableName ChinookSqlite InvoiceLine = "InvoiceLine" 
  type ColumnNames ChinookSqlite InvoiceLine = '[ '("invoiceLineId","InvoiceLineId"), '("invoiceId","InvoiceId"), '("trackId","TrackId"), '("unitPrice","UnitPrice"), '("quantity","Quantity")] 
  type ForeignKey ChinookSqlite InvoiceLine = '[ 'RefBy '["trackId"] Track '["trackId"] "fKInvoiceLineTrackId" , 'RefBy '["invoiceId"] Invoice '["invoiceId"] "fKInvoiceLineInvoiceId" ] 
  type ForeignKeyNames ChinookSqlite InvoiceLine = '[ '("fKInvoiceLineTrackId","FK_InvoiceLineTrackId"), '("fKInvoiceLineInvoiceId","FK_InvoiceLineInvoiceId")]
   
instance Table ChinookSqlite Album where 
  type PrimaryKey ChinookSqlite Album = '["albumId"]  
  type PrimaryKeyName ChinookSqlite Album = 'Just "PK_Album"
  type TableName ChinookSqlite Album = "Album" 
  type ColumnNames ChinookSqlite Album = '[ '("albumId","AlbumId"), '("title","Title"), '("artistId","ArtistId")] 
  type ForeignKey ChinookSqlite Album = '[ 'RefBy '["artistId"] Artist '["artistId"] "fKAlbumArtistId" ] 
  type ForeignKeyNames ChinookSqlite Album = '[ '("fKAlbumArtistId","FK_AlbumArtistId")]
   
instance Table ChinookSqlite Genre where 
  type PrimaryKey ChinookSqlite Genre = '["genreId"]  
  type PrimaryKeyName ChinookSqlite Genre = 'Just "PK_Genre"
   
  
  type TableName ChinookSqlite Genre = "Genre" 
  type ColumnNames ChinookSqlite Genre = '[ '("genreId","GenreId"), '("name","Name")] 
   
instance Table ChinookSqlite Playlist where 
  type PrimaryKey ChinookSqlite Playlist = '["playlistId"]  
  type PrimaryKeyName ChinookSqlite Playlist = 'Just "PK_Playlist"
   
  
  type TableName ChinookSqlite Playlist = "Playlist" 
  type ColumnNames ChinookSqlite Playlist = '[ '("playlistId","PlaylistId"), '("name","Name")] 
   
instance Table ChinookSqlite Artist where 
  type PrimaryKey ChinookSqlite Artist = '["artistId"]  
  type PrimaryKeyName ChinookSqlite Artist = 'Just "PK_Artist"
   
  
  type TableName ChinookSqlite Artist = "Artist" 
  type ColumnNames ChinookSqlite Artist = '[ '("artistId","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

