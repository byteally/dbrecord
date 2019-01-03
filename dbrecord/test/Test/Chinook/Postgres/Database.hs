
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
module Chinook.Postgres.Database where

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

data ChinookPG
  deriving Generic

instance Database ChinookPG where
  type DB     ChinookPG = 'Postgres
  type Tables ChinookPG = '[ MediaType, Track, Employee, PlaylistTrack, Invoice, Customer, InvoiceLine, Album, Genre, Playlist, Artist]
  type Types ChinookPG = '[ ]

instance Table ChinookPG MediaType where 
  type PrimaryKey ChinookPG MediaType = '["mediaTypeId"]  
  type PrimaryKeyName ChinookPG MediaType = 'Just "PK_MediaType"
   
  
  type TableName ChinookPG MediaType = "MediaType" 
  type ColumnNames ChinookPG MediaType = '[ '("mediaTypeId","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookPG Track where 
  type PrimaryKey ChinookPG Track = '["trackId"]  
  type PrimaryKeyName ChinookPG Track = 'Just "PK_Track"
   
  
  type TableName ChinookPG Track = "Track" 
  type ColumnNames ChinookPG Track = '[ '("trackId","TrackId"), '("name","Name"), '("albumId","AlbumId"), '("mediaTypeId","MediaTypeId"), '("genreId","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitPrice","UnitPrice")] 
   
  
   
  
  type ForeignKey ChinookPG Track = '[ 'RefBy '["mediaTypeId"] MediaType '["mediaTypeId"] "fKTrackMediaTypeId" , 'RefBy '["genreId"] Genre '["genreId"] "fKTrackGenreId" , 'RefBy '["albumId"] Album '["albumId"] "fKTrackAlbumId" ] 
  type ForeignKeyNames ChinookPG Track = '[ '("fKTrackMediaTypeId","FK_TrackMediaTypeId"), '("fKTrackGenreId","FK_TrackGenreId"), '("fKTrackAlbumId","FK_TrackAlbumId")]
   
  


instance Table ChinookPG Employee where 
  type PrimaryKey ChinookPG Employee = '["employeeId"]  
  type PrimaryKeyName ChinookPG Employee = 'Just "PK_Employee"
   
  
  type TableName ChinookPG Employee = "Employee" 
  type ColumnNames ChinookPG Employee = '[ '("employeeId","EmployeeId"), '("lastName","LastName"), '("firstName","FirstName"), '("title","Title"), '("reportsTo","ReportsTo"), '("birthDate","BirthDate"), '("hireDate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey ChinookPG Employee = '[ 'RefBy '["reportsTo"] Employee '["employeeId"] "fKEmployeeReportsTo" ] 
  type ForeignKeyNames ChinookPG Employee = '[ '("fKEmployeeReportsTo","FK_EmployeeReportsTo")]
   
  


instance Table ChinookPG PlaylistTrack where 
  type PrimaryKey ChinookPG PlaylistTrack = '["playlistId","trackId"]  
  type PrimaryKeyName ChinookPG PlaylistTrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName ChinookPG PlaylistTrack = "PlaylistTrack" 
  type ColumnNames ChinookPG PlaylistTrack = '[ '("playlistId","PlaylistId"), '("trackId","TrackId")] 
   
  
   
  
  type ForeignKey ChinookPG PlaylistTrack = '[ 'RefBy '["trackId"] Track '["trackId"] "fKPlaylistTrackTrackId" , 'RefBy '["playlistId"] Playlist '["playlistId"] "fKPlaylistTrackPlaylistId" ] 
  type ForeignKeyNames ChinookPG PlaylistTrack = '[ '("fKPlaylistTrackTrackId","FK_PlaylistTrackTrackId"), '("fKPlaylistTrackPlaylistId","FK_PlaylistTrackPlaylistId")]
   
  


instance Table ChinookPG Invoice where 
  type PrimaryKey ChinookPG Invoice = '["invoiceId"]  
  type PrimaryKeyName ChinookPG Invoice = 'Just "PK_Invoice"
   
  
  type TableName ChinookPG Invoice = "Invoice" 
  type ColumnNames ChinookPG Invoice = '[ '("invoiceId","InvoiceId"), '("customerId","CustomerId"), '("invoiceDate","InvoiceDate"), '("billingAddress","BillingAddress"), '("billingCity","BillingCity"), '("billingState","BillingState"), '("billingCountry","BillingCountry"), '("billingPostalCode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey ChinookPG Invoice = '[ 'RefBy '["customerId"] Customer '["customerId"] "fKInvoiceCustomerId" ] 
  type ForeignKeyNames ChinookPG Invoice = '[ '("fKInvoiceCustomerId","FK_InvoiceCustomerId")]
   
  


instance Table ChinookPG Customer where 
  type PrimaryKey ChinookPG Customer = '["customerId"]  
  type PrimaryKeyName ChinookPG Customer = 'Just "PK_Customer"
   
  
  type TableName ChinookPG Customer = "Customer" 
  type ColumnNames ChinookPG Customer = '[ '("customerId","CustomerId"), '("firstName","FirstName"), '("lastName","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportRepId","SupportRepId")] 
   
  
   
  
  type ForeignKey ChinookPG Customer = '[ 'RefBy '["supportRepId"] Employee '["employeeId"] "fKCustomerSupportRepId" ] 
  type ForeignKeyNames ChinookPG Customer = '[ '("fKCustomerSupportRepId","FK_CustomerSupportRepId")]
   
  


instance Table ChinookPG InvoiceLine where 
  type PrimaryKey ChinookPG InvoiceLine = '["invoiceLineId"]  
  type PrimaryKeyName ChinookPG InvoiceLine = 'Just "PK_InvoiceLine"
   
  
  type TableName ChinookPG InvoiceLine = "InvoiceLine" 
  type ColumnNames ChinookPG InvoiceLine = '[ '("invoiceLineId","InvoiceLineId"), '("invoiceId","InvoiceId"), '("trackId","TrackId"), '("unitPrice","UnitPrice"), '("quantity","Quantity")] 
   
  
   
  
  type ForeignKey ChinookPG InvoiceLine = '[ 'RefBy '["trackId"] Track '["trackId"] "fKInvoiceLineTrackId" , 'RefBy '["invoiceId"] Invoice '["invoiceId"] "fKInvoiceLineInvoiceId" ] 
  type ForeignKeyNames ChinookPG InvoiceLine = '[ '("fKInvoiceLineTrackId","FK_InvoiceLineTrackId"), '("fKInvoiceLineInvoiceId","FK_InvoiceLineInvoiceId")]
   
  


instance Table ChinookPG Album where 
  type PrimaryKey ChinookPG Album = '["albumId"]  
  type PrimaryKeyName ChinookPG Album = 'Just "PK_Album"
   
  
  type TableName ChinookPG Album = "Album" 
  type ColumnNames ChinookPG Album = '[ '("albumId","AlbumId"), '("title","Title"), '("artistId","ArtistId")] 
   
  
   
  
  type ForeignKey ChinookPG Album = '[ 'RefBy '["artistId"] Artist '["artistId"] "fKAlbumArtistId" ] 
  type ForeignKeyNames ChinookPG Album = '[ '("fKAlbumArtistId","FK_AlbumArtistId")]
   
  


instance Table ChinookPG Genre where 
  type PrimaryKey ChinookPG Genre = '["genreId"]  
  type PrimaryKeyName ChinookPG Genre = 'Just "PK_Genre"
   
  
  type TableName ChinookPG Genre = "Genre" 
  type ColumnNames ChinookPG Genre = '[ '("genreId","GenreId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookPG Playlist where 
  type PrimaryKey ChinookPG Playlist = '["playlistId"]  
  type PrimaryKeyName ChinookPG Playlist = 'Just "PK_Playlist"
   
  
  type TableName ChinookPG Playlist = "Playlist" 
  type ColumnNames ChinookPG Playlist = '[ '("playlistId","PlaylistId"), '("name","Name")] 
   

instance Table ChinookPG Artist where 
  type PrimaryKey ChinookPG Artist = '["artistId"]  
  type PrimaryKeyName ChinookPG Artist = 'Just "PK_Artist"
   
  
  type TableName ChinookPG Artist = "Artist" 
  type ColumnNames ChinookPG Artist = '[ '("artistId","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

