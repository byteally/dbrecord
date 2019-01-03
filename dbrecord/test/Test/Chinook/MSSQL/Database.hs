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
module Chinook.MSSQL.Database where

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

data ChinookMSSQL
  deriving Generic

instance Database ChinookMSSQL where
  type DB     ChinookMSSQL = 'MSSQL
  type Tables ChinookMSSQL = '[ MediaType, Track, Employee, PlaylistTrack, Invoice, Customer, InvoiceLine, Album, Genre, Playlist, Artist]
  type Types ChinookMSSQL = '[ ]

instance Table ChinookMSSQL MediaType where 
  type PrimaryKey ChinookMSSQL MediaType = '["mediaTypeId"]  
  type PrimaryKeyName ChinookMSSQL MediaType = 'Just "PK_MediaType"
   
  
  type TableName ChinookMSSQL MediaType = "MediaType" 
  type ColumnNames ChinookMSSQL MediaType = '[ '("mediaTypeId","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMSSQL Track where 
  type PrimaryKey ChinookMSSQL Track = '["trackId"]  
  type PrimaryKeyName ChinookMSSQL Track = 'Just "PK_Track"
   
  
  type TableName ChinookMSSQL Track = "Track" 
  type ColumnNames ChinookMSSQL Track = '[ '("trackId","TrackId"), '("name","Name"), '("albumId","AlbumId"), '("mediaTypeId","MediaTypeId"), '("genreId","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitPrice","UnitPrice")] 
   
  
   
  
  type ForeignKey ChinookMSSQL Track = '[ 'RefBy '["mediaTypeId"] MediaType '["mediaTypeId"] "fKTrackMediaTypeId" , 'RefBy '["genreId"] Genre '["genreId"] "fKTrackGenreId" , 'RefBy '["albumId"] Album '["albumId"] "fKTrackAlbumId" ] 
  type ForeignKeyNames ChinookMSSQL Track = '[ '("fKTrackMediaTypeId","FK_TrackMediaTypeId"), '("fKTrackGenreId","FK_TrackGenreId"), '("fKTrackAlbumId","FK_TrackAlbumId")]
   
  


instance Table ChinookMSSQL Employee where 
  type PrimaryKey ChinookMSSQL Employee = '["employeeId"]  
  type PrimaryKeyName ChinookMSSQL Employee = 'Just "PK_Employee"
   
  
  type TableName ChinookMSSQL Employee = "Employee" 
  type ColumnNames ChinookMSSQL Employee = '[ '("employeeId","EmployeeId"), '("lastName","LastName"), '("firstName","FirstName"), '("title","Title"), '("reportsTo","ReportsTo"), '("birthDate","BirthDate"), '("hireDate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey ChinookMSSQL Employee = '[ 'RefBy '["reportsTo"] Employee '["employeeId"] "fKEmployeeReportsTo" ] 
  type ForeignKeyNames ChinookMSSQL Employee = '[ '("fKEmployeeReportsTo","FK_EmployeeReportsTo")]
   
  


instance Table ChinookMSSQL PlaylistTrack where 
  type PrimaryKey ChinookMSSQL PlaylistTrack = '["playlistId","trackId"]  
  type PrimaryKeyName ChinookMSSQL PlaylistTrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName ChinookMSSQL PlaylistTrack = "PlaylistTrack" 
  type ColumnNames ChinookMSSQL PlaylistTrack = '[ '("playlistId","PlaylistId"), '("trackId","TrackId")] 
   
  
   
  
  type ForeignKey ChinookMSSQL PlaylistTrack = '[ 'RefBy '["trackId"] Track '["trackId"] "fKPlaylistTrackTrackId" , 'RefBy '["playlistId"] Playlist '["playlistId"] "fKPlaylistTrackPlaylistId" ] 
  type ForeignKeyNames ChinookMSSQL PlaylistTrack = '[ '("fKPlaylistTrackTrackId","FK_PlaylistTrackTrackId"), '("fKPlaylistTrackPlaylistId","FK_PlaylistTrackPlaylistId")]
   
  


instance Table ChinookMSSQL Invoice where 
  type PrimaryKey ChinookMSSQL Invoice = '["invoiceId"]  
  type PrimaryKeyName ChinookMSSQL Invoice = 'Just "PK_Invoice"
   
  
  type TableName ChinookMSSQL Invoice = "Invoice" 
  type ColumnNames ChinookMSSQL Invoice = '[ '("invoiceId","InvoiceId"), '("customerId","CustomerId"), '("invoiceDate","InvoiceDate"), '("billingAddress","BillingAddress"), '("billingCity","BillingCity"), '("billingState","BillingState"), '("billingCountry","BillingCountry"), '("billingPostalCode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey ChinookMSSQL Invoice = '[ 'RefBy '["customerId"] Customer '["customerId"] "fKInvoiceCustomerId" ] 
  type ForeignKeyNames ChinookMSSQL Invoice = '[ '("fKInvoiceCustomerId","FK_InvoiceCustomerId")]
   
  


instance Table ChinookMSSQL Customer where 
  type PrimaryKey ChinookMSSQL Customer = '["customerId"]  
  type PrimaryKeyName ChinookMSSQL Customer = 'Just "PK_Customer"
   
  
  type TableName ChinookMSSQL Customer = "Customer" 
  type ColumnNames ChinookMSSQL Customer = '[ '("customerId","CustomerId"), '("firstName","FirstName"), '("lastName","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportRepId","SupportRepId")] 
   
  
   
  
  type ForeignKey ChinookMSSQL Customer = '[ 'RefBy '["supportRepId"] Employee '["employeeId"] "fKCustomerSupportRepId" ] 
  type ForeignKeyNames ChinookMSSQL Customer = '[ '("fKCustomerSupportRepId","FK_CustomerSupportRepId")]
   
  


instance Table ChinookMSSQL InvoiceLine where 
  type PrimaryKey ChinookMSSQL InvoiceLine = '["invoiceLineId"]  
  type PrimaryKeyName ChinookMSSQL InvoiceLine = 'Just "PK_InvoiceLine"
   
  
  type TableName ChinookMSSQL InvoiceLine = "InvoiceLine" 
  type ColumnNames ChinookMSSQL InvoiceLine = '[ '("invoiceLineId","InvoiceLineId"), '("invoiceId","InvoiceId"), '("trackId","TrackId"), '("unitPrice","UnitPrice"), '("quantity","Quantity")] 
   
  
   
  
  type ForeignKey ChinookMSSQL InvoiceLine = '[ 'RefBy '["trackId"] Track '["trackId"] "fKInvoiceLineTrackId" , 'RefBy '["invoiceId"] Invoice '["invoiceId"] "fKInvoiceLineInvoiceId" ] 
  type ForeignKeyNames ChinookMSSQL InvoiceLine = '[ '("fKInvoiceLineTrackId","FK_InvoiceLineTrackId"), '("fKInvoiceLineInvoiceId","FK_InvoiceLineInvoiceId")]
   
  


instance Table ChinookMSSQL Album where 
  type PrimaryKey ChinookMSSQL Album = '["albumId"]  
  type PrimaryKeyName ChinookMSSQL Album = 'Just "PK_Album"
   
  
  type TableName ChinookMSSQL Album = "Album" 
  type ColumnNames ChinookMSSQL Album = '[ '("albumId","AlbumId"), '("title","Title"), '("artistId","ArtistId")] 
   
  
   
  
  type ForeignKey ChinookMSSQL Album = '[ 'RefBy '["artistId"] Artist '["artistId"] "fKAlbumArtistId" ] 
  type ForeignKeyNames ChinookMSSQL Album = '[ '("fKAlbumArtistId","FK_AlbumArtistId")]
   
  


instance Table ChinookMSSQL Genre where 
  type PrimaryKey ChinookMSSQL Genre = '["genreId"]  
  type PrimaryKeyName ChinookMSSQL Genre = 'Just "PK_Genre"
   
  
  type TableName ChinookMSSQL Genre = "Genre" 
  type ColumnNames ChinookMSSQL Genre = '[ '("genreId","GenreId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMSSQL Playlist where 
  type PrimaryKey ChinookMSSQL Playlist = '["playlistId"]  
  type PrimaryKeyName ChinookMSSQL Playlist = 'Just "PK_Playlist"
   
  
  type TableName ChinookMSSQL Playlist = "Playlist" 
  type ColumnNames ChinookMSSQL Playlist = '[ '("playlistId","PlaylistId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMSSQL Artist where 
  type PrimaryKey ChinookMSSQL Artist = '["artistId"]  
  type PrimaryKeyName ChinookMSSQL Artist = 'Just "PK_Artist"
   
  
  type TableName ChinookMSSQL Artist = "Artist" 
  type ColumnNames ChinookMSSQL Artist = '[ '("artistId","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

