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
module Database where

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

data ChinookMySql
  deriving Generic

instance Database ChinookMySql where
  type DB     ChinookMySql = 'MySql
  type Tables ChinookMySql = '[ MediaType, Track, Employee, PlaylistTrack, Invoice, Customer, InvoiceLine, Album, Genre, Playlist, Artist]
  type Types ChinookMySql = '[ ]

instance Table ChinookMySql MediaType where 
  type PrimaryKey ChinookMySql MediaType = '["mediaTypeId"]  
  type PrimaryKeyName ChinookMySql MediaType = 'Just "PK_MediaType"
   
  
  type TableName ChinookMySql MediaType = "MediaType" 
  type ColumnNames ChinookMySql MediaType = '[ '("mediaTypeId","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMySql Track where 
  type PrimaryKey ChinookMySql Track = '["trackId"]  
  type PrimaryKeyName ChinookMySql Track = 'Just "PK_Track"
   
  
  type TableName ChinookMySql Track = "Track" 
  type ColumnNames ChinookMySql Track = '[ '("trackId","TrackId"), '("name","Name"), '("albumId","AlbumId"), '("mediaTypeId","MediaTypeId"), '("genreId","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitPrice","UnitPrice")] 
   
  
   
  
  type ForeignKey ChinookMySql Track = '[ 'RefBy '["mediaTypeId"] MediaType '["mediaTypeId"] "fKTrackMediaTypeId" , 'RefBy '["genreId"] Genre '["genreId"] "fKTrackGenreId" , 'RefBy '["albumId"] Album '["albumId"] "fKTrackAlbumId" ] 
  type ForeignKeyNames ChinookMySql Track = '[ '("fKTrackMediaTypeId","FK_TrackMediaTypeId"), '("fKTrackGenreId","FK_TrackGenreId"), '("fKTrackAlbumId","FK_TrackAlbumId")]
   
  


instance Table ChinookMySql Employee where 
  type PrimaryKey ChinookMySql Employee = '["employeeId"]  
  type PrimaryKeyName ChinookMySql Employee = 'Just "PK_Employee"
   
  
  type TableName ChinookMySql Employee = "Employee" 
  type ColumnNames ChinookMySql Employee = '[ '("employeeId","EmployeeId"), '("lastName","LastName"), '("firstName","FirstName"), '("title","Title"), '("reportsTo","ReportsTo"), '("birthDate","BirthDate"), '("hireDate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey ChinookMySql Employee = '[ 'RefBy '["reportsTo"] Employee '["employeeId"] "fKEmployeeReportsTo" ] 
  type ForeignKeyNames ChinookMySql Employee = '[ '("fKEmployeeReportsTo","FK_EmployeeReportsTo")]
   
  


instance Table ChinookMySql PlaylistTrack where 
  type PrimaryKey ChinookMySql PlaylistTrack = '["playlistId","trackId"]  
  type PrimaryKeyName ChinookMySql PlaylistTrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName ChinookMySql PlaylistTrack = "PlaylistTrack" 
  type ColumnNames ChinookMySql PlaylistTrack = '[ '("playlistId","PlaylistId"), '("trackId","TrackId")] 
   
  
   
  
  type ForeignKey ChinookMySql PlaylistTrack = '[ 'RefBy '["trackId"] Track '["trackId"] "fKPlaylistTrackTrackId" , 'RefBy '["playlistId"] Playlist '["playlistId"] "fKPlaylistTrackPlaylistId" ] 
  type ForeignKeyNames ChinookMySql PlaylistTrack = '[ '("fKPlaylistTrackTrackId","FK_PlaylistTrackTrackId"), '("fKPlaylistTrackPlaylistId","FK_PlaylistTrackPlaylistId")]
   
  


instance Table ChinookMySql Invoice where 
  type PrimaryKey ChinookMySql Invoice = '["invoiceId"]  
  type PrimaryKeyName ChinookMySql Invoice = 'Just "PK_Invoice"
   
  
  type TableName ChinookMySql Invoice = "Invoice" 
  type ColumnNames ChinookMySql Invoice = '[ '("invoiceId","InvoiceId"), '("customerId","CustomerId"), '("invoiceDate","InvoiceDate"), '("billingAddress","BillingAddress"), '("billingCity","BillingCity"), '("billingState","BillingState"), '("billingCountry","BillingCountry"), '("billingPostalCode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey ChinookMySql Invoice = '[ 'RefBy '["customerId"] Customer '["customerId"] "fKInvoiceCustomerId" ] 
  type ForeignKeyNames ChinookMySql Invoice = '[ '("fKInvoiceCustomerId","FK_InvoiceCustomerId")]
   
  


instance Table ChinookMySql Customer where 
  type PrimaryKey ChinookMySql Customer = '["customerId"]  
  type PrimaryKeyName ChinookMySql Customer = 'Just "PK_Customer"
   
  
  type TableName ChinookMySql Customer = "Customer" 
  type ColumnNames ChinookMySql Customer = '[ '("customerId","CustomerId"), '("firstName","FirstName"), '("lastName","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportRepId","SupportRepId")] 
   
  
   
  
  type ForeignKey ChinookMySql Customer = '[ 'RefBy '["supportRepId"] Employee '["employeeId"] "fKCustomerSupportRepId" ] 
  type ForeignKeyNames ChinookMySql Customer = '[ '("fKCustomerSupportRepId","FK_CustomerSupportRepId")]
   
  


instance Table ChinookMySql InvoiceLine where 
  type PrimaryKey ChinookMySql InvoiceLine = '["invoiceLineId"]  
  type PrimaryKeyName ChinookMySql InvoiceLine = 'Just "PK_InvoiceLine"
   
  
  type TableName ChinookMySql InvoiceLine = "InvoiceLine" 
  type ColumnNames ChinookMySql InvoiceLine = '[ '("invoiceLineId","InvoiceLineId"), '("invoiceId","InvoiceId"), '("trackId","TrackId"), '("unitPrice","UnitPrice"), '("quantity","Quantity")] 
   
  
   
  
  type ForeignKey ChinookMySql InvoiceLine = '[ 'RefBy '["trackId"] Track '["trackId"] "fKInvoiceLineTrackId" , 'RefBy '["invoiceId"] Invoice '["invoiceId"] "fKInvoiceLineInvoiceId" ] 
  type ForeignKeyNames ChinookMySql InvoiceLine = '[ '("fKInvoiceLineTrackId","FK_InvoiceLineTrackId"), '("fKInvoiceLineInvoiceId","FK_InvoiceLineInvoiceId")]
   
  


instance Table ChinookMySql Album where 
  type PrimaryKey ChinookMySql Album = '["albumId"]  
  type PrimaryKeyName ChinookMySql Album = 'Just "PK_Album"
   
  
  type TableName ChinookMySql Album = "Album" 
  type ColumnNames ChinookMySql Album = '[ '("albumId","AlbumId"), '("title","Title"), '("artistId","ArtistId")] 
   
  
   
  
  type ForeignKey ChinookMySql Album = '[ 'RefBy '["artistId"] Artist '["artistId"] "fKAlbumArtistId" ] 
  type ForeignKeyNames ChinookMySql Album = '[ '("fKAlbumArtistId","FK_AlbumArtistId")]
   
  


instance Table ChinookMySql Genre where 
  type PrimaryKey ChinookMySql Genre = '["genreId"]  
  type PrimaryKeyName ChinookMySql Genre = 'Just "PK_Genre"
   
  
  type TableName ChinookMySql Genre = "Genre" 
  type ColumnNames ChinookMySql Genre = '[ '("genreId","GenreId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMySql Playlist where 
  type PrimaryKey ChinookMySql Playlist = '["playlistId"]  
  type PrimaryKeyName ChinookMySql Playlist = 'Just "PK_Playlist"
   
  
  type TableName ChinookMySql Playlist = "Playlist" 
  type ColumnNames ChinookMySql Playlist = '[ '("playlistId","PlaylistId"), '("name","Name")] 
  
instance Table ChinookMySql Artist where 
  type PrimaryKey ChinookMySql Artist = '["artistId"]  
  type PrimaryKeyName ChinookMySql Artist = 'Just "PK_Artist"
   
  
  type TableName ChinookMySql Artist = "Artist" 
  type ColumnNames ChinookMySql Artist = '[ '("artistId","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

