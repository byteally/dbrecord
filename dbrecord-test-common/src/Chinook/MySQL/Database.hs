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
module Chinook.MySQL.Database where

import DBRecord.Schema
import DBRecord.Internal.DBTypes
import GHC.Generics

import Chinook.Models.MediaType
import Chinook.Models.Track
import Chinook.Models.Employee
import Chinook.Models.PlaylistTrack
import Chinook.Models.Invoice
import Chinook.Models.Customer
import Chinook.Models.InvoiceLine
import Chinook.Models.Album
import Chinook.Models.Genre
import Chinook.Models.Playlist
import Chinook.Models.Artist

data ChinookMySQL
  deriving Generic

instance Database ChinookMySQL where
  type DB     ChinookMySQL = 'MySQL
  type Tables ChinookMySQL = '[ MediaType, Track, Employee, PlaylistTrack, Invoice, Customer, InvoiceLine, Album, Genre, Playlist, Artist]
  type Types ChinookMySQL = '[ ]

instance Table ChinookMySQL MediaType where 
  type PrimaryKey ChinookMySQL MediaType = '["mediaTypeId"]  
  type PrimaryKeyName ChinookMySQL MediaType = 'Just "PK_MediaType"
   
  
  type TableName ChinookMySQL MediaType = "MediaType" 
  type ColumnNames ChinookMySQL MediaType = '[ '("mediaTypeId","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMySQL Track where 
  type PrimaryKey ChinookMySQL Track = '["trackId"]  
  type PrimaryKeyName ChinookMySQL Track = 'Just "PK_Track"
   
  
  type TableName ChinookMySQL Track = "Track" 
  type ColumnNames ChinookMySQL Track = '[ '("trackId","TrackId"), '("name","Name"), '("albumId","AlbumId"), '("mediaTypeId","MediaTypeId"), '("genreId","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitPrice","UnitPrice")] 
   
  
   
  
  type ForeignKey ChinookMySQL Track = '[ 'RefBy '["mediaTypeId"] MediaType '["mediaTypeId"] "fKTrackMediaTypeId" , 'RefBy '["genreId"] Genre '["genreId"] "fKTrackGenreId" , 'RefBy '["albumId"] Album '["albumId"] "fKTrackAlbumId" ] 
  type ForeignKeyNames ChinookMySQL Track = '[ '("fKTrackMediaTypeId","FK_TrackMediaTypeId"), '("fKTrackGenreId","FK_TrackGenreId"), '("fKTrackAlbumId","FK_TrackAlbumId")]
   
  


instance Table ChinookMySQL Employee where 
  type PrimaryKey ChinookMySQL Employee = '["employeeId"]  
  type PrimaryKeyName ChinookMySQL Employee = 'Just "PK_Employee"
   
  
  type TableName ChinookMySQL Employee = "Employee" 
  type ColumnNames ChinookMySQL Employee = '[ '("employeeId","EmployeeId"), '("lastName","LastName"), '("firstName","FirstName"), '("title","Title"), '("reportsTo","ReportsTo"), '("birthDate","BirthDate"), '("hireDate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey ChinookMySQL Employee = '[ 'RefBy '["reportsTo"] Employee '["employeeId"] "fKEmployeeReportsTo" ] 
  type ForeignKeyNames ChinookMySQL Employee = '[ '("fKEmployeeReportsTo","FK_EmployeeReportsTo")]
   
  


instance Table ChinookMySQL PlaylistTrack where 
  type PrimaryKey ChinookMySQL PlaylistTrack = '["playlistId","trackId"]  
  type PrimaryKeyName ChinookMySQL PlaylistTrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName ChinookMySQL PlaylistTrack = "PlaylistTrack" 
  type ColumnNames ChinookMySQL PlaylistTrack = '[ '("playlistId","PlaylistId"), '("trackId","TrackId")] 
   
  
   
  
  type ForeignKey ChinookMySQL PlaylistTrack = '[ 'RefBy '["trackId"] Track '["trackId"] "fKPlaylistTrackTrackId" , 'RefBy '["playlistId"] Playlist '["playlistId"] "fKPlaylistTrackPlaylistId" ] 
  type ForeignKeyNames ChinookMySQL PlaylistTrack = '[ '("fKPlaylistTrackTrackId","FK_PlaylistTrackTrackId"), '("fKPlaylistTrackPlaylistId","FK_PlaylistTrackPlaylistId")]
   
  


instance Table ChinookMySQL Invoice where 
  type PrimaryKey ChinookMySQL Invoice = '["invoiceId"]  
  type PrimaryKeyName ChinookMySQL Invoice = 'Just "PK_Invoice"
   
  
  type TableName ChinookMySQL Invoice = "Invoice" 
  type ColumnNames ChinookMySQL Invoice = '[ '("invoiceId","InvoiceId"), '("customerId","CustomerId"), '("invoiceDate","InvoiceDate"), '("billingAddress","BillingAddress"), '("billingCity","BillingCity"), '("billingState","BillingState"), '("billingCountry","BillingCountry"), '("billingPostalCode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey ChinookMySQL Invoice = '[ 'RefBy '["customerId"] Customer '["customerId"] "fKInvoiceCustomerId" ] 
  type ForeignKeyNames ChinookMySQL Invoice = '[ '("fKInvoiceCustomerId","FK_InvoiceCustomerId")]
   
  


instance Table ChinookMySQL Customer where 
  type PrimaryKey ChinookMySQL Customer = '["customerId"]  
  type PrimaryKeyName ChinookMySQL Customer = 'Just "PK_Customer"
   
  
  type TableName ChinookMySQL Customer = "Customer" 
  type ColumnNames ChinookMySQL Customer = '[ '("customerId","CustomerId"), '("firstName","FirstName"), '("lastName","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalCode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportRepId","SupportRepId")] 
   
  
   
  
  type ForeignKey ChinookMySQL Customer = '[ 'RefBy '["supportRepId"] Employee '["employeeId"] "fKCustomerSupportRepId" ] 
  type ForeignKeyNames ChinookMySQL Customer = '[ '("fKCustomerSupportRepId","FK_CustomerSupportRepId")]
   
  


instance Table ChinookMySQL InvoiceLine where 
  type PrimaryKey ChinookMySQL InvoiceLine = '["invoiceLineId"]  
  type PrimaryKeyName ChinookMySQL InvoiceLine = 'Just "PK_InvoiceLine"
   
  
  type TableName ChinookMySQL InvoiceLine = "InvoiceLine" 
  type ColumnNames ChinookMySQL InvoiceLine = '[ '("invoiceLineId","InvoiceLineId"), '("invoiceId","InvoiceId"), '("trackId","TrackId"), '("unitPrice","UnitPrice"), '("quantity","Quantity")] 
   
  
   
  
  type ForeignKey ChinookMySQL InvoiceLine = '[ 'RefBy '["trackId"] Track '["trackId"] "fKInvoiceLineTrackId" , 'RefBy '["invoiceId"] Invoice '["invoiceId"] "fKInvoiceLineInvoiceId" ] 
  type ForeignKeyNames ChinookMySQL InvoiceLine = '[ '("fKInvoiceLineTrackId","FK_InvoiceLineTrackId"), '("fKInvoiceLineInvoiceId","FK_InvoiceLineInvoiceId")]
   
  


instance Table ChinookMySQL Album where 
  type PrimaryKey ChinookMySQL Album = '["albumId"]  
  type PrimaryKeyName ChinookMySQL Album = 'Just "PK_Album"
   
  
  type TableName ChinookMySQL Album = "Album" 
  type ColumnNames ChinookMySQL Album = '[ '("albumId","AlbumId"), '("title","Title"), '("artistId","ArtistId")] 
   
  
   
  
  type ForeignKey ChinookMySQL Album = '[ 'RefBy '["artistId"] Artist '["artistId"] "fKAlbumArtistId" ] 
  type ForeignKeyNames ChinookMySQL Album = '[ '("fKAlbumArtistId","FK_AlbumArtistId")]
   
  


instance Table ChinookMySQL Genre where 
  type PrimaryKey ChinookMySQL Genre = '["genreId"]  
  type PrimaryKeyName ChinookMySQL Genre = 'Just "PK_Genre"
   
  
  type TableName ChinookMySQL Genre = "Genre" 
  type ColumnNames ChinookMySQL Genre = '[ '("genreId","GenreId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table ChinookMySQL Playlist where 
  type PrimaryKey ChinookMySQL Playlist = '["playlistId"]  
  type PrimaryKeyName ChinookMySQL Playlist = 'Just "PK_Playlist"
   
  
  type TableName ChinookMySQL Playlist = "Playlist" 
  type ColumnNames ChinookMySQL Playlist = '[ '("playlistId","PlaylistId"), '("name","Name")] 
  
instance Table ChinookMySQL Artist where 
  type PrimaryKey ChinookMySQL Artist = '["artistId"]  
  type PrimaryKeyName ChinookMySQL Artist = 'Just "PK_Artist"
   
  
  type TableName ChinookMySQL Artist = "Artist" 
  type ColumnNames ChinookMySQL Artist = '[ '("artistId","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

