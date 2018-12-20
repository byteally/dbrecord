
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



import Models.Mediatype
import Models.Track
import Models.Employee
import Models.Playlisttrack
import Models.Invoice
import Models.Customer
import Models.Invoiceline
import Models.Album
import Models.Genre
import Models.Playlist
import Models.Artist


data Chinook

instance Database Chinook where
  type DB     Chinook = 'Postgres
  type Tables Chinook = '[ Mediatype, Track, Employee, Playlisttrack, Invoice, Customer, Invoiceline, Album, Genre, Playlist, Artist]
  type Types Chinook = '[ ]

instance Table Chinook Mediatype where 
  type PrimaryKey Chinook Mediatype = '["mediatypeid"]  
  type PrimaryKeyName Chinook Mediatype = 'Just "PK_MediaType"
   
  
  type TableName Chinook Mediatype = "MediaType" 
  type ColumnNames Chinook Mediatype = '[ '("mediatypeid","MediaTypeId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table Chinook Track where 
  type PrimaryKey Chinook Track = '["trackid"]  
  type PrimaryKeyName Chinook Track = 'Just "PK_Track"
   
  
  type TableName Chinook Track = "Track" 
  type ColumnNames Chinook Track = '[ '("trackid","TrackId"), '("name","Name"), '("albumid","AlbumId"), '("mediatypeid","MediaTypeId"), '("genreid","GenreId"), '("composer","Composer"), '("milliseconds","Milliseconds"), '("bytes","Bytes"), '("unitprice","UnitPrice")] 
   
  
   
  
  type ForeignKey Chinook Track = '[ 'RefBy '["mediatypeid"] Mediatype '["mediatypeid"] "fktrackmediatypeid" , 'RefBy '["genreid"] Genre '["genreid"] "fktrackgenreid" , 'RefBy '["albumid"] Album '["albumid"] "fktrackalbumid" ] 
  type ForeignKeyNames Chinook Track = '[ '("fktrackmediatypeid","FK_TrackMediaTypeId"), '("fktrackgenreid","FK_TrackGenreId"), '("fktrackalbumid","FK_TrackAlbumId")]
   
  


instance Table Chinook Employee where 
  type PrimaryKey Chinook Employee = '["employeeid"]  
  type PrimaryKeyName Chinook Employee = 'Just "PK_Employee"
   
  
  type TableName Chinook Employee = "Employee" 
  type ColumnNames Chinook Employee = '[ '("employeeid","EmployeeId"), '("lastname","LastName"), '("firstname","FirstName"), '("title","Title"), '("reportsto","ReportsTo"), '("birthdate","BirthDate"), '("hiredate","HireDate"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalcode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email")] 
   
  
   
  
  type ForeignKey Chinook Employee = '[ 'RefBy '["reportsto"] Employee '["employeeid"] "fkemployeereportsto" ] 
  type ForeignKeyNames Chinook Employee = '[ '("fkemployeereportsto","FK_EmployeeReportsTo")]
   
  


instance Table Chinook Playlisttrack where 
  type PrimaryKey Chinook Playlisttrack = '["playlistid","trackid"]  
  type PrimaryKeyName Chinook Playlisttrack = 'Just "PK_PlaylistTrack"
   
  
  type TableName Chinook Playlisttrack = "PlaylistTrack" 
  type ColumnNames Chinook Playlisttrack = '[ '("playlistid","PlaylistId"), '("trackid","TrackId")] 
   
  
   
  
  type ForeignKey Chinook Playlisttrack = '[ 'RefBy '["trackid"] Track '["trackid"] "fkplaylisttracktrackid" , 'RefBy '["playlistid"] Playlist '["playlistid"] "fkplaylisttrackplaylistid" ] 
  type ForeignKeyNames Chinook Playlisttrack = '[ '("fkplaylisttracktrackid","FK_PlaylistTrackTrackId"), '("fkplaylisttrackplaylistid","FK_PlaylistTrackPlaylistId")]
   
  


instance Table Chinook Invoice where 
  type PrimaryKey Chinook Invoice = '["invoiceid"]  
  type PrimaryKeyName Chinook Invoice = 'Just "PK_Invoice"
   
  
  type TableName Chinook Invoice = "Invoice" 
  type ColumnNames Chinook Invoice = '[ '("invoiceid","InvoiceId"), '("customerid","CustomerId"), '("invoicedate","InvoiceDate"), '("billingaddress","BillingAddress"), '("billingcity","BillingCity"), '("billingstate","BillingState"), '("billingcountry","BillingCountry"), '("billingpostalcode","BillingPostalCode"), '("total","Total")] 
   
  
   
  
  type ForeignKey Chinook Invoice = '[ 'RefBy '["customerid"] Customer '["customerid"] "fkinvoicecustomerid" ] 
  type ForeignKeyNames Chinook Invoice = '[ '("fkinvoicecustomerid","FK_InvoiceCustomerId")]
   
  


instance Table Chinook Customer where 
  type PrimaryKey Chinook Customer = '["customerid"]  
  type PrimaryKeyName Chinook Customer = 'Just "PK_Customer"
   
  
  type TableName Chinook Customer = "Customer" 
  type ColumnNames Chinook Customer = '[ '("customerid","CustomerId"), '("firstname","FirstName"), '("lastname","LastName"), '("company","Company"), '("address","Address"), '("city","City"), '("state","State"), '("country","Country"), '("postalcode","PostalCode"), '("phone","Phone"), '("fax","Fax"), '("email","Email"), '("supportrepid","SupportRepId")] 
   
  
   
  
  type ForeignKey Chinook Customer = '[ 'RefBy '["supportrepid"] Employee '["employeeid"] "fkcustomersupportrepid" ] 
  type ForeignKeyNames Chinook Customer = '[ '("fkcustomersupportrepid","FK_CustomerSupportRepId")]
   
  


instance Table Chinook Invoiceline where 
  type PrimaryKey Chinook Invoiceline = '["invoicelineid"]  
  type PrimaryKeyName Chinook Invoiceline = 'Just "PK_InvoiceLine"
   
  
  type TableName Chinook Invoiceline = "InvoiceLine" 
  type ColumnNames Chinook Invoiceline = '[ '("invoicelineid","InvoiceLineId"), '("invoiceid","InvoiceId"), '("trackid","TrackId"), '("unitprice","UnitPrice"), '("quantity","Quantity")] 
   
  
   
  
  type ForeignKey Chinook Invoiceline = '[ 'RefBy '["trackid"] Track '["trackid"] "fkinvoicelinetrackid" , 'RefBy '["invoiceid"] Invoice '["invoiceid"] "fkinvoicelineinvoiceid" ] 
  type ForeignKeyNames Chinook Invoiceline = '[ '("fkinvoicelinetrackid","FK_InvoiceLineTrackId"), '("fkinvoicelineinvoiceid","FK_InvoiceLineInvoiceId")]
   
  


instance Table Chinook Album where 
  type PrimaryKey Chinook Album = '["albumid"]  
  type PrimaryKeyName Chinook Album = 'Just "PK_Album"
   
  
  type TableName Chinook Album = "Album" 
  type ColumnNames Chinook Album = '[ '("albumid","AlbumId"), '("title","Title"), '("artistid","ArtistId")] 
   
  
   
  
  type ForeignKey Chinook Album = '[ 'RefBy '["artistid"] Artist '["artistid"] "fkalbumartistid" ] 
  type ForeignKeyNames Chinook Album = '[ '("fkalbumartistid","FK_AlbumArtistId")]
   
  


instance Table Chinook Genre where 
  type PrimaryKey Chinook Genre = '["genreid"]  
  type PrimaryKeyName Chinook Genre = 'Just "PK_Genre"
   
  
  type TableName Chinook Genre = "Genre" 
  type ColumnNames Chinook Genre = '[ '("genreid","GenreId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table Chinook Playlist where 
  type PrimaryKey Chinook Playlist = '["playlistid"]  
  type PrimaryKeyName Chinook Playlist = 'Just "PK_Playlist"
   
  
  type TableName Chinook Playlist = "Playlist" 
  type ColumnNames Chinook Playlist = '[ '("playlistid","PlaylistId"), '("name","Name")] 
   
  
   
  
   
  
   
  


instance Table Chinook Artist where 
  type PrimaryKey Chinook Artist = '["artistid"]  
  type PrimaryKeyName Chinook Artist = 'Just "PK_Artist"
   
  
  type TableName Chinook Artist = "Artist" 
  type ColumnNames Chinook Artist = '[ '("artistid","ArtistId"), '("name","Name")] 
   
  
   
  
   
  
   
  

