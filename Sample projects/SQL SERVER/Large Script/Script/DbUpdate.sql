If Not Exists(
       Select *
       From   sysobjects
       Where  Name          = 'TbUsers'
              And xtype     = 'U'
   )
    Create Table TbUsers
    (
    	ID           Int Primary Key Identity(1, 1) Not Null,
    	UserName     Nvarchar(100),
    	Pass         Nvarchar(50)
    );
    Go
    
    Alter Table TbUsers Add ImageLink Varchar(500)
    Go
    
    Alter Table TbUsers Drop Column ImageLink
    Go

If Not Exists(
       Select *
       From   sysobjects
       Where  Name          = 'TbCustomers'
              And xtype     = 'U'
   )
    Create Table TbCustomers
    (
    	ID         Int Primary Key Identity(1, 1) Not Null,
    	Name       Nvarchar(100),
    	Family     Nvarchar(50)
    );
Go

    Alter Table TbCustomers Add Phone Varchar(10)
  Go  
    Alter Table TbCustomers Drop Column Phone
    Go


If Not Exists(
       Select *
       From   sysobjects
       Where  Name          = 'TbInvoices'
              And xtype     = 'U'
   )
    Create Table TbInvoices
    (
    	ID              Int Primary Key Identity(1, 1) Not Null,
    	InvoiceID       Int,
    	CustomerID      Int,
    	InvoiceDate     Datetime
    );
Go

    Alter Table TbInvoices Add TotlaAmount Decimal(10, 2)
  Go  
    Alter Table TbInvoices Drop Column TotlaAmount
    
    
