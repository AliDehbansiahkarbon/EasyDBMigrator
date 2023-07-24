# EasyDbMigrator ![EasyDBMigrator](https://github.com/AliDehbansiahkarbon/EasyDB/assets/5601608/99f201d8-8705-469c-97f3-f01e90904261)

## EasyDbMigrator is a database migration library for Delphi, built to streamline the database evolution.
## Migrations are kinda structured objects to alter your database schema and are an alternative to creating lots of SQL scripts that have to be run manually by every developer involved. 
Migrations solve the problem of evolving a database schema for multiple databases (for example, the developer's local database, the test database, and the production database). Database schema changes are described in classes written in Delphi that can be checked into a version control system.

# Supported Databases

|Name | Simple | Advanced | ORM | LargeScript Execution|
|---|---|---|---|---|
| **Microsoft SQL SERVER** | ✅ | ✅ | ✅ | ✅ |
| **MySQL** | ✅ | ✅ | ✅ | NA |
| **MariaDB** | ✅ | ✅ | ✅ | NA |
| **Oracle** | ✅ | NA | NA | NA |
| **PostgreSQL** | ✅ | NA | NA | NA |

# How it works?
It's a library, so you need to use the units in your projects, add migrations and run the migratory.

# Is it possible to use it for other databases?
Yes, you can easily integrate it with your environment, please have a look at the integration section.

# How to use it?
There are some samples that demonstrate the usage of the library with extra details but have a look at the following codes for a quick start please:

# Simple 
**Suitable** for small projects (using on-demand classes with anonymous methods).

<details>
<summary>
  🔵 SQL SERVER Sample 
</summary>

### Project name: EasyDB_Simple_SQLServer

<details>
  <summary>
Initializing
  </summary>

  ```delphi
uses
  EasyDB.Core,
  EasyDB.Migration,
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

var
  Runner: TSQLRunner;
  ConnectionParams: TConnectionParams;
begin

  with LvConnectionParams do // Could be loaded from ini, registry, or somewhere else.
  begin
    Server := '127.0.0.1'; // SQL Server address
    LoginTimeout := 30000;
    Username := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
    Schema := 'dbo'; //Optional
  end;

  {Use this line if you need a local log}
  TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog; // Logger must be configured before creating the Runner.

  {Use this line if you don't need a local log}
  // TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal).RollBackAllByAnyError(True); //each part This line is Optional
end
```
</details>  

<details>
  <summary>
Add migrations
  </summary>


```delphi

Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010001, 'Alex', 'Create table Users, Task Number #2701',
  procedure
  var sql: string;
  begin
    sql := 'If Not Exists( Select * From sysobjects Where Name = ''TbUsers'' And xtype = ''U'') ' + #10
     + '    Create Table TbUsers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	UserName Nvarchar(100), ' + #10
     + '    	Pass Nvarchar(50) ' + #10
     + '    );';

    Runner.SQLConnection.ExecuteAdHocQuery(sql);
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('DROP TABLE TbUsers');
  end
  ));

  //============================================
  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2701',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField2 VARCHAR(50)');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField2');
  end
  ));

  //============================================

  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField3 INT');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField3');
  end
  ));

  //============================================

  Runner.MigrationList.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2702',
  procedure
  var sql: string;
  begin
    sql := 'If Not Exists( Select * From sysobjects Where Name = ''TbCustomers'' And xtype = ''U'') ' + #10
     + '    Create Table TbCustomers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	Name Nvarchar(100), ' + #10
     + '    	Family Nvarchar(50) ' + #10
     + '    );';
    Runner.SQLConnection.ExecuteAdHocQuery(sql);
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('DROP TABLE TbCustomers');
  end
  ));

//...
//Add other migrations here
//...

```
  
</details>  

<details>
<summary>
  Run the Migrator
</summary>


- 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
```    
</details>
</details>
<details>
<summary>
  🟠 MySQL Sample
</summary>
  
  ### Project name: EasyDB_Simple_MySQL 
  ### It's the same as the SQL Server sample but some different units should be used, refer to the samples, please.

<details>
<summary>
  Initializing
</summary>
  
 ```delphi

uses
  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Migration,
  EasyDB.MySQLRunner;

var
  LvConnectionParams: TMySqlConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry, or somewhere else.
  begin
    Server := '127.0.0.1';
    LoginTimeout := 30000;
    Port := 3306;
    Username := 'ali';
    Pass := 'Admin123!@#';
    Schema := 'Library';
  end;

  Runner := TMySQLRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True) // Optional
    .UseInternalThread(True) //Optional
    .SetProgressbar(pbTotal); //Optional

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
```
</details>

<details>
<summary>
  Add migrations
</summary>

  ```delphi
  //Modern way
  Runner.Clear
  .Add(TUsersMgr_202301010001.Create)
  .Add(TUsersMgr_202301010002.Create)
  .Add(TUsersMgr_202301010003.Create)
  .Add(TCustomersMgr_202301010005.Create)
  .Add(TCustomersMgr_202301010010.Create)
  .Add(TInvoicesMgr_202301010005.Create)
  .Add(TInvoicesMgr_202301010010.Create);

  // Classic Way
{
  Runner.Clear;
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
}
```  
</details>  
<details>
<summary>
  Run the Migrator
</summary>
  - 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
``` 
</details>
</details>


# Advanced
### Suitable for large projects (using versioned classes with attributes).

<details>
  <summary>
   🔵 SQL SERVER Sample
  </summary>
  
### Project name: EasyDB_Advance_SQLServer

<details>
<summary>
  Initializing
</summary>

```delphi
uses
  EasyDB.Core,
  EasyDB.Migration,
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

var
  Runner: TSQLRunner;
  ConnectionParams: TConnectionParams;
begin

  with LvConnectionParams do // Could be loaded from ini, registry, or somewhere else.
  begin
    Server := '127.0.0.1'; // SQL Server address
    LoginTimeout := 30000;
    Username := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
    Schema := 'dbo'; //Optional
  end;

  {Use this line if you need a local log}
  TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog; // Logger must be configured before creating the Runner.

  {Use this line if you don't need a local log}
  // TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal).RollBackAllByAnyError(True); //each part This line is Optional
end
```  
</details>

<details>
<summary>
  Define migrations in diffrent place(unit)
</summary>
**Instead of creating some on-demand classes you can create one unit per entity and implement versioned classes like the following code:**

```delphi
uses
  EasyDB.Core,
  EasyDB.ConnectionManager.SQL,
  EasyDB.MigrationX,
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

type

  [TCustomMigrationAttribute('TbUsers', 202301010001, 'Created users table', 'Alex')]
  TUsersMgr_202301010001 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbUsers', 202301010002, 'Added newfielad1', 'Alex')]
  TUsersMgr_202301010002 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbUsers', 202301010003, 'Added newfielad2', 'Alex')]
  TUsersMgr_202301010003 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TUsersMgr_202301010001 }
procedure TUsersMgr_202301010001.Downgrade;
begin
  try
    SQL.ExecuteAdHocQuery('Drop Table TbUsers');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010001.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'If Not Exists( Select * From sysobjects Where Name = ''TbUsers'' And xtype = ''U'') ' + #10
            + ' Create Table TbUsers( ' + #10
            + ' ID Int Primary key Identity(1, 1) Not null, ' + #10
            + ' UserName Nvarchar(100), ' + #10
            + ' Pass Nvarchar(50) ' + #10
            + ' );';
  try
    SQL.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010002 }
procedure TUsersMgr_202301010002.Downgrade;
var
  LvScript: string;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbUsers Drop Column CreatedDate');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010002.Upgrade;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbUsers Add CreatedDate Datetime');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010003 }

procedure TUsersMgr_202301010003.Downgrade;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbUsers Drop Column ImageLink');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010003.Upgrade;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbUsers Add ImageLink Varchar(500)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;
```
  
</details>

<details>
<summary>
 Add migrations
</summary>
  
  ```delphi
  //Modern way
  Runner.Clear
  .Add(TUsersMgr_202301010001.Create)
  .Add(TUsersMgr_202301010002.Create)
  .Add(TUsersMgr_202301010003.Create)
  .Add(TCustomersMgr_202301010005.Create)
  .Add(TCustomersMgr_202301010010.Create)
  .Add(TInvoicesMgr_202301010005.Create)
  .Add(TInvoicesMgr_202301010010.Create);

  // Classic Way
{
  Runner.Clear;
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
}
```
</details>

<details>
<summary>
 Run the Migrator exactly like the simple mode.
</summary>
  
- 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
```
</details>
</details>

<details>
  <summary>
   🟠 MySQL Sample
  </summary>
  
### Project name: EasyDB_Advance_MySQL
### It's the same as the SQL Server sample but different units should be used.

<details>
  <summary>
   Initializing
  </summary>

   
```delphi
  uses
    EasyDB.Core,
    EasyDB.ConnectionManager.MySQL,
    EasyDB.MigrationX,
    EasyDB.MySQLRunner,
    EasyDB.Logger;


var
  LvConnectionParams: TMySqlConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '127.0.0.1';
    LoginTimeout := 30000;
    Port := 3306;
    UserName := 'ali';
    Pass := 'Admin123!@#';
    Schema := 'Library';
  end;

  Runner := TmySQLRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True) // Optional
    .UseInternalThread(True) //Optional
    .SetProgressbar(pbTotal); //Optional

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
 ```

</details>

<details>
  <summary>
   Add migrations
  </summary>

  ```delphi
  //Modern way
  Runner.Clear
  .Add(TUsersMgr_202301010001.Create)
  .Add(TUsersMgr_202301010002.Create)
  .Add(TUsersMgr_202301010003.Create)
  .Add(TCustomersMgr_202301010005.Create)
  .Add(TCustomersMgr_202301010010.Create)
  .Add(TInvoicesMgr_202301010005.Create)
  .Add(TInvoicesMgr_202301010010.Create);

  // Classic Way
{
  Runner.Clear;
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
}
```
</details>
  
<details>
  <summary>
   Run the Migrator
  </summary>

  - 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
```  
</details>  
</details>

# ORM
### There is a **mini ORM** underlying in this library which can help you to make your database upgrades/downgrades more modern and more readable.
<details>
  <summary>
   🔵 SQL SERVER Sample
  </summary>

<details>
  <summary>
   Initializing + Database creation
  </summary>

  ```delphi
var
  LvConnectionParams: TSqlConnectionParams;
  LvRunner: TSQLRunner;
  ORM: TORM;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Master';
    Schema := 'dbo';
  end;

  LvRunner := TSQLRunner.Create(LvConnectionParams);
  LvRunner.Config
    .LogAllExecutions(True)// Optional
    .UseInternalThread(False)// Better to run with a single thread(Only for Database creation)
    .SetProgressbar(pbTotal)// Optional
    .DelayedExecution(500);// Optional

  LvRunner.AddLogger.OnLog := OnLog;

  try
    LvRunner.Clear;
    ORM := TORM.GetInstance(ttSQLServer);
    LvRunner.ORM := ORM;

    LvRunner.Add(TMigration.Create('Library DB', 202301010000, 'GodAdmin!', 'Created the Database',
    procedure
    begin
      with ORM do
      begin
        Create.Database('Library')
        .MdfFileName('C:\Program Files\Microsoft SQL Server\MSSQL15.MSSQLSERVER\MSSQL\DATA\Library.mdf')
        .MdfSize('8192KB')
        .MdfFileGrowth('65536KB')
        .MdfMaxSize('UNLIMITED')
        .LdfFileName('C:\Program Files\Microsoft SQL Server\MSSQL15.MSSQLSERVER\MSSQL\DATA\Library.ldf')
        .LdfSize('8192KB')
        .LdfFileGrowth('65536KB')
        .LdfMaxSize('2048GB')
        .Collation('Latin1_General_CI_AS');

        SubmitChanges;
      end;
    end,
    procedure
    begin
      ORM.Delete.Database('Library');
      ORM.SubmitChanges;
    end
    ));

    LvRunner.UpgradeDatabase;
  finally
    LvRunner.Free;
  end;
end;
```
</details>  

<details>
  <summary>
   Add migrations
  </summary>

```delphi
var
  ORM: TORM;
begin
  InitializeRunner;
  TLogger.Instance.Log(atUpgrade, '');

  Runner.Clear;
  ORM := TORM.GetInstance(ttSQLServer);
  Runner.ORM := ORM;

  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Created table Users(#2701)',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbUsers').WithIdColumn
      .WithColumn('UserName').AsNvarchar(100).Nullable
      .WithColumn('Pass').AsNvarchar(50).Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbUsers');
    ORM.SubmitChanges;
  end
  ));
  Exit;
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Added NewField2 to table Tbusers(#2702)',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField2').AsVarchar(50).Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField2');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Added NewField3 to table Tbusers(#2703)',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField3').AsInt.Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField3');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Created Table TbCustomers and table TbInvoices(#2703)',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbCustomers')
      .WithColumn('Name').AsNvarchar(100).Nullable
      .WithColumn('Family').AsNvarchar(50).Nullable;

      Create.Table('TbInvoices').WithIdColumn
      .WithColumn('InvoiceNumber').AsBigInt.Nullable
      .WithColumn('InvoiceDate').AsDateTime.Nullable
      .WithColumn('MarketCode').AsInt.Nullable
      .WithColumn('TotalAmount').AsMoney.Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbCustomers');
    ORM.Delete.Table('TbInvoices');
    ORM.SubmitChanges;
  end
  ));

  //============================================
  Runner.Add(TMigration.Create('SelectTopTenCustomers', 202301010004, 'Alexander', 'Added SP and function(Task number #2704)',
  procedure
  var LvBody: string;
  begin
    with ORM do
    begin
      LvBody := 'Select * from TbInvoices where TotalAmount > @TotalAmount and MarketCode = @MarketCode and InvoiceDate = @ReportData';

      Create.StoredProc('SelectTopTenCustomers')
      .AddParam('TotalAmount', TDataType.Create(ctMoney))
      .AddParam('ReportData', TDataType.Create(ctDateTime))
      .AddParam('MarketCode', TDataType.Create(ctInt))
      .AddBody(LvBody);


      LvBody := 'Declare @Result Money '+ #10 +
                'Select @Result = Sum(TotalAmount) From TbInvoices where InvoiceDate <= @ReportData' + #10 +
                'Return @Result';

      Create.StoredFunction('GetTotalSum')
      .AddParam('ReportData', TDataType.Create(ctDateTime))
      .ReturnType(TDataType.Create(ctMoney))
      .AddBody(LvBody);

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.StoredProc('SelectTopTenCustomers');
    ORM.Delete.StoredFunc('GetTotalSum');
    ORM.SubmitChanges;
  end
  ));
end;
```  
</details>  

<details>
  <summary>
   Run the Migrator
  </summary>

  - 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
``` 

</details>
</details>

<details>
  <summary>
   🟠 MySQL Sample
  </summary>

<details>
  <summary>
   Initializing
  </summary>

  ```delphi
var
  LvConnectionParams: TMySqlConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '127.0.0.1';
    LoginTimeout := 30000;
    Port := 3306;
    UserName := 'ali';
    Pass := 'Admin123!@#';
    Schema := 'Library';
  end;

  Runner := TMySQLRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True) // Optional
    .UseInternalThread(True) //Optional
    .SetProgressbar(pbTotal) //Optional
    .DelayedExecution(500);

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
```
</details>

<details>
  <summary>
   Add migrations
  </summary>

  ```delphi
var
  ORM: TORM;
begin
  Runner.Clear;
  ORM := TORM.GetInstance(ttMySQL);
  Runner.ORM := ORM;

  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Created table Users(#2701)',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbUsers').WithIdColumn
        .WithColumn('UserName').AsNvarchar(100).Nullable
        .WithColumn('Pass').AsNvarchar(50).Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbUsers');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Added NewField2 to table Tbusers(#2702)',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField2').AsVarchar(50).Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField2');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Added NewField3 to table Tbusers(#2703)',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField3').AsInt.Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField3');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Created Table TbCustomers and table TbInvoices(#2703)',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbCustomers')
        .WithColumn('Name').AsNvarchar(100).Nullable
        .WithColumn('Family').AsNvarchar(50).Nullable;

      Create.Table('TbInvoices').WithIdColumn
        .WithColumn('InvoiceNumber').AsBigInt.Nullable
        .WithColumn('InvoiceDate').AsDateTime.Nullable
        .WithColumn('MarketCode').AsInt.Nullable
        .WithColumn('TotalAmount').AsMoney.Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbCustomers');
    ORM.Delete.Table('TbInvoices');
    ORM.SubmitChanges;
  end
  ));

  //============================================
  Runner.Add(TMigration.Create('SelectTopTenCustomers', 202301010004, 'Alexander', 'Added SP and function(Task number #2704)',
  procedure
  var LvBody: string;
  begin
    with ORM do
    begin
      LvBody := 'Select * from TbInvoices where TotalAmount > @TotalAmount and MarketCode = @MarketCode and InvoiceDate = @ReportData;';

      Create.StoredProc('SelectTopTenCustomers')
        .AddParam('TotalAmount', TDataType.Create(ctMoney))
        .AddParam('ReportData', TDataType.Create(ctDateTime))
        .AddParam('MarketCode', TDataType.Create(ctInt))
        .AddBody(LvBody);


      LvBody := 'Declare Result DECIMAL(10, 2); '+ #10 +
                'Select Sum(TotalAmount) Into Result From TbInvoices where InvoiceDate <= @ReportData;' + #10 +
                'Return Result;';

      Create.StoredFunction('GetTotalSum')
        .AddParam('ReportData', TDataType.Create(ctDateTime))
        .ReturnType(TDataType.Create(ctDecimal, 10, 2), True)
        .AddBody(LvBody);

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.StoredProc('SelectTopTenCustomers');
    ORM.Delete.StoredFunc('GetTotalSum');
    ORM.SubmitChanges;
  end
  ));
end;
```
</details>

<details>
  <summary>
   Run the Migrator
  </summary>

  - 🟩 Upgrade the database to the latest version
```delphi
  Runner.UpgradeDatabase; // Do upgrade
```
- 🟩 Downgrade the database to the latest version
```delphi
  Runner.DowngradeDatabase(202301010001); // Do downgrade to a specific version.
  //This version and lower versions of the database will remain and any version above this will be restored.
``` 

</details>  
</details>


# Large Script execution - SQL Server Only

<details>
  <summary>
   🚩Note
  </summary>
 
### To execute large scripts with this library you must separate each statement with the SQL Server-specific keyword "GO", this is mandatory!
### If you already have an extensive script that's not a problem you can keep it as it is and continue with this library from now on.
### To execute the existing script with any size refer to the related sample project(EasyDB_LargeScript_SQLServer).
</details>

<details>
  <summary>
   🔵 SQL SERVER Sample
  </summary>


## Initiate and run your script easily!  
```delphi
var
  LvConnectionParams: TSqlConnectionParams;
begin
  pbTotal.Style := pbstMarquee;

  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'AdventureWorks2019';
    Schema := 'dbo';
  end;

  TLogger.Instance.OnLog := OnLog;
  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.Config.UseInternalThread(True).LogAllExecutions(chkLogExecutions.Checked);
  Runner.SQL.ExecuteScriptFile('..\..\Script\AdventureWorks2019_Minimal.sql');
end;
```
  

![image](https://github.com/AliDehbansiahkarbon/EasyDB/assets/5601608/2d5b9113-8a9f-46e6-8c5d-2073bacfeac5)
</details>

# Last but not least!
### OnLog() - a very useful callback event.

There is an internal simple logger inside the library that is able to write log data in a text file but it has a beneficial event that will fire with each logging activity.
Using his event you can use your desired logging method like [QuickLogger](https://github.com/exilon/QuickLogger) or anything else and target any destination 
like [Graylog](https://www.graylog.org), [Betterstack](https://betterstack.com), Telegram, Email, etc...

---

# New Integration instruction
## Didn't find your database?
**No problem!** you can easily integrate this library with your environment, folowwing bellow steps:
- Create a new unit in this path ..\EasyDB\Lib\ConnectionManagers including a class that is inherited 
  from TConnection in the unit EasyDB.ConnectionManager.Base.pas, see the EasyDB.ConnectionManager.SQL.pas unit as a template.
- Create a new unit in this path ..\EasyDB\Lib\Runners including a class that is inherited from
  TRunner in the unit EasyDB.Runner.pas, see the EasyDB.MSSQLRunner.pas unit as a template.
- The two above steps are enough to use in simple and advanced modes but in case you prefer to use ORM form you need to create 
  a new function in the TBuilder class to generate scripts based on your target database, this class can be found
  in the path ..\EasyDB\Lib\ORM and unit EasyDB.ORM.Builder.pas.
