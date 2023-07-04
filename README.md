# $\textcolor{Cyan}{EasyDbMigrator}$

## EasyDb Migrator is a database migration library for Delphi, built to streamline database evolution.
## Migrations are a structured way to alter your database schema and are an alternative to creating lots of SQL scripts that have to be run manually by every developer involved. Migrations solve the problem of evolving a database schema for multiple databases (for example, the developer's local database, the test database, and the production database). Database schema changes are described in classes written in Delphi that can be checked into a version control system.

# $\textcolor{Cyan}{How\ it\ works?}$
It's a library, so you just need to use the units in your projects, add migrations and run the migratory.

# $\textcolor{Cyan}{How\ to\ use\ it?}$
There are two samples that demonstrate the usage with extra details but have a look at the following codes for a quick start:

# $\textcolor{Cyan}{Simple\ way\ (using\ on-demand\ classes\ with\ anonymous\ methods)}$

<details>
<summary>
  🟥 SQL SERVER Sample 
</summary>

### Sample project name : EasyDBMigration_Simple_SQLServer

### Initializing
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

### Add migrations
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

### Run the Migrator
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

<details>
<summary>
  🟦 MySQL Sample
</summary>
  
  ### Sample project name : EasyDBMigration_Simple_MySQL 
  
  ### It's the same as the SQL Server sample but some different units should be used.
 
```delphi
  uses
    EasyDB.Core,
    EasyDB.Logger,
    EasyDB.Migration,
    EasyDB.MySQLRunner;
 ```
 ### *** same initialization, same run method. (see the sample project, EasyDBMigration_Simple_MySQL) ***
 
</details>

# $\textcolor{Cyan}{Advanced\ way\ (using\ versioned\ classes\ with\ attributes)}$

<details>
  <summary>
   🟥 SQL SERVER Sample
  </summary>
  
### Sample project name : EasyDBMigration_Advance_SQLServer

### Initializing is the exactly same as simple mode.

### Define migrations
Instead of creating some on-demand classes you can create one unit per entity and implement versioned classes like the following code:
```delphi
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

### Add migrations
```delphi
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
```

### Run the Migrator exactly like the simple mode.
```delphi
  Runner.UpgradeDatabase; // Do upgrade
  // or
  Runner.DowngradeDatabase(202301010001);
```
</details>

<details>
  <summary>
   🟦 MySQL Sample
  </summary>
  
### Sample project name : EasyDBMigration_Advance_MySQL
  
### It's the same as the SQL Server sample but some different units should be used.
 
```delphi
  uses
    EasyDB.Core,
    EasyDB.ConnectionManager.MySQL,
    EasyDB.MigrationX, // Do not use "EasyDB.Migration.Base" here if you are going to use class-level Attributes.
    EasyDB.MySQLRunner,
    EasyDB.Logger;
 ```

 ### *** same initialization, same run method. (see the sample project, EasyDBMigration_Advance_MySQL) ***
</details>
