# EasyDbMigrator

EasyDb Migrator is a migration library for Delphi similar to Fluent migrator for .Net
Migrations are a structured way to alter your database schema and are an alternative to creating lots of SQL scripts that have to be run manually by every developer involved. Migrations solve the problem of evolving a database schema for multiple databases (for example, the developer's local database, the test database, and the production database). Database schema changes are described in classes written in Delphi that can be checked into a version control system.

# Prerequisites

| Tool/Library          | Consequences when not installed | Is it included in the source? | Where to find? |
|-----------------------|---------------------------------|-------------------------------|----------------|
| BufferedStreamReader  | You can not run large scripts   | Yes.                          | https://github.com/lordcrc/BufferedStreamReader


# How it works?
It's a library, so you just need to use the units in your projects, add migrations and run the migrator.

# How to use it?
There are two samples that demonstrate the usage with extra details but have a look at the following codes for a quick start:


# $\textcolor{Cyan}{Simple\ way\ (using\ on-demand\ classes\ with\ anonymous\ methods)}$

<details>
<summary>
  🟥 SQL SERVER Sample   
</summary>

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
  //This version and lower versions of database will remain and any version above this will be restored.
```  
</details>

<details>
<summary>
  🟦 MySQL Sample   
</summary>
</details>

# $\textcolor{Cyan}{Advanced\ way\ (using\ versioned\ classes\ with\ attributes)}$

<details>
  <summary>
   🟥 SQL SERVER Sample 
  </summary>
</details>

<details>
  <summary>
   🟦 MySQL Sample   
  </summary>
</details>
