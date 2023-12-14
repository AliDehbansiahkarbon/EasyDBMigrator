{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.ORM.Core;

interface

type
  Largeint = Int64;

  TDatabaseType = (dtSQLServer, dtMySQL, dtOracle, dtSQLite, dtAccess);

  TColType = (ctBigInt, ctInt, ctSmallInt, ctTinyInt, ctBit, ctDecimal, ctNumeric, ctMoney, ctSmallMoney,
              ctFloat, ctReal, ctDateTime, ctSmallDateTime, ctDate, ctTime, ctDateTimeOffset, ctDatetime2,
              ctChar, ctVarchar, ctVarcharMmax, ctText, ctNchar, ctNvarchar, ctNtext, ctBinary, ctVarbinary,
              ctImage, ctNone);

  TAlterMode = (amAdd, amDrop, amEdit, amRename, amNone);

  TTargetType = (ttSQLServer, ttMySQL, ttMariaDB, ttNone);

  TObjectType = (otDatabase, otTable, otView, otStoredProcedure, otFunction, orNone);

  TParamType = (ptIN, ptOUT, ptINOUT, ptNone);

implementation

end.
