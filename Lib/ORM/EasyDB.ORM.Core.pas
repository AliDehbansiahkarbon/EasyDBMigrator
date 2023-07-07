unit EasyDB.ORM.Core;

interface

type
  Largeint = Int64;

  TColType = (ctBigInt, ctInt, ctSmallInt, ctTinyInt, ctBit, ctDecimal, ctNumeric, ctMoney, ctSmallMoney,
              ctFloat, ctReal, ctDateTime, ctSmallDateTime, ctDate, ctTime, ctDateTimeOffset, ctDatetime2,
              ctChar, ctVarchar, ctVarcharMmax, ctText, ctNchar, ctNvarchar, ctNtext, ctBinary, ctVarbinary,
              ctImage, ctNone);

  TAlterMode = (amAdd, amDrop, amEdit, amNone);

  TTargetType = (ttSQLServer, ttMySQL, ttNone);

//  TBaseTable = class
//  end;
implementation

end.
