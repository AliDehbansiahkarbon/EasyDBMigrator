/****** Object:  Schema [HumanResources]    Script Date: 7/3/2023 18:06:41 ******/
CREATE SCHEMA [HumanResources]
GO
/****** Object:  Schema [Person]    Script Date: 7/3/2023 18:06:41 ******/
CREATE SCHEMA [Person]
GO
/****** Object:  Schema [Production]    Script Date: 7/3/2023 18:06:41 ******/
CREATE SCHEMA [Production]
GO
/****** Object:  Schema [Purchasing]    Script Date: 7/3/2023 18:06:41 ******/
CREATE SCHEMA [Purchasing]
GO
/****** Object:  Schema [Sales]    Script Date: 7/3/2023 18:06:41 ******/
CREATE SCHEMA [Sales]
GO
/****** Object:  XmlSchemaCollection [HumanResources].[HRResumeSchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [HumanResources].[HRResumeSchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume" elementFormDefault="qualified"><xsd:element name="Address" type="t:AddressType" /><xsd:element name="Education" type="t:EducationType" /><xsd:element name="Employment" type="t:EmploymentType" /><xsd:element name="Location" type="t:LocationType" /><xsd:element name="Name" type="t:NameType" /><xsd:element name="Resume" type="t:ResumeType" /><xsd:element name="Telephone" type="t:TelephoneType" /><xsd:complexType name="AddressType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Addr.Type" type="xsd:string" /><xsd:element name="Addr.OrgName" type="xsd:string" minOccurs="0" /><xsd:element name="Addr.Street" type="xsd:string" maxOccurs="unbounded" /><xsd:element name="Addr.Location"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Location" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element><xsd:element name="Addr.PostalCode" type="xsd:string" /><xsd:element name="Addr.Telephone" minOccurs="0"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Telephone" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="EducationType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Edu.Level" type="xsd:string" /><xsd:element name="Edu.StartDate" type="xsd:date" /><xsd:element name="Edu.EndDate" type="xsd:date" /><xsd:element name="Edu.Degree" type="xsd:string" minOccurs="0" /><xsd:element name="Edu.Major" type="xsd:string" minOccurs="0" /><xsd:element name="Edu.Minor" type="xsd:string" minOccurs="0" /><xsd:element name="Edu.GPA" type="xsd:string" minOccurs="0" /><xsd:element name="Edu.GPAAlternate" type="xsd:decimal" minOccurs="0" /><xsd:element name="Edu.GPAScale" type="xsd:decimal" minOccurs="0" /><xsd:element name="Edu.School" type="xsd:string" minOccurs="0" /><xsd:element name="Edu.Location" minOccurs="0"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Location" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="EmploymentType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Emp.StartDate" type="xsd:date" minOccurs="0" /><xsd:element name="Emp.EndDate" type="xsd:date" minOccurs="0" /><xsd:element name="Emp.OrgName" type="xsd:string" /><xsd:element name="Emp.JobTitle" type="xsd:string" /><xsd:element name="Emp.Responsibility" type="xsd:string" /><xsd:element name="Emp.FunctionCategory" type="xsd:string" minOccurs="0" /><xsd:element name="Emp.IndustryCategory" type="xsd:string" minOccurs="0" /><xsd:element name="Emp.Location" minOccurs="0"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Location" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="LocationType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Loc.CountryRegion" type="xsd:string" /><xsd:element name="Loc.State" type="xsd:string" minOccurs="0" /><xsd:element name="Loc.City" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="NameType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Name.Prefix" type="xsd:string" minOccurs="0" /><xsd:element name="Name.First" type="xsd:string" /><xsd:element name="Name.Middle" type="xsd:string" minOccurs="0" /><xsd:element name="Name.Last" type="xsd:string" /><xsd:element name="Name.Suffix" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="ResumeType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Name" /><xsd:element name="Skills" type="xsd:string" minOccurs="0" /><xsd:element ref="t:Employment" maxOccurs="unbounded" /><xsd:element ref="t:Education" maxOccurs="unbounded" /><xsd:element ref="t:Address" maxOccurs="unbounded" /><xsd:element ref="t:Telephone" minOccurs="0" /><xsd:element name="EMail" type="xsd:string" minOccurs="0" /><xsd:element name="WebSite" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="TelephoneType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Tel.Type" type="xsd:anyType" minOccurs="0" /><xsd:element name="Tel.IntlCode" type="xsd:int" minOccurs="0" /><xsd:element name="Tel.AreaCode" type="xsd:int" minOccurs="0" /><xsd:element name="Tel.Number" type="xsd:string" /><xsd:element name="Tel.Extension" type="xsd:int" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:schema>'
GO
/****** Object:  XmlSchemaCollection [Person].[AdditionalContactInfoSchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [Person].[AdditionalContactInfoSchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"><xsd:element name="AdditionalContactInfo"><xsd:complexType mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:any namespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactRecord http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:schema><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactRecord" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactRecord"><xsd:element name="ContactRecord"><xsd:complexType mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:choice minOccurs="0" maxOccurs="unbounded"><xsd:any namespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes" /></xsd:choice><xsd:attribute name="date" type="xsd:date" /></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:schema><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes" elementFormDefault="qualified"><xsd:element name="eMail" type="t:eMailType" /><xsd:element name="facsimileTelephoneNumber" type="t:phoneNumberType" /><xsd:element name="homePostalAddress" type="t:addressType" /><xsd:element name="internationaliSDNNumber" type="t:phoneNumberType" /><xsd:element name="mobile" type="t:phoneNumberType" /><xsd:element name="pager" type="t:phoneNumberType" /><xsd:element name="physicalDeliveryOfficeName" type="t:addressType" /><xsd:element name="registeredAddress" type="t:addressType" /><xsd:element name="telephoneNumber" type="t:phoneNumberType" /><xsd:element name="telexNumber" type="t:phoneNumberType" /><xsd:complexType name="addressType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Street" type="xsd:string" maxOccurs="2" /><xsd:element name="City" type="xsd:string" /><xsd:element name="StateProvince" type="xsd:string" /><xsd:element name="PostalCode" type="xsd:string" minOccurs="0" /><xsd:element name="CountryRegion" type="xsd:string" /><xsd:element name="SpecialInstructions" type="t:specialInstructionsType" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="eMailType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="eMailAddress" type="xsd:string" /><xsd:element name="SpecialInstructions" type="t:specialInstructionsType" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="phoneNumberType"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="number"><xsd:simpleType><xsd:restriction base="xsd:string"><xsd:pattern value="[0-9\(\)\-]*" /></xsd:restriction></xsd:simpleType></xsd:element><xsd:element name="SpecialInstructions" type="t:specialInstructionsType" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="specialInstructionsType" mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:any namespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:schema>'
GO
/****** Object:  XmlSchemaCollection [Person].[IndividualSurveySchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [Person].[IndividualSurveySchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey" elementFormDefault="qualified"><xsd:element name="IndividualSurvey"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="TotalPurchaseYTD" type="xsd:decimal" minOccurs="0" /><xsd:element name="DateFirstPurchase" type="xsd:date" minOccurs="0" /><xsd:element name="BirthDate" type="xsd:date" minOccurs="0" /><xsd:element name="MaritalStatus" type="xsd:string" minOccurs="0" /><xsd:element name="YearlyIncome" type="t:SalaryType" minOccurs="0" /><xsd:element name="Gender" type="xsd:string" minOccurs="0" /><xsd:element name="TotalChildren" type="xsd:int" minOccurs="0" /><xsd:element name="NumberChildrenAtHome" type="xsd:int" minOccurs="0" /><xsd:element name="Education" type="xsd:string" minOccurs="0" /><xsd:element name="Occupation" type="xsd:string" minOccurs="0" /><xsd:element name="HomeOwnerFlag" type="xsd:string" minOccurs="0" /><xsd:element name="NumberCarsOwned" type="xsd:int" minOccurs="0" /><xsd:element name="Hobby" type="xsd:string" minOccurs="0" maxOccurs="unbounded" /><xsd:element name="CommuteDistance" type="t:MileRangeType" minOccurs="0" /><xsd:element name="Comments" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element><xsd:simpleType name="MileRangeType"><xsd:restriction base="xsd:string"><xsd:enumeration value="0-1 Miles" /><xsd:enumeration value="1-2 Miles" /><xsd:enumeration value="2-5 Miles" /><xsd:enumeration value="5-10 Miles" /><xsd:enumeration value="10+ Miles" /></xsd:restriction></xsd:simpleType><xsd:simpleType name="SalaryType"><xsd:restriction base="xsd:string"><xsd:enumeration value="0-25000" /><xsd:enumeration value="25001-50000" /><xsd:enumeration value="50001-75000" /><xsd:enumeration value="75001-100000" /><xsd:enumeration value="greater than 100000" /></xsd:restriction></xsd:simpleType></xsd:schema>'
GO
/****** Object:  XmlSchemaCollection [Production].[ManuInstructionsSchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [Production].[ManuInstructionsSchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelManuInstructions" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelManuInstructions" elementFormDefault="qualified"><xsd:element name="root"><xsd:complexType mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Location" maxOccurs="unbounded"><xsd:complexType mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="step" type="t:StepType" maxOccurs="unbounded" /></xsd:sequence><xsd:attribute name="LocationID" type="xsd:integer" use="required" /><xsd:attribute name="SetupHours" type="xsd:decimal" /><xsd:attribute name="MachineHours" type="xsd:decimal" /><xsd:attribute name="LaborHours" type="xsd:decimal" /><xsd:attribute name="LotSize" type="xsd:decimal" /></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element><xsd:complexType name="StepType" mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:choice minOccurs="0" maxOccurs="unbounded"><xsd:element name="tool" type="xsd:string" /><xsd:element name="material" type="xsd:string" /><xsd:element name="blueprint" type="xsd:string" /><xsd:element name="specs" type="xsd:string" /><xsd:element name="diag" type="xsd:string" /></xsd:choice></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:schema>'
GO
/****** Object:  XmlSchemaCollection [Production].[ProductDescriptionSchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [Production].[ProductDescriptionSchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain" elementFormDefault="qualified"><xsd:element name="Maintenance"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="NoOfYears" type="xsd:string" /><xsd:element name="Description" type="xsd:string" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element><xsd:element name="Warranty"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="WarrantyPeriod" type="xsd:string" /><xsd:element name="Description" type="xsd:string" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element></xsd:schema><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:ns1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription" elementFormDefault="qualified"><xsd:import namespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain" /><xsd:element name="Code" type="xsd:string" /><xsd:element name="Description" type="xsd:string" /><xsd:element name="ProductDescription" type="t:ProductDescription" /><xsd:element name="Taxonomy" type="xsd:string" /><xsd:complexType name="Category"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="t:Taxonomy" /><xsd:element ref="t:Code" /><xsd:element ref="t:Description" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="Features" mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element ref="ns1:Warranty" /><xsd:element ref="ns1:Maintenance" /><xsd:any namespace="##other" processContents="skip" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="Manufacturer"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Name" type="xsd:string" minOccurs="0" /><xsd:element name="CopyrightURL" type="xsd:string" minOccurs="0" /><xsd:element name="Copyright" type="xsd:string" minOccurs="0" /><xsd:element name="ProductURL" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="Picture"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Name" type="xsd:string" minOccurs="0" /><xsd:element name="Angle" type="xsd:string" minOccurs="0" /><xsd:element name="Size" type="xsd:string" minOccurs="0" /><xsd:element name="ProductPhotoID" type="xsd:integer" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="ProductDescription"><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="Summary" type="t:Summary" minOccurs="0" /><xsd:element name="Manufacturer" type="t:Manufacturer" minOccurs="0" /><xsd:element name="Features" type="t:Features" minOccurs="0" maxOccurs="unbounded" /><xsd:element name="Picture" type="t:Picture" minOccurs="0" maxOccurs="unbounded" /><xsd:element name="Category" type="t:Category" minOccurs="0" maxOccurs="unbounded" /><xsd:element name="Specifications" type="t:Specifications" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence><xsd:attribute name="ProductModelID" type="xsd:string" /><xsd:attribute name="ProductModelName" type="xsd:string" /></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="Specifications" mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:any processContents="skip" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType><xsd:complexType name="Summary" mixed="true"><xsd:complexContent mixed="true"><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:any namespace="http://www.w3.org/1999/xhtml" processContents="skip" minOccurs="0" maxOccurs="unbounded" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:schema>'
GO
/****** Object:  XmlSchemaCollection [Sales].[StoreSurveySchemaCollection]    Script Date: 7/3/2023 18:06:41 ******/
CREATE XML SCHEMA COLLECTION [Sales].[StoreSurveySchemaCollection] AS N'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:t="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey" targetNamespace="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey" elementFormDefault="qualified"><xsd:element name="StoreSurvey"><xsd:complexType><xsd:complexContent><xsd:restriction base="xsd:anyType"><xsd:sequence><xsd:element name="ContactName" type="xsd:string" minOccurs="0" /><xsd:element name="JobTitle" type="xsd:string" minOccurs="0" /><xsd:element name="AnnualSales" type="xsd:decimal" minOccurs="0" /><xsd:element name="AnnualRevenue" type="xsd:decimal" minOccurs="0" /><xsd:element name="BankName" type="xsd:string" minOccurs="0" /><xsd:element name="BusinessType" type="t:BusinessType" minOccurs="0" /><xsd:element name="YearOpened" type="xsd:gYear" minOccurs="0" /><xsd:element name="Specialty" type="t:SpecialtyType" minOccurs="0" /><xsd:element name="SquareFeet" type="xsd:float" minOccurs="0" /><xsd:element name="Brands" type="t:BrandType" minOccurs="0" /><xsd:element name="Internet" type="t:InternetType" minOccurs="0" /><xsd:element name="NumberEmployees" type="xsd:int" minOccurs="0" /><xsd:element name="Comments" type="xsd:string" minOccurs="0" /></xsd:sequence></xsd:restriction></xsd:complexContent></xsd:complexType></xsd:element><xsd:simpleType name="BrandType"><xsd:restriction base="xsd:string"><xsd:enumeration value="AW" /><xsd:enumeration value="2" /><xsd:enumeration value="3" /><xsd:enumeration value="4+" /></xsd:restriction></xsd:simpleType><xsd:simpleType name="BusinessType"><xsd:restriction base="xsd:string"><xsd:enumeration value="BM" /><xsd:enumeration value="BS" /><xsd:enumeration value="D" /><xsd:enumeration value="OS" /><xsd:enumeration value="SGS" /></xsd:restriction></xsd:simpleType><xsd:simpleType name="InternetType"><xsd:restriction base="xsd:string"><xsd:enumeration value="56kb" /><xsd:enumeration value="ISDN" /><xsd:enumeration value="DSL" /><xsd:enumeration value="T1" /><xsd:enumeration value="T2" /><xsd:enumeration value="T3" /></xsd:restriction></xsd:simpleType><xsd:simpleType name="SpecialtyType"><xsd:restriction base="xsd:string"><xsd:enumeration value="Family" /><xsd:enumeration value="Kids" /><xsd:enumeration value="BMX" /><xsd:enumeration value="Touring" /><xsd:enumeration value="Road" /><xsd:enumeration value="Mountain" /><xsd:enumeration value="All" /></xsd:restriction></xsd:simpleType></xsd:schema>'
GO
/****** Object:  UserDefinedDataType [dbo].[AccountNumber]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[AccountNumber] FROM [nvarchar](15) NULL
GO
/****** Object:  UserDefinedDataType [dbo].[Flag]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[Flag] FROM [bit] NOT NULL
GO
/****** Object:  UserDefinedDataType [dbo].[Name]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[Name] FROM [nvarchar](50) NULL
GO
/****** Object:  UserDefinedDataType [dbo].[NameStyle]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[NameStyle] FROM [bit] NOT NULL
GO
/****** Object:  UserDefinedDataType [dbo].[OrderNumber]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[OrderNumber] FROM [nvarchar](25) NULL
GO
/****** Object:  UserDefinedDataType [dbo].[Phone]    Script Date: 7/3/2023 18:06:41 ******/
CREATE TYPE [dbo].[Phone] FROM [nvarchar](25) NULL
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetAccountingEndDate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetAccountingEndDate]()
RETURNS [datetime] 
AS 
BEGIN
    RETURN DATEADD(millisecond, -2, CONVERT(datetime, '20040701', 112));
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetAccountingStartDate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetAccountingStartDate]()
RETURNS [datetime] 
AS 
BEGIN
    RETURN CONVERT(datetime, '20030701', 112);
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetContactInformation]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetContactInformation](@PersonID int)
RETURNS @retContactInformation TABLE 
(
    -- Columns returned by the function
    [PersonID] int NOT NULL, 
    [FirstName] [nvarchar](50) NULL, 
    [LastName] [nvarchar](50) NULL, 
	[JobTitle] [nvarchar](50) NULL,
    [BusinessEntityType] [nvarchar](50) NULL
)
AS 
-- Returns the first name, last name, job title and business entity type for the specified contact.
-- Since a contact can serve multiple roles, more than one row may be returned.
BEGIN
	IF @PersonID IS NOT NULL 
		BEGIN
		IF EXISTS(SELECT * FROM [HumanResources].[Employee] e 
					WHERE e.[BusinessEntityID] = @PersonID) 
			INSERT INTO @retContactInformation
				SELECT @PersonID, p.FirstName, p.LastName, e.[JobTitle], 'Employee'
				FROM [HumanResources].[Employee] AS e
					INNER JOIN [Person].[Person] p
					ON p.[BusinessEntityID] = e.[BusinessEntityID]
				WHERE e.[BusinessEntityID] = @PersonID;

		IF EXISTS(SELECT * FROM [Purchasing].[Vendor] AS v
					INNER JOIN [Person].[BusinessEntityContact] bec 
					ON bec.[BusinessEntityID] = v.[BusinessEntityID]
					WHERE bec.[PersonID] = @PersonID)
			INSERT INTO @retContactInformation
				SELECT @PersonID, p.FirstName, p.LastName, ct.[Name], 'Vendor Contact' 
				FROM [Purchasing].[Vendor] AS v
					INNER JOIN [Person].[BusinessEntityContact] bec 
					ON bec.[BusinessEntityID] = v.[BusinessEntityID]
					INNER JOIN [Person].ContactType ct
					ON ct.[ContactTypeID] = bec.[ContactTypeID]
					INNER JOIN [Person].[Person] p
					ON p.[BusinessEntityID] = bec.[PersonID]
				WHERE bec.[PersonID] = @PersonID;
		
		IF EXISTS(SELECT * FROM [Sales].[Store] AS s
					INNER JOIN [Person].[BusinessEntityContact] bec 
					ON bec.[BusinessEntityID] = s.[BusinessEntityID]
					WHERE bec.[PersonID] = @PersonID)
			INSERT INTO @retContactInformation
				SELECT @PersonID, p.FirstName, p.LastName, ct.[Name], 'Store Contact' 
				FROM [Sales].[Store] AS s
					INNER JOIN [Person].[BusinessEntityContact] bec 
					ON bec.[BusinessEntityID] = s.[BusinessEntityID]
					INNER JOIN [Person].ContactType ct
					ON ct.[ContactTypeID] = bec.[ContactTypeID]
					INNER JOIN [Person].[Person] p
					ON p.[BusinessEntityID] = bec.[PersonID]
				WHERE bec.[PersonID] = @PersonID;

		IF EXISTS(SELECT * FROM [Person].[Person] AS p
					INNER JOIN [Sales].[Customer] AS c
					ON c.[PersonID] = p.[BusinessEntityID]
					WHERE p.[BusinessEntityID] = @PersonID AND c.[StoreID] IS NULL) 
			INSERT INTO @retContactInformation
				SELECT @PersonID, p.FirstName, p.LastName, NULL, 'Consumer' 
				FROM [Person].[Person] AS p
					INNER JOIN [Sales].[Customer] AS c
					ON c.[PersonID] = p.[BusinessEntityID]
					WHERE p.[BusinessEntityID] = @PersonID AND c.[StoreID] IS NULL; 
		END

	RETURN;
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetDocumentStatusText]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetDocumentStatusText](@Status [tinyint])
RETURNS [nvarchar](16) 
AS 
-- Returns the sales order status text representation for the status value.
BEGIN
    DECLARE @ret [nvarchar](16);

    SET @ret = 
        CASE @Status
            WHEN 1 THEN N'Pending approval'
            WHEN 2 THEN N'Approved'
            WHEN 3 THEN N'Obsolete'
            ELSE N'** Invalid **'
        END;
    
    RETURN @ret
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetProductDealerPrice]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE FUNCTION [dbo].[ufnGetProductDealerPrice](@ProductID [int], @OrderDate [datetime])
RETURNS [money] 
AS 
-- Returns the dealer price for the product on a specific date.
BEGIN
    DECLARE @DealerPrice money;
    DECLARE @DealerDiscount money;

    SET @DealerDiscount = 0.60  -- 60% of list price

    SELECT @DealerPrice = plph.[ListPrice] * @DealerDiscount 
    FROM [Production].[Product] p 
        INNER JOIN [Production].[ProductListPriceHistory] plph 
        ON p.[ProductID] = plph.[ProductID] 
            AND p.[ProductID] = @ProductID 
            AND @OrderDate BETWEEN plph.[StartDate] AND COALESCE(plph.[EndDate], CONVERT(datetime, '99991231', 112)); -- Make sure we get all the prices!

    RETURN @DealerPrice;
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetProductListPrice]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetProductListPrice](@ProductID [int], @OrderDate [datetime])
RETURNS [money] 
AS 
BEGIN
    DECLARE @ListPrice money;

    SELECT @ListPrice = plph.[ListPrice] 
    FROM [Production].[Product] p 
        INNER JOIN [Production].[ProductListPriceHistory] plph 
        ON p.[ProductID] = plph.[ProductID] 
            AND p.[ProductID] = @ProductID 
            AND @OrderDate BETWEEN plph.[StartDate] AND COALESCE(plph.[EndDate], CONVERT(datetime, '99991231', 112)); -- Make sure we get all the prices!

    RETURN @ListPrice;
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetProductStandardCost]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetProductStandardCost](@ProductID [int], @OrderDate [datetime])
RETURNS [money] 
AS 
-- Returns the standard cost for the product on a specific date.
BEGIN
    DECLARE @StandardCost money;

    SELECT @StandardCost = pch.[StandardCost] 
    FROM [Production].[Product] p 
        INNER JOIN [Production].[ProductCostHistory] pch 
        ON p.[ProductID] = pch.[ProductID] 
            AND p.[ProductID] = @ProductID 
            AND @OrderDate BETWEEN pch.[StartDate] AND COALESCE(pch.[EndDate], CONVERT(datetime, '99991231', 112)); -- Make sure we get all the prices!

    RETURN @StandardCost;
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetPurchaseOrderStatusText]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetPurchaseOrderStatusText](@Status [tinyint])
RETURNS [nvarchar](15) 
AS 
-- Returns the sales order status text representation for the status value.
BEGIN
    DECLARE @ret [nvarchar](15);

    SET @ret = 
        CASE @Status
            WHEN 1 THEN 'Pending'
            WHEN 2 THEN 'Approved'
            WHEN 3 THEN 'Rejected'
            WHEN 4 THEN 'Complete'
            ELSE '** Invalid **'
        END;
    
    RETURN @ret
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetSalesOrderStatusText]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetSalesOrderStatusText](@Status [tinyint])
RETURNS [nvarchar](15) 
AS 
-- Returns the sales order status text representation for the status value.
BEGIN
    DECLARE @ret [nvarchar](15);

    SET @ret = 
        CASE @Status
            WHEN 1 THEN 'In process'
            WHEN 2 THEN 'Approved'
            WHEN 3 THEN 'Backordered'
            WHEN 4 THEN 'Rejected'
            WHEN 5 THEN 'Shipped'
            WHEN 6 THEN 'Cancelled'
            ELSE '** Invalid **'
        END;
    
    RETURN @ret
END;
GO
/****** Object:  UserDefinedFunction [dbo].[ufnGetStock]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnGetStock](@ProductID [int])
RETURNS [int] 
AS 
-- Returns the stock level for the product. This function is used internally only
BEGIN
    DECLARE @ret int;
    
    SELECT @ret = SUM(p.[Quantity]) 
    FROM [Production].[ProductInventory] p 
    WHERE p.[ProductID] = @ProductID 
        AND p.[LocationID] = '6'; -- Only look at inventory in the misc storage
    
    IF (@ret IS NULL) 
        SET @ret = 0
    
    RETURN @ret
END;
GO
/****** Object:  Table [Person].[Person]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[Person](
	[BusinessEntityID] [int] NOT NULL,
	[PersonType] [nchar](2) NOT NULL,
	[NameStyle] [dbo].[NameStyle] NOT NULL,
	[Title] [nvarchar](8) NULL,
	[FirstName] [dbo].[Name] NOT NULL,
	[MiddleName] [dbo].[Name] NULL,
	[LastName] [dbo].[Name] NOT NULL,
	[Suffix] [nvarchar](10) NULL,
	[EmailPromotion] [int] NOT NULL,
	[AdditionalContactInfo] [xml](CONTENT [Person].[AdditionalContactInfoSchemaCollection]) NULL,
	[Demographics] [xml](CONTENT [Person].[IndividualSurveySchemaCollection]) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Person_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  View [Person].[vAdditionalContactInfo]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Person].[vAdditionalContactInfo] 
AS 
SELECT 
    [BusinessEntityID] 
    ,[FirstName]
    ,[MiddleName]
    ,[LastName]
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:telephoneNumber)[1]/act:number', 'nvarchar(50)') AS [TelephoneNumber] 
    ,LTRIM(RTRIM([ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:telephoneNumber/act:SpecialInstructions/text())[1]', 'nvarchar(max)'))) AS [TelephoneSpecialInstructions] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes";
        (act:homePostalAddress/act:Street)[1]', 'nvarchar(50)') AS [Street] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:homePostalAddress/act:City)[1]', 'nvarchar(50)') AS [City] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:homePostalAddress/act:StateProvince)[1]', 'nvarchar(50)') AS [StateProvince] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:homePostalAddress/act:PostalCode)[1]', 'nvarchar(50)') AS [PostalCode] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:homePostalAddress/act:CountryRegion)[1]', 'nvarchar(50)') AS [CountryRegion] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:homePostalAddress/act:SpecialInstructions/text())[1]', 'nvarchar(max)') AS [HomeAddressSpecialInstructions] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:eMail/act:eMailAddress)[1]', 'nvarchar(128)') AS [EMailAddress] 
    ,LTRIM(RTRIM([ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:eMail/act:SpecialInstructions/text())[1]', 'nvarchar(max)'))) AS [EMailSpecialInstructions] 
    ,[ContactInfo].ref.value(N'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
        declare namespace act="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactTypes"; 
        (act:eMail/act:SpecialInstructions/act:telephoneNumber/act:number)[1]', 'nvarchar(50)') AS [EMailTelephoneNumber] 
    ,[rowguid] 
    ,[ModifiedDate]
FROM [Person].[Person]
OUTER APPLY [AdditionalContactInfo].nodes(
    'declare namespace ci="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ContactInfo"; 
    /ci:AdditionalContactInfo') AS ContactInfo(ref) 
WHERE [AdditionalContactInfo] IS NOT NULL;
GO
/****** Object:  Table [Person].[PersonPhone]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[PersonPhone](
	[BusinessEntityID] [int] NOT NULL,
	[PhoneNumber] [dbo].[Phone] NOT NULL,
	[PhoneNumberTypeID] [int] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_PersonPhone_BusinessEntityID_PhoneNumber_PhoneNumberTypeID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[PhoneNumber] ASC,
	[PhoneNumberTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[PhoneNumberType]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[PhoneNumberType](
	[PhoneNumberTypeID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_PhoneNumberType_PhoneNumberTypeID] PRIMARY KEY CLUSTERED 
(
	[PhoneNumberTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[Address]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[Address](
	[AddressID] [int] IDENTITY(1,1) NOT FOR REPLICATION NOT NULL,
	[AddressLine1] [nvarchar](60) NOT NULL,
	[AddressLine2] [nvarchar](60) NULL,
	[City] [nvarchar](30) NOT NULL,
	[StateProvinceID] [int] NOT NULL,
	[PostalCode] [nvarchar](15) NOT NULL,
	[SpatialLocation] [geography] NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Address_AddressID] PRIMARY KEY CLUSTERED 
(
	[AddressID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [Person].[StateProvince]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[StateProvince](
	[StateProvinceID] [int] IDENTITY(1,1) NOT NULL,
	[StateProvinceCode] [nchar](3) NOT NULL,
	[CountryRegionCode] [nvarchar](3) NOT NULL,
	[IsOnlyStateProvinceFlag] [dbo].[Flag] NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[TerritoryID] [int] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_StateProvince_StateProvinceID] PRIMARY KEY CLUSTERED 
(
	[StateProvinceID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[BusinessEntityAddress]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[BusinessEntityAddress](
	[BusinessEntityID] [int] NOT NULL,
	[AddressID] [int] NOT NULL,
	[AddressTypeID] [int] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_BusinessEntityAddress_BusinessEntityID_AddressID_AddressTypeID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[AddressID] ASC,
	[AddressTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[CountryRegion]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[CountryRegion](
	[CountryRegionCode] [nvarchar](3) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_CountryRegion_CountryRegionCode] PRIMARY KEY CLUSTERED 
(
	[CountryRegionCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[EmailAddress]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[EmailAddress](
	[BusinessEntityID] [int] NOT NULL,
	[EmailAddressID] [int] IDENTITY(1,1) NOT NULL,
	[EmailAddress] [nvarchar](50) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_EmailAddress_BusinessEntityID_EmailAddressID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[EmailAddressID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [HumanResources].[Employee]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[Employee](
	[BusinessEntityID] [int] NOT NULL,
	[NationalIDNumber] [nvarchar](15) NOT NULL,
	[LoginID] [nvarchar](256) NOT NULL,
	[OrganizationNode] [hierarchyid] NULL,
	[OrganizationLevel]  AS ([OrganizationNode].[GetLevel]()),
	[JobTitle] [nvarchar](50) NOT NULL,
	[BirthDate] [date] NOT NULL,
	[MaritalStatus] [nchar](1) NOT NULL,
	[Gender] [nchar](1) NOT NULL,
	[HireDate] [date] NOT NULL,
	[SalariedFlag] [dbo].[Flag] NOT NULL,
	[VacationHours] [smallint] NOT NULL,
	[SickLeaveHours] [smallint] NOT NULL,
	[CurrentFlag] [dbo].[Flag] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Employee_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [HumanResources].[vEmployee]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vEmployee] 
AS 
SELECT 
    e.[BusinessEntityID]
    ,p.[Title]
    ,p.[FirstName]
    ,p.[MiddleName]
    ,p.[LastName]
    ,p.[Suffix]
    ,e.[JobTitle]  
    ,pp.[PhoneNumber]
    ,pnt.[Name] AS [PhoneNumberType]
    ,ea.[EmailAddress]
    ,p.[EmailPromotion]
    ,a.[AddressLine1]
    ,a.[AddressLine2]
    ,a.[City]
    ,sp.[Name] AS [StateProvinceName] 
    ,a.[PostalCode]
    ,cr.[Name] AS [CountryRegionName] 
    ,p.[AdditionalContactInfo]
FROM [HumanResources].[Employee] e
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = e.[BusinessEntityID]
    INNER JOIN [Person].[BusinessEntityAddress] bea 
    ON bea.[BusinessEntityID] = e.[BusinessEntityID] 
    INNER JOIN [Person].[Address] a 
    ON a.[AddressID] = bea.[AddressID]
    INNER JOIN [Person].[StateProvince] sp 
    ON sp.[StateProvinceID] = a.[StateProvinceID]
    INNER JOIN [Person].[CountryRegion] cr 
    ON cr.[CountryRegionCode] = sp.[CountryRegionCode]
    LEFT OUTER JOIN [Person].[PersonPhone] pp
    ON pp.BusinessEntityID = p.[BusinessEntityID]
    LEFT OUTER JOIN [Person].[PhoneNumberType] pnt
    ON pp.[PhoneNumberTypeID] = pnt.[PhoneNumberTypeID]
    LEFT OUTER JOIN [Person].[EmailAddress] ea
    ON p.[BusinessEntityID] = ea.[BusinessEntityID];
GO
/****** Object:  Table [HumanResources].[Department]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[Department](
	[DepartmentID] [smallint] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[GroupName] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Department_DepartmentID] PRIMARY KEY CLUSTERED 
(
	[DepartmentID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [HumanResources].[EmployeeDepartmentHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[EmployeeDepartmentHistory](
	[BusinessEntityID] [int] NOT NULL,
	[DepartmentID] [smallint] NOT NULL,
	[ShiftID] [tinyint] NOT NULL,
	[StartDate] [date] NOT NULL,
	[EndDate] [date] NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_EmployeeDepartmentHistory_BusinessEntityID_StartDate_DepartmentID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[StartDate] ASC,
	[DepartmentID] ASC,
	[ShiftID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [HumanResources].[vEmployeeDepartment]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vEmployeeDepartment] 
AS 
SELECT 
    e.[BusinessEntityID] 
    ,p.[Title] 
    ,p.[FirstName] 
    ,p.[MiddleName] 
    ,p.[LastName] 
    ,p.[Suffix] 
    ,e.[JobTitle]
    ,d.[Name] AS [Department] 
    ,d.[GroupName] 
    ,edh.[StartDate] 
FROM [HumanResources].[Employee] e
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = e.[BusinessEntityID]
    INNER JOIN [HumanResources].[EmployeeDepartmentHistory] edh 
    ON e.[BusinessEntityID] = edh.[BusinessEntityID] 
    INNER JOIN [HumanResources].[Department] d 
    ON edh.[DepartmentID] = d.[DepartmentID] 
WHERE edh.EndDate IS NULL
GO
/****** Object:  Table [HumanResources].[Shift]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[Shift](
	[ShiftID] [tinyint] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[StartTime] [time](7) NOT NULL,
	[EndTime] [time](7) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Shift_ShiftID] PRIMARY KEY CLUSTERED 
(
	[ShiftID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [HumanResources].[vEmployeeDepartmentHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vEmployeeDepartmentHistory] 
AS 
SELECT 
    e.[BusinessEntityID] 
    ,p.[Title] 
    ,p.[FirstName] 
    ,p.[MiddleName] 
    ,p.[LastName] 
    ,p.[Suffix] 
    ,s.[Name] AS [Shift]
    ,d.[Name] AS [Department] 
    ,d.[GroupName] 
    ,edh.[StartDate] 
    ,edh.[EndDate]
FROM [HumanResources].[Employee] e
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = e.[BusinessEntityID]
    INNER JOIN [HumanResources].[EmployeeDepartmentHistory] edh 
    ON e.[BusinessEntityID] = edh.[BusinessEntityID] 
    INNER JOIN [HumanResources].[Department] d 
    ON edh.[DepartmentID] = d.[DepartmentID] 
    INNER JOIN [HumanResources].[Shift] s
    ON s.[ShiftID] = edh.[ShiftID];
GO
/****** Object:  Table [Person].[AddressType]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[AddressType](
	[AddressTypeID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_AddressType_AddressTypeID] PRIMARY KEY CLUSTERED 
(
	[AddressTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  UserDefinedFunction [dbo].[ufnLeadingZeros]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufnLeadingZeros](
    @Value int
) 
RETURNS varchar(8) 
WITH SCHEMABINDING 
AS 
BEGIN
    DECLARE @ReturnValue varchar(8);

    SET @ReturnValue = CONVERT(varchar(8), @Value);
    SET @ReturnValue = REPLICATE('0', 8 - DATALENGTH(@ReturnValue)) + @ReturnValue;

    RETURN (@ReturnValue);
END;
GO
/****** Object:  Table [Sales].[Customer]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[Customer](
	[CustomerID] [int] IDENTITY(1,1) NOT FOR REPLICATION NOT NULL,
	[PersonID] [int] NULL,
	[StoreID] [int] NULL,
	[TerritoryID] [int] NULL,
	[AccountNumber]  AS (isnull('AW'+[dbo].[ufnLeadingZeros]([CustomerID]),'')),
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Customer_CustomerID] PRIMARY KEY CLUSTERED 
(
	[CustomerID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Sales].[vIndividualCustomer]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vIndividualCustomer] 
AS 
SELECT 
    p.[BusinessEntityID]
    ,p.[Title]
    ,p.[FirstName]
    ,p.[MiddleName]
    ,p.[LastName]
    ,p.[Suffix]
    ,pp.[PhoneNumber]
	,pnt.[Name] AS [PhoneNumberType]
    ,ea.[EmailAddress]
    ,p.[EmailPromotion]
    ,at.[Name] AS [AddressType]
    ,a.[AddressLine1]
    ,a.[AddressLine2]
    ,a.[City]
    ,[StateProvinceName] = sp.[Name]
    ,a.[PostalCode]
    ,[CountryRegionName] = cr.[Name]
    ,p.[Demographics]
FROM [Person].[Person] p
    INNER JOIN [Person].[BusinessEntityAddress] bea 
    ON bea.[BusinessEntityID] = p.[BusinessEntityID] 
    INNER JOIN [Person].[Address] a 
    ON a.[AddressID] = bea.[AddressID]
    INNER JOIN [Person].[StateProvince] sp 
    ON sp.[StateProvinceID] = a.[StateProvinceID]
    INNER JOIN [Person].[CountryRegion] cr 
    ON cr.[CountryRegionCode] = sp.[CountryRegionCode]
    INNER JOIN [Person].[AddressType] at 
    ON at.[AddressTypeID] = bea.[AddressTypeID]
	INNER JOIN [Sales].[Customer] c
	ON c.[PersonID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[EmailAddress] ea
	ON ea.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PersonPhone] pp
	ON pp.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PhoneNumberType] pnt
	ON pnt.[PhoneNumberTypeID] = pp.[PhoneNumberTypeID]
WHERE c.StoreID IS NULL;
GO
/****** Object:  View [Sales].[vPersonDemographics]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vPersonDemographics] 
AS 
SELECT 
    p.[BusinessEntityID] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        TotalPurchaseYTD[1]', 'money') AS [TotalPurchaseYTD] 
    ,CONVERT(datetime, REPLACE([IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        DateFirstPurchase[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [DateFirstPurchase] 
    ,CONVERT(datetime, REPLACE([IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        BirthDate[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [BirthDate] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        MaritalStatus[1]', 'nvarchar(1)') AS [MaritalStatus] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        YearlyIncome[1]', 'nvarchar(30)') AS [YearlyIncome] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Gender[1]', 'nvarchar(1)') AS [Gender] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        TotalChildren[1]', 'integer') AS [TotalChildren] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        NumberChildrenAtHome[1]', 'integer') AS [NumberChildrenAtHome] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Education[1]', 'nvarchar(30)') AS [Education] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Occupation[1]', 'nvarchar(30)') AS [Occupation] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        HomeOwnerFlag[1]', 'bit') AS [HomeOwnerFlag] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        NumberCarsOwned[1]', 'integer') AS [NumberCarsOwned] 
FROM [Person].[Person] p 
CROSS APPLY p.[Demographics].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
    /IndividualSurvey') AS [IndividualSurvey](ref) 
WHERE [Demographics] IS NOT NULL;
GO
/****** Object:  Table [HumanResources].[JobCandidate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[JobCandidate](
	[JobCandidateID] [int] IDENTITY(1,1) NOT NULL,
	[BusinessEntityID] [int] NULL,
	[Resume] [xml](CONTENT [HumanResources].[HRResumeSchemaCollection]) NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_JobCandidate_JobCandidateID] PRIMARY KEY CLUSTERED 
(
	[JobCandidateID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  View [HumanResources].[vJobCandidate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vJobCandidate] 
AS 
SELECT 
    jc.[JobCandidateID] 
    ,jc.[BusinessEntityID] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/Name/Name.Prefix)[1]', 'nvarchar(30)') AS [Name.Prefix] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume";
        (/Resume/Name/Name.First)[1]', 'nvarchar(30)') AS [Name.First] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/Name/Name.Middle)[1]', 'nvarchar(30)') AS [Name.Middle] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/Name/Name.Last)[1]', 'nvarchar(30)') AS [Name.Last] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/Name/Name.Suffix)[1]', 'nvarchar(30)') AS [Name.Suffix] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/Skills)[1]', 'nvarchar(max)') AS [Skills] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Address/Addr.Type)[1]', 'nvarchar(30)') AS [Addr.Type]
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Address/Addr.Location/Location/Loc.CountryRegion)[1]', 'nvarchar(100)') AS [Addr.Loc.CountryRegion]
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Address/Addr.Location/Location/Loc.State)[1]', 'nvarchar(100)') AS [Addr.Loc.State]
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Address/Addr.Location/Location/Loc.City)[1]', 'nvarchar(100)') AS [Addr.Loc.City]
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Address/Addr.PostalCode)[1]', 'nvarchar(20)') AS [Addr.PostalCode]
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/EMail)[1]', 'nvarchar(max)') AS [EMail] 
    ,[Resume].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (/Resume/WebSite)[1]', 'nvarchar(max)') AS [WebSite] 
    ,jc.[ModifiedDate] 
FROM [HumanResources].[JobCandidate] jc 
CROSS APPLY jc.[Resume].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
    /Resume') AS Resume(ref);
GO
/****** Object:  View [HumanResources].[vJobCandidateEmployment]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vJobCandidateEmployment] 
AS 
SELECT 
    jc.[JobCandidateID] 
    ,CONVERT(datetime, REPLACE([Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.StartDate)[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [Emp.StartDate] 
    ,CONVERT(datetime, REPLACE([Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.EndDate)[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [Emp.EndDate] 
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.OrgName)[1]', 'nvarchar(100)') AS [Emp.OrgName]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.JobTitle)[1]', 'nvarchar(100)') AS [Emp.JobTitle]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.Responsibility)[1]', 'nvarchar(max)') AS [Emp.Responsibility]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.FunctionCategory)[1]', 'nvarchar(max)') AS [Emp.FunctionCategory]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.IndustryCategory)[1]', 'nvarchar(max)') AS [Emp.IndustryCategory]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.Location/Location/Loc.CountryRegion)[1]', 'nvarchar(max)') AS [Emp.Loc.CountryRegion]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.Location/Location/Loc.State)[1]', 'nvarchar(max)') AS [Emp.Loc.State]
    ,[Employment].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Emp.Location/Location/Loc.City)[1]', 'nvarchar(max)') AS [Emp.Loc.City]
FROM [HumanResources].[JobCandidate] jc 
CROSS APPLY jc.[Resume].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
    /Resume/Employment') AS Employment(ref);
GO
/****** Object:  View [HumanResources].[vJobCandidateEducation]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [HumanResources].[vJobCandidateEducation] 
AS 
SELECT 
    jc.[JobCandidateID] 
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Level)[1]', 'nvarchar(max)') AS [Edu.Level]
    ,CONVERT(datetime, REPLACE([Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.StartDate)[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [Edu.StartDate] 
    ,CONVERT(datetime, REPLACE([Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.EndDate)[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [Edu.EndDate] 
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Degree)[1]', 'nvarchar(50)') AS [Edu.Degree]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Major)[1]', 'nvarchar(50)') AS [Edu.Major]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Minor)[1]', 'nvarchar(50)') AS [Edu.Minor]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.GPA)[1]', 'nvarchar(5)') AS [Edu.GPA]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.GPAScale)[1]', 'nvarchar(5)') AS [Edu.GPAScale]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.School)[1]', 'nvarchar(100)') AS [Edu.School]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Location/Location/Loc.CountryRegion)[1]', 'nvarchar(100)') AS [Edu.Loc.CountryRegion]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Location/Location/Loc.State)[1]', 'nvarchar(100)') AS [Edu.Loc.State]
    ,[Education].ref.value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
        (Edu.Location/Location/Loc.City)[1]', 'nvarchar(100)') AS [Edu.Loc.City]
FROM [HumanResources].[JobCandidate] jc 
CROSS APPLY jc.[Resume].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/Resume"; 
    /Resume/Education') AS [Education](ref);
GO
/****** Object:  Table [Production].[Product]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[Product](
	[ProductID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ProductNumber] [nvarchar](25) NOT NULL,
	[MakeFlag] [dbo].[Flag] NOT NULL,
	[FinishedGoodsFlag] [dbo].[Flag] NOT NULL,
	[Color] [nvarchar](15) NULL,
	[SafetyStockLevel] [smallint] NOT NULL,
	[ReorderPoint] [smallint] NOT NULL,
	[StandardCost] [money] NOT NULL,
	[ListPrice] [money] NOT NULL,
	[Size] [nvarchar](5) NULL,
	[SizeUnitMeasureCode] [nchar](3) NULL,
	[WeightUnitMeasureCode] [nchar](3) NULL,
	[Weight] [decimal](8, 2) NULL,
	[DaysToManufacture] [int] NOT NULL,
	[ProductLine] [nchar](2) NULL,
	[Class] [nchar](2) NULL,
	[Style] [nchar](2) NULL,
	[ProductSubcategoryID] [int] NULL,
	[ProductModelID] [int] NULL,
	[SellStartDate] [datetime] NOT NULL,
	[SellEndDate] [datetime] NULL,
	[DiscontinuedDate] [datetime] NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Product_ProductID] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductDescription]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductDescription](
	[ProductDescriptionID] [int] IDENTITY(1,1) NOT NULL,
	[Description] [nvarchar](400) NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductDescription_ProductDescriptionID] PRIMARY KEY CLUSTERED 
(
	[ProductDescriptionID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductModel]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductModel](
	[ProductModelID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[CatalogDescription] [xml](CONTENT [Production].[ProductDescriptionSchemaCollection]) NULL,
	[Instructions] [xml](CONTENT [Production].[ManuInstructionsSchemaCollection]) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductModel_ProductModelID] PRIMARY KEY CLUSTERED 
(
	[ProductModelID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductModelProductDescriptionCulture]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductModelProductDescriptionCulture](
	[ProductModelID] [int] NOT NULL,
	[ProductDescriptionID] [int] NOT NULL,
	[CultureID] [nchar](6) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductModelProductDescriptionCulture_ProductModelID_ProductDescriptionID_CultureID] PRIMARY KEY CLUSTERED 
(
	[ProductModelID] ASC,
	[ProductDescriptionID] ASC,
	[CultureID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Production].[vProductAndDescription]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Production].[vProductAndDescription] 
WITH SCHEMABINDING 
AS 
-- View (indexed or standard) to display products and product descriptions by language.
SELECT 
    p.[ProductID] 
    ,p.[Name] 
    ,pm.[Name] AS [ProductModel] 
    ,pmx.[CultureID] 
    ,pd.[Description] 
FROM [Production].[Product] p 
    INNER JOIN [Production].[ProductModel] pm 
    ON p.[ProductModelID] = pm.[ProductModelID] 
    INNER JOIN [Production].[ProductModelProductDescriptionCulture] pmx 
    ON pm.[ProductModelID] = pmx.[ProductModelID] 
    INNER JOIN [Production].[ProductDescription] pd 
    ON pmx.[ProductDescriptionID] = pd.[ProductDescriptionID];
GO
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET NUMERIC_ROUNDABORT OFF
GO
/****** Object:  Index [IX_vProductAndDescription]    Script Date: 7/3/2023 18:06:41 ******/
CREATE UNIQUE CLUSTERED INDEX [IX_vProductAndDescription] ON [Production].[vProductAndDescription]
(
	[CultureID] ASC,
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  View [Production].[vProductModelCatalogDescription]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Production].[vProductModelCatalogDescription] 
AS 
SELECT 
    [ProductModelID] 
    ,[Name] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace html="http://www.w3.org/1999/xhtml"; 
        (/p1:ProductDescription/p1:Summary/html:p)[1]', 'nvarchar(max)') AS [Summary] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Manufacturer/p1:Name)[1]', 'nvarchar(max)') AS [Manufacturer] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Manufacturer/p1:Copyright)[1]', 'nvarchar(30)') AS [Copyright] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Manufacturer/p1:ProductURL)[1]', 'nvarchar(256)') AS [ProductURL] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wm="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain"; 
        (/p1:ProductDescription/p1:Features/wm:Warranty/wm:WarrantyPeriod)[1]', 'nvarchar(256)') AS [WarrantyPeriod] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wm="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain"; 
        (/p1:ProductDescription/p1:Features/wm:Warranty/wm:Description)[1]', 'nvarchar(256)') AS [WarrantyDescription] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wm="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain"; 
        (/p1:ProductDescription/p1:Features/wm:Maintenance/wm:NoOfYears)[1]', 'nvarchar(256)') AS [NoOfYears] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wm="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelWarrAndMain"; 
        (/p1:ProductDescription/p1:Features/wm:Maintenance/wm:Description)[1]', 'nvarchar(256)') AS [MaintenanceDescription] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wf="http://www.adventure-works.com/schemas/OtherFeatures"; 
        (/p1:ProductDescription/p1:Features/wf:wheel)[1]', 'nvarchar(256)') AS [Wheel] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wf="http://www.adventure-works.com/schemas/OtherFeatures"; 
        (/p1:ProductDescription/p1:Features/wf:saddle)[1]', 'nvarchar(256)') AS [Saddle] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wf="http://www.adventure-works.com/schemas/OtherFeatures"; 
        (/p1:ProductDescription/p1:Features/wf:pedal)[1]', 'nvarchar(256)') AS [Pedal] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wf="http://www.adventure-works.com/schemas/OtherFeatures"; 
        (/p1:ProductDescription/p1:Features/wf:BikeFrame)[1]', 'nvarchar(max)') AS [BikeFrame] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        declare namespace wf="http://www.adventure-works.com/schemas/OtherFeatures"; 
        (/p1:ProductDescription/p1:Features/wf:crankset)[1]', 'nvarchar(256)') AS [Crankset] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Picture/p1:Angle)[1]', 'nvarchar(256)') AS [PictureAngle] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Picture/p1:Size)[1]', 'nvarchar(256)') AS [PictureSize] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Picture/p1:ProductPhotoID)[1]', 'nvarchar(256)') AS [ProductPhotoID] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Specifications/Material)[1]', 'nvarchar(256)') AS [Material] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Specifications/Color)[1]', 'nvarchar(256)') AS [Color] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Specifications/ProductLine)[1]', 'nvarchar(256)') AS [ProductLine] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Specifications/Style)[1]', 'nvarchar(256)') AS [Style] 
    ,[CatalogDescription].value(N'declare namespace p1="http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelDescription"; 
        (/p1:ProductDescription/p1:Specifications/RiderExperience)[1]', 'nvarchar(1024)') AS [RiderExperience] 
    ,[rowguid] 
    ,[ModifiedDate]
FROM [Production].[ProductModel] 
WHERE [CatalogDescription] IS NOT NULL;
GO
/****** Object:  View [Production].[vProductModelInstructions]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Production].[vProductModelInstructions] 
AS 
SELECT 
    [ProductModelID] 
    ,[Name] 
    ,[Instructions].value(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelManuInstructions"; 
        (/root/text())[1]', 'nvarchar(max)') AS [Instructions] 
    ,[MfgInstructions].ref.value('@LocationID[1]', 'int') AS [LocationID] 
    ,[MfgInstructions].ref.value('@SetupHours[1]', 'decimal(9, 4)') AS [SetupHours] 
    ,[MfgInstructions].ref.value('@MachineHours[1]', 'decimal(9, 4)') AS [MachineHours] 
    ,[MfgInstructions].ref.value('@LaborHours[1]', 'decimal(9, 4)') AS [LaborHours] 
    ,[MfgInstructions].ref.value('@LotSize[1]', 'int') AS [LotSize] 
    ,[Steps].ref.value('string(.)[1]', 'nvarchar(1024)') AS [Step] 
    ,[rowguid] 
    ,[ModifiedDate]
FROM [Production].[ProductModel] 
CROSS APPLY [Instructions].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelManuInstructions"; 
    /root/Location') MfgInstructions(ref)
CROSS APPLY [MfgInstructions].ref.nodes('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/ProductModelManuInstructions"; 
    step') Steps(ref);
GO
/****** Object:  Table [Sales].[SalesTerritory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesTerritory](
	[TerritoryID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[CountryRegionCode] [nvarchar](3) NOT NULL,
	[Group] [nvarchar](50) NOT NULL,
	[SalesYTD] [money] NOT NULL,
	[SalesLastYear] [money] NOT NULL,
	[CostYTD] [money] NOT NULL,
	[CostLastYear] [money] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesTerritory_TerritoryID] PRIMARY KEY CLUSTERED 
(
	[TerritoryID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesPerson]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesPerson](
	[BusinessEntityID] [int] NOT NULL,
	[TerritoryID] [int] NULL,
	[SalesQuota] [money] NULL,
	[Bonus] [money] NOT NULL,
	[CommissionPct] [smallmoney] NOT NULL,
	[SalesYTD] [money] NOT NULL,
	[SalesLastYear] [money] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesPerson_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Sales].[vSalesPerson]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vSalesPerson] 
AS 
SELECT 
    s.[BusinessEntityID]
    ,p.[Title]
    ,p.[FirstName]
    ,p.[MiddleName]
    ,p.[LastName]
    ,p.[Suffix]
    ,e.[JobTitle]
    ,pp.[PhoneNumber]
	,pnt.[Name] AS [PhoneNumberType]
    ,ea.[EmailAddress]
    ,p.[EmailPromotion]
    ,a.[AddressLine1]
    ,a.[AddressLine2]
    ,a.[City]
    ,[StateProvinceName] = sp.[Name]
    ,a.[PostalCode]
    ,[CountryRegionName] = cr.[Name]
    ,[TerritoryName] = st.[Name]
    ,[TerritoryGroup] = st.[Group]
    ,s.[SalesQuota]
    ,s.[SalesYTD]
    ,s.[SalesLastYear]
FROM [Sales].[SalesPerson] s
    INNER JOIN [HumanResources].[Employee] e 
    ON e.[BusinessEntityID] = s.[BusinessEntityID]
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = s.[BusinessEntityID]
    INNER JOIN [Person].[BusinessEntityAddress] bea 
    ON bea.[BusinessEntityID] = s.[BusinessEntityID] 
    INNER JOIN [Person].[Address] a 
    ON a.[AddressID] = bea.[AddressID]
    INNER JOIN [Person].[StateProvince] sp 
    ON sp.[StateProvinceID] = a.[StateProvinceID]
    INNER JOIN [Person].[CountryRegion] cr 
    ON cr.[CountryRegionCode] = sp.[CountryRegionCode]
    LEFT OUTER JOIN [Sales].[SalesTerritory] st 
    ON st.[TerritoryID] = s.[TerritoryID]
	LEFT OUTER JOIN [Person].[EmailAddress] ea
	ON ea.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PersonPhone] pp
	ON pp.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PhoneNumberType] pnt
	ON pnt.[PhoneNumberTypeID] = pp.[PhoneNumberTypeID];
GO
/****** Object:  Table [Sales].[SalesOrderHeader]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesOrderHeader](
	[SalesOrderID] [int] IDENTITY(1,1) NOT FOR REPLICATION NOT NULL,
	[RevisionNumber] [tinyint] NOT NULL,
	[OrderDate] [datetime] NOT NULL,
	[DueDate] [datetime] NOT NULL,
	[ShipDate] [datetime] NULL,
	[Status] [tinyint] NOT NULL,
	[OnlineOrderFlag] [dbo].[Flag] NOT NULL,
	[SalesOrderNumber]  AS (isnull(N'SO'+CONVERT([nvarchar](23),[SalesOrderID]),N'*** ERROR ***')),
	[PurchaseOrderNumber] [dbo].[OrderNumber] NULL,
	[AccountNumber] [dbo].[AccountNumber] NULL,
	[CustomerID] [int] NOT NULL,
	[SalesPersonID] [int] NULL,
	[TerritoryID] [int] NULL,
	[BillToAddressID] [int] NOT NULL,
	[ShipToAddressID] [int] NOT NULL,
	[ShipMethodID] [int] NOT NULL,
	[CreditCardID] [int] NULL,
	[CreditCardApprovalCode] [varchar](15) NULL,
	[CurrencyRateID] [int] NULL,
	[SubTotal] [money] NOT NULL,
	[TaxAmt] [money] NOT NULL,
	[Freight] [money] NOT NULL,
	[TotalDue]  AS (isnull(([SubTotal]+[TaxAmt])+[Freight],(0))),
	[Comment] [nvarchar](128) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesOrderHeader_SalesOrderID] PRIMARY KEY CLUSTERED 
(
	[SalesOrderID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Sales].[vSalesPersonSalesByFiscalYears]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vSalesPersonSalesByFiscalYears] 
AS 
SELECT 
    pvt.[SalesPersonID]
    ,pvt.[FullName]
    ,pvt.[JobTitle]
    ,pvt.[SalesTerritory]
    ,pvt.[2002]
    ,pvt.[2003]
    ,pvt.[2004] 
FROM (SELECT 
        soh.[SalesPersonID]
        ,p.[FirstName] + ' ' + COALESCE(p.[MiddleName], '') + ' ' + p.[LastName] AS [FullName]
        ,e.[JobTitle]
        ,st.[Name] AS [SalesTerritory]
        ,soh.[SubTotal]
        ,YEAR(DATEADD(m, 6, soh.[OrderDate])) AS [FiscalYear] 
    FROM [Sales].[SalesPerson] sp 
        INNER JOIN [Sales].[SalesOrderHeader] soh 
        ON sp.[BusinessEntityID] = soh.[SalesPersonID]
        INNER JOIN [Sales].[SalesTerritory] st 
        ON sp.[TerritoryID] = st.[TerritoryID] 
        INNER JOIN [HumanResources].[Employee] e 
        ON soh.[SalesPersonID] = e.[BusinessEntityID] 
		INNER JOIN [Person].[Person] p
		ON p.[BusinessEntityID] = sp.[BusinessEntityID]
	 ) AS soh 
PIVOT 
(
    SUM([SubTotal]) 
    FOR [FiscalYear] 
    IN ([2002], [2003], [2004])
) AS pvt;
GO
/****** Object:  View [Person].[vStateProvinceCountryRegion]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Person].[vStateProvinceCountryRegion] 
WITH SCHEMABINDING 
AS 
SELECT 
    sp.[StateProvinceID] 
    ,sp.[StateProvinceCode] 
    ,sp.[IsOnlyStateProvinceFlag] 
    ,sp.[Name] AS [StateProvinceName] 
    ,sp.[TerritoryID] 
    ,cr.[CountryRegionCode] 
    ,cr.[Name] AS [CountryRegionName]
FROM [Person].[StateProvince] sp 
    INNER JOIN [Person].[CountryRegion] cr 
    ON sp.[CountryRegionCode] = cr.[CountryRegionCode];
GO
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET NUMERIC_ROUNDABORT OFF
GO
/****** Object:  Index [IX_vStateProvinceCountryRegion]    Script Date: 7/3/2023 18:06:41 ******/
CREATE UNIQUE CLUSTERED INDEX [IX_vStateProvinceCountryRegion] ON [Person].[vStateProvinceCountryRegion]
(
	[StateProvinceID] ASC,
	[CountryRegionCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[Store]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[Store](
	[BusinessEntityID] [int] NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[SalesPersonID] [int] NULL,
	[Demographics] [xml](CONTENT [Sales].[StoreSurveySchemaCollection]) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Store_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  View [Sales].[vStoreWithDemographics]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vStoreWithDemographics] AS 
SELECT 
    s.[BusinessEntityID] 
    ,s.[Name] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/AnnualSales)[1]', 'money') AS [AnnualSales] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/AnnualRevenue)[1]', 'money') AS [AnnualRevenue] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/BankName)[1]', 'nvarchar(50)') AS [BankName] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/BusinessType)[1]', 'nvarchar(5)') AS [BusinessType] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/YearOpened)[1]', 'integer') AS [YearOpened] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/Specialty)[1]', 'nvarchar(50)') AS [Specialty] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/SquareFeet)[1]', 'integer') AS [SquareFeet] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/Brands)[1]', 'nvarchar(30)') AS [Brands] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/Internet)[1]', 'nvarchar(30)') AS [Internet] 
    ,s.[Demographics].value('declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/StoreSurvey"; 
        (/StoreSurvey/NumberEmployees)[1]', 'integer') AS [NumberEmployees] 
FROM [Sales].[Store] s;
GO
/****** Object:  Table [Person].[BusinessEntityContact]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[BusinessEntityContact](
	[BusinessEntityID] [int] NOT NULL,
	[PersonID] [int] NOT NULL,
	[ContactTypeID] [int] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_BusinessEntityContact_BusinessEntityID_PersonID_ContactTypeID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[PersonID] ASC,
	[ContactTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[ContactType]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[ContactType](
	[ContactTypeID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ContactType_ContactTypeID] PRIMARY KEY CLUSTERED 
(
	[ContactTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Sales].[vStoreWithContacts]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vStoreWithContacts] AS 
SELECT 
    s.[BusinessEntityID] 
    ,s.[Name] 
    ,ct.[Name] AS [ContactType] 
    ,p.[Title] 
    ,p.[FirstName] 
    ,p.[MiddleName] 
    ,p.[LastName] 
    ,p.[Suffix] 
    ,pp.[PhoneNumber] 
	,pnt.[Name] AS [PhoneNumberType]
    ,ea.[EmailAddress] 
    ,p.[EmailPromotion] 
FROM [Sales].[Store] s
    INNER JOIN [Person].[BusinessEntityContact] bec 
    ON bec.[BusinessEntityID] = s.[BusinessEntityID]
	INNER JOIN [Person].[ContactType] ct
	ON ct.[ContactTypeID] = bec.[ContactTypeID]
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = bec.[PersonID]
	LEFT OUTER JOIN [Person].[EmailAddress] ea
	ON ea.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PersonPhone] pp
	ON pp.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PhoneNumberType] pnt
	ON pnt.[PhoneNumberTypeID] = pp.[PhoneNumberTypeID];
GO
/****** Object:  View [Sales].[vStoreWithAddresses]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Sales].[vStoreWithAddresses] AS 
SELECT 
    s.[BusinessEntityID] 
    ,s.[Name] 
    ,at.[Name] AS [AddressType]
    ,a.[AddressLine1] 
    ,a.[AddressLine2] 
    ,a.[City] 
    ,sp.[Name] AS [StateProvinceName] 
    ,a.[PostalCode] 
    ,cr.[Name] AS [CountryRegionName] 
FROM [Sales].[Store] s
    INNER JOIN [Person].[BusinessEntityAddress] bea 
    ON bea.[BusinessEntityID] = s.[BusinessEntityID] 
    INNER JOIN [Person].[Address] a 
    ON a.[AddressID] = bea.[AddressID]
    INNER JOIN [Person].[StateProvince] sp 
    ON sp.[StateProvinceID] = a.[StateProvinceID]
    INNER JOIN [Person].[CountryRegion] cr 
    ON cr.[CountryRegionCode] = sp.[CountryRegionCode]
    INNER JOIN [Person].[AddressType] at 
    ON at.[AddressTypeID] = bea.[AddressTypeID];
GO
/****** Object:  Table [Purchasing].[Vendor]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Purchasing].[Vendor](
	[BusinessEntityID] [int] NOT NULL,
	[AccountNumber] [dbo].[AccountNumber] NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[CreditRating] [tinyint] NOT NULL,
	[PreferredVendorStatus] [dbo].[Flag] NOT NULL,
	[ActiveFlag] [dbo].[Flag] NOT NULL,
	[PurchasingWebServiceURL] [nvarchar](1024) NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Vendor_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [Purchasing].[vVendorWithContacts]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Purchasing].[vVendorWithContacts] AS 
SELECT 
    v.[BusinessEntityID]
    ,v.[Name]
    ,ct.[Name] AS [ContactType] 
    ,p.[Title] 
    ,p.[FirstName] 
    ,p.[MiddleName] 
    ,p.[LastName] 
    ,p.[Suffix] 
    ,pp.[PhoneNumber] 
	,pnt.[Name] AS [PhoneNumberType]
    ,ea.[EmailAddress] 
    ,p.[EmailPromotion] 
FROM [Purchasing].[Vendor] v
    INNER JOIN [Person].[BusinessEntityContact] bec 
    ON bec.[BusinessEntityID] = v.[BusinessEntityID]
	INNER JOIN [Person].ContactType ct
	ON ct.[ContactTypeID] = bec.[ContactTypeID]
	INNER JOIN [Person].[Person] p
	ON p.[BusinessEntityID] = bec.[PersonID]
	LEFT OUTER JOIN [Person].[EmailAddress] ea
	ON ea.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PersonPhone] pp
	ON pp.[BusinessEntityID] = p.[BusinessEntityID]
	LEFT OUTER JOIN [Person].[PhoneNumberType] pnt
	ON pnt.[PhoneNumberTypeID] = pp.[PhoneNumberTypeID];
GO
/****** Object:  View [Purchasing].[vVendorWithAddresses]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [Purchasing].[vVendorWithAddresses] AS 
SELECT 
    v.[BusinessEntityID]
    ,v.[Name]
    ,at.[Name] AS [AddressType]
    ,a.[AddressLine1] 
    ,a.[AddressLine2] 
    ,a.[City] 
    ,sp.[Name] AS [StateProvinceName] 
    ,a.[PostalCode] 
    ,cr.[Name] AS [CountryRegionName] 
FROM [Purchasing].[Vendor] v
    INNER JOIN [Person].[BusinessEntityAddress] bea 
    ON bea.[BusinessEntityID] = v.[BusinessEntityID] 
    INNER JOIN [Person].[Address] a 
    ON a.[AddressID] = bea.[AddressID]
    INNER JOIN [Person].[StateProvince] sp 
    ON sp.[StateProvinceID] = a.[StateProvinceID]
    INNER JOIN [Person].[CountryRegion] cr 
    ON cr.[CountryRegionCode] = sp.[CountryRegionCode]
    INNER JOIN [Person].[AddressType] at 
    ON at.[AddressTypeID] = bea.[AddressTypeID];
GO
/****** Object:  Table [dbo].[AWBuildVersion]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[AWBuildVersion](
	[SystemInformationID] [tinyint] IDENTITY(1,1) NOT NULL,
	[Database Version] [nvarchar](25) NOT NULL,
	[VersionDate] [datetime] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_AWBuildVersion_SystemInformationID] PRIMARY KEY CLUSTERED 
(
	[SystemInformationID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[DatabaseLog]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[DatabaseLog](
	[DatabaseLogID] [int] IDENTITY(1,1) NOT NULL,
	[PostTime] [datetime] NOT NULL,
	[DatabaseUser] [sysname] NOT NULL,
	[Event] [sysname] NOT NULL,
	[Schema] [sysname] NULL,
	[Object] [sysname] NULL,
	[TSQL] [nvarchar](max) NOT NULL,
	[XmlEvent] [xml] NOT NULL,
 CONSTRAINT [PK_DatabaseLog_DatabaseLogID] PRIMARY KEY NONCLUSTERED 
(
	[DatabaseLogID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [dbo].[ErrorLog]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[ErrorLog](
	[ErrorLogID] [int] IDENTITY(1,1) NOT NULL,
	[ErrorTime] [datetime] NOT NULL,
	[UserName] [sysname] NOT NULL,
	[ErrorNumber] [int] NOT NULL,
	[ErrorSeverity] [int] NULL,
	[ErrorState] [int] NULL,
	[ErrorProcedure] [nvarchar](126) NULL,
	[ErrorLine] [int] NULL,
	[ErrorMessage] [nvarchar](4000) NOT NULL,
 CONSTRAINT [PK_ErrorLog_ErrorLogID] PRIMARY KEY CLUSTERED 
(
	[ErrorLogID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [HumanResources].[EmployeePayHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [HumanResources].[EmployeePayHistory](
	[BusinessEntityID] [int] NOT NULL,
	[RateChangeDate] [datetime] NOT NULL,
	[Rate] [money] NOT NULL,
	[PayFrequency] [tinyint] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_EmployeePayHistory_BusinessEntityID_RateChangeDate] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[RateChangeDate] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[BusinessEntity]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[BusinessEntity](
	[BusinessEntityID] [int] IDENTITY(1,1) NOT FOR REPLICATION NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_BusinessEntity_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Person].[Password]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Person].[Password](
	[BusinessEntityID] [int] NOT NULL,
	[PasswordHash] [varchar](128) NOT NULL,
	[PasswordSalt] [varchar](10) NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Password_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[BillOfMaterials]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[BillOfMaterials](
	[BillOfMaterialsID] [int] IDENTITY(1,1) NOT NULL,
	[ProductAssemblyID] [int] NULL,
	[ComponentID] [int] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
	[UnitMeasureCode] [nchar](3) NOT NULL,
	[BOMLevel] [smallint] NOT NULL,
	[PerAssemblyQty] [decimal](8, 2) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_BillOfMaterials_BillOfMaterialsID] PRIMARY KEY NONCLUSTERED 
(
	[BillOfMaterialsID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Index [AK_BillOfMaterials_ProductAssemblyID_ComponentID_StartDate]    Script Date: 7/3/2023 18:06:41 ******/
CREATE UNIQUE CLUSTERED INDEX [AK_BillOfMaterials_ProductAssemblyID_ComponentID_StartDate] ON [Production].[BillOfMaterials]
(
	[ProductAssemblyID] ASC,
	[ComponentID] ASC,
	[StartDate] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Table [Production].[Culture]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[Culture](
	[CultureID] [nchar](6) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Culture_CultureID] PRIMARY KEY CLUSTERED 
(
	[CultureID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[Document]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[Document](
	[DocumentNode] [hierarchyid] NOT NULL,
	[DocumentLevel]  AS ([DocumentNode].[GetLevel]()),
	[Title] [nvarchar](50) NOT NULL,
	[Owner] [int] NOT NULL,
	[FolderFlag] [bit] NOT NULL,
	[FileName] [nvarchar](400) NOT NULL,
	[FileExtension] [nvarchar](8) NOT NULL,
	[Revision] [nchar](5) NOT NULL,
	[ChangeNumber] [int] NOT NULL,
	[Status] [tinyint] NOT NULL,
	[DocumentSummary] [nvarchar](max) NULL,
	[Document] [varbinary](max) NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Document_DocumentNode] PRIMARY KEY CLUSTERED 
(
	[DocumentNode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY],
UNIQUE NONCLUSTERED 
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [Production].[Illustration]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[Illustration](
	[IllustrationID] [int] IDENTITY(1,1) NOT NULL,
	[Diagram] [xml] NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Illustration_IllustrationID] PRIMARY KEY CLUSTERED 
(
	[IllustrationID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [Production].[Location]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[Location](
	[LocationID] [smallint] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[CostRate] [smallmoney] NOT NULL,
	[Availability] [decimal](8, 2) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Location_LocationID] PRIMARY KEY CLUSTERED 
(
	[LocationID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductCategory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductCategory](
	[ProductCategoryID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductCategory_ProductCategoryID] PRIMARY KEY CLUSTERED 
(
	[ProductCategoryID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductCostHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductCostHistory](
	[ProductID] [int] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
	[StandardCost] [money] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductCostHistory_ProductID_StartDate] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC,
	[StartDate] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductDocument]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductDocument](
	[ProductID] [int] NOT NULL,
	[DocumentNode] [hierarchyid] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductDocument_ProductID_DocumentNode] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC,
	[DocumentNode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductInventory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductInventory](
	[ProductID] [int] NOT NULL,
	[LocationID] [smallint] NOT NULL,
	[Shelf] [nvarchar](10) NOT NULL,
	[Bin] [tinyint] NOT NULL,
	[Quantity] [smallint] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductInventory_ProductID_LocationID] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC,
	[LocationID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductListPriceHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductListPriceHistory](
	[ProductID] [int] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
	[ListPrice] [money] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductListPriceHistory_ProductID_StartDate] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC,
	[StartDate] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductModelIllustration]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductModelIllustration](
	[ProductModelID] [int] NOT NULL,
	[IllustrationID] [int] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductModelIllustration_ProductModelID_IllustrationID] PRIMARY KEY CLUSTERED 
(
	[ProductModelID] ASC,
	[IllustrationID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductPhoto]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductPhoto](
	[ProductPhotoID] [int] IDENTITY(1,1) NOT NULL,
	[ThumbNailPhoto] [varbinary](max) NULL,
	[ThumbnailPhotoFileName] [nvarchar](50) NULL,
	[LargePhoto] [varbinary](max) NULL,
	[LargePhotoFileName] [nvarchar](50) NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductPhoto_ProductPhotoID] PRIMARY KEY CLUSTERED 
(
	[ProductPhotoID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductProductPhoto]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductProductPhoto](
	[ProductID] [int] NOT NULL,
	[ProductPhotoID] [int] NOT NULL,
	[Primary] [dbo].[Flag] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductProductPhoto_ProductID_ProductPhotoID] PRIMARY KEY NONCLUSTERED 
(
	[ProductID] ASC,
	[ProductPhotoID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductReview]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductReview](
	[ProductReviewID] [int] IDENTITY(1,1) NOT NULL,
	[ProductID] [int] NOT NULL,
	[ReviewerName] [dbo].[Name] NOT NULL,
	[ReviewDate] [datetime] NOT NULL,
	[EmailAddress] [nvarchar](50) NOT NULL,
	[Rating] [int] NOT NULL,
	[Comments] [nvarchar](3850) NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductReview_ProductReviewID] PRIMARY KEY CLUSTERED 
(
	[ProductReviewID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ProductSubcategory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ProductSubcategory](
	[ProductSubcategoryID] [int] IDENTITY(1,1) NOT NULL,
	[ProductCategoryID] [int] NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductSubcategory_ProductSubcategoryID] PRIMARY KEY CLUSTERED 
(
	[ProductSubcategoryID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[ScrapReason]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[ScrapReason](
	[ScrapReasonID] [smallint] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ScrapReason_ScrapReasonID] PRIMARY KEY CLUSTERED 
(
	[ScrapReasonID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[TransactionHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[TransactionHistory](
	[TransactionID] [int] IDENTITY(100000,1) NOT NULL,
	[ProductID] [int] NOT NULL,
	[ReferenceOrderID] [int] NOT NULL,
	[ReferenceOrderLineID] [int] NOT NULL,
	[TransactionDate] [datetime] NOT NULL,
	[TransactionType] [nchar](1) NOT NULL,
	[Quantity] [int] NOT NULL,
	[ActualCost] [money] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_TransactionHistory_TransactionID] PRIMARY KEY CLUSTERED 
(
	[TransactionID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[TransactionHistoryArchive]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[TransactionHistoryArchive](
	[TransactionID] [int] NOT NULL,
	[ProductID] [int] NOT NULL,
	[ReferenceOrderID] [int] NOT NULL,
	[ReferenceOrderLineID] [int] NOT NULL,
	[TransactionDate] [datetime] NOT NULL,
	[TransactionType] [nchar](1) NOT NULL,
	[Quantity] [int] NOT NULL,
	[ActualCost] [money] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_TransactionHistoryArchive_TransactionID] PRIMARY KEY CLUSTERED 
(
	[TransactionID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[UnitMeasure]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[UnitMeasure](
	[UnitMeasureCode] [nchar](3) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_UnitMeasure_UnitMeasureCode] PRIMARY KEY CLUSTERED 
(
	[UnitMeasureCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[WorkOrder]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[WorkOrder](
	[WorkOrderID] [int] IDENTITY(1,1) NOT NULL,
	[ProductID] [int] NOT NULL,
	[OrderQty] [int] NOT NULL,
	[StockedQty]  AS (isnull([OrderQty]-[ScrappedQty],(0))),
	[ScrappedQty] [smallint] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
	[DueDate] [datetime] NOT NULL,
	[ScrapReasonID] [smallint] NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_WorkOrder_WorkOrderID] PRIMARY KEY CLUSTERED 
(
	[WorkOrderID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Production].[WorkOrderRouting]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Production].[WorkOrderRouting](
	[WorkOrderID] [int] NOT NULL,
	[ProductID] [int] NOT NULL,
	[OperationSequence] [smallint] NOT NULL,
	[LocationID] [smallint] NOT NULL,
	[ScheduledStartDate] [datetime] NOT NULL,
	[ScheduledEndDate] [datetime] NOT NULL,
	[ActualStartDate] [datetime] NULL,
	[ActualEndDate] [datetime] NULL,
	[ActualResourceHrs] [decimal](9, 4) NULL,
	[PlannedCost] [money] NOT NULL,
	[ActualCost] [money] NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_WorkOrderRouting_WorkOrderID_ProductID_OperationSequence] PRIMARY KEY CLUSTERED 
(
	[WorkOrderID] ASC,
	[ProductID] ASC,
	[OperationSequence] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Purchasing].[ProductVendor]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Purchasing].[ProductVendor](
	[ProductID] [int] NOT NULL,
	[BusinessEntityID] [int] NOT NULL,
	[AverageLeadTime] [int] NOT NULL,
	[StandardPrice] [money] NOT NULL,
	[LastReceiptCost] [money] NULL,
	[LastReceiptDate] [datetime] NULL,
	[MinOrderQty] [int] NOT NULL,
	[MaxOrderQty] [int] NOT NULL,
	[OnOrderQty] [int] NULL,
	[UnitMeasureCode] [nchar](3) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ProductVendor_ProductID_BusinessEntityID] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC,
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Purchasing].[PurchaseOrderDetail]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Purchasing].[PurchaseOrderDetail](
	[PurchaseOrderID] [int] NOT NULL,
	[PurchaseOrderDetailID] [int] IDENTITY(1,1) NOT NULL,
	[DueDate] [datetime] NOT NULL,
	[OrderQty] [smallint] NOT NULL,
	[ProductID] [int] NOT NULL,
	[UnitPrice] [money] NOT NULL,
	[LineTotal]  AS (isnull([OrderQty]*[UnitPrice],(0.00))),
	[ReceivedQty] [decimal](8, 2) NOT NULL,
	[RejectedQty] [decimal](8, 2) NOT NULL,
	[StockedQty]  AS (isnull([ReceivedQty]-[RejectedQty],(0.00))),
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_PurchaseOrderDetail_PurchaseOrderID_PurchaseOrderDetailID] PRIMARY KEY CLUSTERED 
(
	[PurchaseOrderID] ASC,
	[PurchaseOrderDetailID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Purchasing].[PurchaseOrderHeader]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Purchasing].[PurchaseOrderHeader](
	[PurchaseOrderID] [int] IDENTITY(1,1) NOT NULL,
	[RevisionNumber] [tinyint] NOT NULL,
	[Status] [tinyint] NOT NULL,
	[EmployeeID] [int] NOT NULL,
	[VendorID] [int] NOT NULL,
	[ShipMethodID] [int] NOT NULL,
	[OrderDate] [datetime] NOT NULL,
	[ShipDate] [datetime] NULL,
	[SubTotal] [money] NOT NULL,
	[TaxAmt] [money] NOT NULL,
	[Freight] [money] NOT NULL,
	[TotalDue]  AS (isnull(([SubTotal]+[TaxAmt])+[Freight],(0))) PERSISTED NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_PurchaseOrderHeader_PurchaseOrderID] PRIMARY KEY CLUSTERED 
(
	[PurchaseOrderID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Purchasing].[ShipMethod]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Purchasing].[ShipMethod](
	[ShipMethodID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ShipBase] [money] NOT NULL,
	[ShipRate] [money] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ShipMethod_ShipMethodID] PRIMARY KEY CLUSTERED 
(
	[ShipMethodID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[CountryRegionCurrency]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[CountryRegionCurrency](
	[CountryRegionCode] [nvarchar](3) NOT NULL,
	[CurrencyCode] [nchar](3) NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_CountryRegionCurrency_CountryRegionCode_CurrencyCode] PRIMARY KEY CLUSTERED 
(
	[CountryRegionCode] ASC,
	[CurrencyCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[CreditCard]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[CreditCard](
	[CreditCardID] [int] IDENTITY(1,1) NOT NULL,
	[CardType] [nvarchar](50) NOT NULL,
	[CardNumber] [nvarchar](25) NOT NULL,
	[ExpMonth] [tinyint] NOT NULL,
	[ExpYear] [smallint] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_CreditCard_CreditCardID] PRIMARY KEY CLUSTERED 
(
	[CreditCardID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[Currency]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[Currency](
	[CurrencyCode] [nchar](3) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_Currency_CurrencyCode] PRIMARY KEY CLUSTERED 
(
	[CurrencyCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[CurrencyRate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[CurrencyRate](
	[CurrencyRateID] [int] IDENTITY(1,1) NOT NULL,
	[CurrencyRateDate] [datetime] NOT NULL,
	[FromCurrencyCode] [nchar](3) NOT NULL,
	[ToCurrencyCode] [nchar](3) NOT NULL,
	[AverageRate] [money] NOT NULL,
	[EndOfDayRate] [money] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_CurrencyRate_CurrencyRateID] PRIMARY KEY CLUSTERED 
(
	[CurrencyRateID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[PersonCreditCard]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[PersonCreditCard](
	[BusinessEntityID] [int] NOT NULL,
	[CreditCardID] [int] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_PersonCreditCard_BusinessEntityID_CreditCardID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[CreditCardID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesOrderDetail]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesOrderDetail](
	[SalesOrderID] [int] NOT NULL,
	[SalesOrderDetailID] [int] IDENTITY(1,1) NOT NULL,
	[CarrierTrackingNumber] [nvarchar](25) NULL,
	[OrderQty] [smallint] NOT NULL,
	[ProductID] [int] NOT NULL,
	[SpecialOfferID] [int] NOT NULL,
	[UnitPrice] [money] NOT NULL,
	[UnitPriceDiscount] [money] NOT NULL,
	[LineTotal]  AS (isnull(([UnitPrice]*((1.0)-[UnitPriceDiscount]))*[OrderQty],(0.0))),
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesOrderDetail_SalesOrderID_SalesOrderDetailID] PRIMARY KEY CLUSTERED 
(
	[SalesOrderID] ASC,
	[SalesOrderDetailID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesOrderHeaderSalesReason]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesOrderHeaderSalesReason](
	[SalesOrderID] [int] NOT NULL,
	[SalesReasonID] [int] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesOrderHeaderSalesReason_SalesOrderID_SalesReasonID] PRIMARY KEY CLUSTERED 
(
	[SalesOrderID] ASC,
	[SalesReasonID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesPersonQuotaHistory]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesPersonQuotaHistory](
	[BusinessEntityID] [int] NOT NULL,
	[QuotaDate] [datetime] NOT NULL,
	[SalesQuota] [money] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesPersonQuotaHistory_BusinessEntityID_QuotaDate] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[QuotaDate] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesReason]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesReason](
	[SalesReasonID] [int] IDENTITY(1,1) NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[ReasonType] [dbo].[Name] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesReason_SalesReasonID] PRIMARY KEY CLUSTERED 
(
	[SalesReasonID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesTaxRate]    Script Date: 7/3/2023 18:06:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesTaxRate](
	[SalesTaxRateID] [int] IDENTITY(1,1) NOT NULL,
	[StateProvinceID] [int] NOT NULL,
	[TaxType] [tinyint] NOT NULL,
	[TaxRate] [smallmoney] NOT NULL,
	[Name] [dbo].[Name] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesTaxRate_SalesTaxRateID] PRIMARY KEY CLUSTERED 
(
	[SalesTaxRateID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SalesTerritoryHistory]    Script Date: 7/3/2023 18:06:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SalesTerritoryHistory](
	[BusinessEntityID] [int] NOT NULL,
	[TerritoryID] [int] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SalesTerritoryHistory_BusinessEntityID_StartDate_TerritoryID] PRIMARY KEY CLUSTERED 
(
	[BusinessEntityID] ASC,
	[StartDate] ASC,
	[TerritoryID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[ShoppingCartItem]    Script Date: 7/3/2023 18:06:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[ShoppingCartItem](
	[ShoppingCartItemID] [int] IDENTITY(1,1) NOT NULL,
	[ShoppingCartID] [nvarchar](50) NOT NULL,
	[Quantity] [int] NOT NULL,
	[ProductID] [int] NOT NULL,
	[DateCreated] [datetime] NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_ShoppingCartItem_ShoppingCartItemID] PRIMARY KEY CLUSTERED 
(
	[ShoppingCartItemID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SpecialOffer]    Script Date: 7/3/2023 18:06:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SpecialOffer](
	[SpecialOfferID] [int] IDENTITY(1,1) NOT NULL,
	[Description] [nvarchar](255) NOT NULL,
	[DiscountPct] [smallmoney] NOT NULL,
	[Type] [nvarchar](50) NOT NULL,
	[Category] [nvarchar](50) NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NOT NULL,
	[MinQty] [int] NOT NULL,
	[MaxQty] [int] NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SpecialOffer_SpecialOfferID] PRIMARY KEY CLUSTERED 
(
	[SpecialOfferID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [Sales].[SpecialOfferProduct]    Script Date: 7/3/2023 18:06:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Sales].[SpecialOfferProduct](
	[SpecialOfferID] [int] NOT NULL,
	[ProductID] [int] NOT NULL,
	[rowguid] [uniqueidentifier] ROWGUIDCOL  NOT NULL,
	[ModifiedDate] [datetime] NOT NULL,
 CONSTRAINT [PK_SpecialOfferProduct_SpecialOfferID_ProductID] PRIMARY KEY CLUSTERED 
(
	[SpecialOfferID] ASC,
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Department_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Department_Name] ON [HumanResources].[Department]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Employee_LoginID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Employee_LoginID] ON [HumanResources].[Employee]
(
	[LoginID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Employee_NationalIDNumber]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Employee_NationalIDNumber] ON [HumanResources].[Employee]
(
	[NationalIDNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Employee_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Employee_rowguid] ON [HumanResources].[Employee]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET NUMERIC_ROUNDABORT OFF
GO
/****** Object:  Index [IX_Employee_OrganizationLevel_OrganizationNode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_Employee_OrganizationLevel_OrganizationNode] ON [HumanResources].[Employee]
(
	[OrganizationLevel] ASC,
	[OrganizationNode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Employee_OrganizationNode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_Employee_OrganizationNode] ON [HumanResources].[Employee]
(
	[OrganizationNode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_EmployeeDepartmentHistory_DepartmentID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_EmployeeDepartmentHistory_DepartmentID] ON [HumanResources].[EmployeeDepartmentHistory]
(
	[DepartmentID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_EmployeeDepartmentHistory_ShiftID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_EmployeeDepartmentHistory_ShiftID] ON [HumanResources].[EmployeeDepartmentHistory]
(
	[ShiftID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_JobCandidate_BusinessEntityID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_JobCandidate_BusinessEntityID] ON [HumanResources].[JobCandidate]
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Shift_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Shift_Name] ON [HumanResources].[Shift]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Shift_StartTime_EndTime]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Shift_StartTime_EndTime] ON [HumanResources].[Shift]
(
	[StartTime] ASC,
	[EndTime] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Address_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Address_rowguid] ON [Person].[Address]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_Address_AddressLine1_AddressLine2_City_StateProvinceID_PostalCode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [IX_Address_AddressLine1_AddressLine2_City_StateProvinceID_PostalCode] ON [Person].[Address]
(
	[AddressLine1] ASC,
	[AddressLine2] ASC,
	[City] ASC,
	[StateProvinceID] ASC,
	[PostalCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Address_StateProvinceID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_Address_StateProvinceID] ON [Person].[Address]
(
	[StateProvinceID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_AddressType_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_AddressType_Name] ON [Person].[AddressType]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_AddressType_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_AddressType_rowguid] ON [Person].[AddressType]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_BusinessEntity_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_BusinessEntity_rowguid] ON [Person].[BusinessEntity]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_BusinessEntityAddress_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_BusinessEntityAddress_rowguid] ON [Person].[BusinessEntityAddress]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_BusinessEntityAddress_AddressID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_BusinessEntityAddress_AddressID] ON [Person].[BusinessEntityAddress]
(
	[AddressID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_BusinessEntityAddress_AddressTypeID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_BusinessEntityAddress_AddressTypeID] ON [Person].[BusinessEntityAddress]
(
	[AddressTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_BusinessEntityContact_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_BusinessEntityContact_rowguid] ON [Person].[BusinessEntityContact]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_BusinessEntityContact_ContactTypeID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_BusinessEntityContact_ContactTypeID] ON [Person].[BusinessEntityContact]
(
	[ContactTypeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_BusinessEntityContact_PersonID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_BusinessEntityContact_PersonID] ON [Person].[BusinessEntityContact]
(
	[PersonID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ContactType_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ContactType_Name] ON [Person].[ContactType]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_CountryRegion_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_CountryRegion_Name] ON [Person].[CountryRegion]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_EmailAddress_EmailAddress]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_EmailAddress_EmailAddress] ON [Person].[EmailAddress]
(
	[EmailAddress] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Person_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Person_rowguid] ON [Person].[Person]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_Person_LastName_FirstName_MiddleName]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_Person_LastName_FirstName_MiddleName] ON [Person].[Person]
(
	[LastName] ASC,
	[FirstName] ASC,
	[MiddleName] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_PersonPhone_PhoneNumber]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_PersonPhone_PhoneNumber] ON [Person].[PersonPhone]
(
	[PhoneNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_StateProvince_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_StateProvince_Name] ON [Person].[StateProvince]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_StateProvince_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_StateProvince_rowguid] ON [Person].[StateProvince]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_StateProvince_StateProvinceCode_CountryRegionCode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_StateProvince_StateProvinceCode_CountryRegionCode] ON [Person].[StateProvince]
(
	[StateProvinceCode] ASC,
	[CountryRegionCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_BillOfMaterials_UnitMeasureCode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_BillOfMaterials_UnitMeasureCode] ON [Production].[BillOfMaterials]
(
	[UnitMeasureCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Culture_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Culture_Name] ON [Production].[Culture]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET NUMERIC_ROUNDABORT OFF
GO
/****** Object:  Index [AK_Document_DocumentLevel_DocumentNode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Document_DocumentLevel_DocumentNode] ON [Production].[Document]
(
	[DocumentLevel] ASC,
	[DocumentNode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Document_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Document_rowguid] ON [Production].[Document]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_Document_FileName_Revision]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_Document_FileName_Revision] ON [Production].[Document]
(
	[FileName] ASC,
	[Revision] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Location_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Location_Name] ON [Production].[Location]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Product_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Product_Name] ON [Production].[Product]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Product_ProductNumber]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Product_ProductNumber] ON [Production].[Product]
(
	[ProductNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_Product_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Product_rowguid] ON [Production].[Product]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ProductCategory_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductCategory_Name] ON [Production].[ProductCategory]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_ProductCategory_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductCategory_rowguid] ON [Production].[ProductCategory]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_ProductDescription_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductDescription_rowguid] ON [Production].[ProductDescription]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ProductModel_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductModel_Name] ON [Production].[ProductModel]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_ProductModel_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductModel_rowguid] ON [Production].[ProductModel]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_ProductReview_ProductID_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_ProductReview_ProductID_Name] ON [Production].[ProductReview]
(
	[ProductID] ASC,
	[ReviewerName] ASC
)
INCLUDE([Comments]) WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ProductSubcategory_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductSubcategory_Name] ON [Production].[ProductSubcategory]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_ProductSubcategory_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ProductSubcategory_rowguid] ON [Production].[ProductSubcategory]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ScrapReason_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ScrapReason_Name] ON [Production].[ScrapReason]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_TransactionHistory_ProductID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_TransactionHistory_ProductID] ON [Production].[TransactionHistory]
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_TransactionHistory_ReferenceOrderID_ReferenceOrderLineID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_TransactionHistory_ReferenceOrderID_ReferenceOrderLineID] ON [Production].[TransactionHistory]
(
	[ReferenceOrderID] ASC,
	[ReferenceOrderLineID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_TransactionHistoryArchive_ProductID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_TransactionHistoryArchive_ProductID] ON [Production].[TransactionHistoryArchive]
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_TransactionHistoryArchive_ReferenceOrderID_ReferenceOrderLineID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_TransactionHistoryArchive_ReferenceOrderID_ReferenceOrderLineID] ON [Production].[TransactionHistoryArchive]
(
	[ReferenceOrderID] ASC,
	[ReferenceOrderLineID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_UnitMeasure_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_UnitMeasure_Name] ON [Production].[UnitMeasure]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_WorkOrder_ProductID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_WorkOrder_ProductID] ON [Production].[WorkOrder]
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_WorkOrder_ScrapReasonID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_WorkOrder_ScrapReasonID] ON [Production].[WorkOrder]
(
	[ScrapReasonID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_WorkOrderRouting_ProductID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_WorkOrderRouting_ProductID] ON [Production].[WorkOrderRouting]
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_ProductVendor_BusinessEntityID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_ProductVendor_BusinessEntityID] ON [Purchasing].[ProductVendor]
(
	[BusinessEntityID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_ProductVendor_UnitMeasureCode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_ProductVendor_UnitMeasureCode] ON [Purchasing].[ProductVendor]
(
	[UnitMeasureCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_PurchaseOrderDetail_ProductID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_PurchaseOrderDetail_ProductID] ON [Purchasing0].[PurchaseOrderDetail]
(
	[ProductID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_PurchaseOrderHeader_EmployeeID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_PurchaseOrderHeader_EmployeeID] ON [Purchasing0].[PurchaseOrderHeader]
(
	[EmployeeID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_PurchaseOrderHeader_VendorID]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_PurchaseOrderHeader_VendorID] ON [Purchasing0].[PurchaseOrderHeader]
(
	[VendorID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_ShipMethod_Name]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ShipMethod_Name] ON [Purchasing].[ShipMethod]
(
	[Name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [AK_ShipMethod_rowguid]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_ShipMethod_rowguid] ON [Purchasing].[ShipMethod]
(
	[rowguid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [AK_Vendor_AccountNumber]    Script Date: 7/3/2023 18:06:42 ******/
CREATE UNIQUE NONCLUSTERED INDEX [AK_Vendor_AccountNumber] ON [Purchasing].[Vendor]
(
	[AccountNumber] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
/****** Object:  Index [IX_CountryRegionCurrency_CurrencyCode]    Script Date: 7/3/2023 18:06:42 ******/
CREATE NONCLUSTERED INDEX [IX_CountryRegionCurrency_CurrencyCode] ON [Sales].[CountryRegionCurrency]
(
	[CurrencyCode] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
SET ANSI_PADDING ON
GO
