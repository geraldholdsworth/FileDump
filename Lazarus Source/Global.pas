unit Global;

{$MODE objfpc}{$H+}

interface

uses
 Registry;

procedure OpenReg(key: String);
function DeleteKey(key: String): Boolean;
function GetRegValS(V: String;D: String): String;
procedure GetRegValA(V: String;var D: array of Byte);
function GetRegValI(V: String;D: Cardinal): Cardinal;
function GetRegValB(V: String;D: Boolean): Boolean;
procedure SetRegValS(V: String;D: String);
procedure SetRegValA(V: String;var D: array of Byte);
procedure SetRegValI(V: String;D: Cardinal);
procedure SetRegValB(V: String;D: Boolean);
function ExtractKey(var V: String):String;

var
 HexReg              : TRegistry;

const
 //Registry Key to use
 RegKey              = '\Software\GJH Software\File Dump';

implementation

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
procedure OpenReg(key: String);
begin
 HexReg:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 HexReg.OpenKey(RegKey+key,true);
end;

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
function DeleteKey(key: String): Boolean;
var
 x: Boolean;
begin
 x:=True;
 OpenReg(ExtractKey(key));
 if HexReg.ValueExists(key) then x:=HexReg.DeleteValue(key);
 HexReg.Free;
 Result:=x;
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValS(V: String;D: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If HexReg.ValueExists(V)then X:=HexReg.ReadString(V)
 else begin X:=D;HexReg.WriteString(V,X);end;
 HexReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
procedure GetRegValA(V: String;var D: array of Byte);
var
 s: Integer;
begin
 OpenReg(ExtractKey(V));
 If HexReg.ValueExists(V)then
 begin
  s:=HexReg.GetDataSize(V);
  HexReg.ReadBinaryData(V,D,s);
 end
 else
 begin
  HexReg.WriteBinaryData(V,D,SizeOf(D));
 end;
 HexReg.Free;
end;

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If HexReg.ValueExists(V)then X:=HexReg.ReadInteger(V)
 else begin X:=D;HexReg.WriteInteger(V,X);end;
 HexReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If HexReg.ValueExists(V)then X:=HexReg.ReadBool(V)
 else begin X:=D;HexReg.WriteBool(V,X);end;
 HexReg.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure SetRegValS(V: String;D: String);
begin
 OpenReg(ExtractKey(V));
 HexReg.WriteString(V,D);
 HexReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
procedure SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 HexReg.WriteBinaryData(V,D,SizeOf(D));
 HexReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure SetRegValI(V: String;D: Cardinal);
begin
 OpenReg(ExtractKey(V));
 HexReg.WriteInteger(V,D);
 HexReg.Free;
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure SetRegValB(V: String;D: Boolean);
begin
 OpenReg(ExtractKey(V));
 HexReg.WriteBool(V,D);
 HexReg.Free;
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

end.
