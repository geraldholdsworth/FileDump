unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,
 ExtCtrls, StdCtrls, ComCtrls;

type

 { TMainForm }

 TMainForm = class(TForm)
  btnMoveDown: TSpeedButton;
  btnMoveToBottom: TSpeedButton;
  btnMoveDownLine: TSpeedButton;
  btnMoveToTop: TSpeedButton;
  btnMoveUpLine: TSpeedButton;
  btnOpen: TSpeedButton;
  edJump: TEdit;
  edXOR: TEdit;
  HexDumpDisplay: TStringGrid;
  ButtonImages: TImageList;
  NavImages: TImageList;
  Label1: TLabel;
  Label2: TLabel;
  OpenFile: TOpenDialog;
  Panel1: TPanel;
  Panel2: TPanel;
  btnMoveUp: TSpeedButton;
  btnSaveText: TSpeedButton;
  SaveFile: TSaveDialog;
  ScrollBar1: TScrollBar;
  btnLoadCompare: TSpeedButton;
  StatusBar: TStatusBar;
  procedure btnLoadCompareClick(Sender: TObject);
  procedure btnMoveDownClick(Sender: TObject);
  procedure btnMoveDownLineClick(Sender: TObject);
  procedure btnMoveToBottomClick(Sender: TObject);
  procedure btnMoveToTopClick(Sender: TObject);
  procedure btnMoveUpClick(Sender: TObject);
  procedure btnMoveUpLineClick(Sender: TObject);
  procedure btnOpenClick(Sender: TObject);
  procedure edXORKeyPress(Sender: TObject; var Key: char);
  procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
   var ScrollPos: Integer);
  procedure WriteLine(F: TFileStream;line: String);
  procedure btnSaveTextClick(Sender: TObject);
  procedure DisplayHex(start: Cardinal);
  procedure edJumpKeyPress(Sender: TObject; var Key: char);
  procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure HexDumpDisplayGetCellHint(Sender: TObject; ACol, ARow: Integer;
   var HintText: String);
  procedure HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
  procedure HexDumpDisplayPrepareCanvas(sender: TObject; aCol, aRow: Integer;
   aState: TGridDrawState);
  procedure HexDumpDisplaySelectCell(Sender: TObject; aCol, aRow: Integer;
   var CanSelect: Boolean);
  procedure HexDumpDisplaySetEditText(Sender: TObject; ACol, ARow: Integer;
   const Value: string);
  procedure HexDumpDisplayValidateEntry(sender: TObject; aCol, aRow: Integer;
   const OldValue: string; var NewValue: String);
 private
  MainFile,
  CompareFile     : TFileStream;
  MainFileName,
  CompareFileName : String;
 public

 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
var
 c: Integer;
begin
 HexDumpDisplay.ColWidths[0]:=75;
 HexDumpDisplay.Cells[0,0]:='Address';
 HexDumpDisplay.ColWidths[17]:=115;
 HexDumpDisplay.Cells[17,0]:='ASCII';
 for c:=1 to 16 do
 begin
  HexDumpDisplay.ColWidths[c]:=25;
  HexDumpDisplay.Cells[c,0]:=IntToHex(c-1,2);
 end;
 MainFilename:='';
 btnMoveUp.Enabled:=False;
 btnMoveDown.Enabled:=False;
 btnMoveUpLine.Enabled:=False;
 btnMoveDownLine.Enabled:=False;
 btnMoveToTop.Enabled:=False;
 btnMoveToBottom.Enabled:=False;
 edJump.Enabled:=False;
 btnSaveText.Enabled:=False;
 btnLoadCompare.Enabled:=False;
 Caption:=Application.Title+' v3.00 by Gerald J Holdsworth';
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
 if OpenFile.Execute then
 begin
  if MainFilename<>'' then MainFile.Free;
  if CompareFilename<>'' then
  begin
   CompareFile.Free;
   CompareFilename:='';
   StatusBar.Panels[2].Text:='';
  end;
  MainFilename:=OpenFile.Filename;
  StatusBar.Panels[0].Text:=ExtractFileName(MainFilename);
  MainFile:=TFileStream.Create(MainFilename,fmOpenReadWrite);
  StatusBar.Panels[1].Text:=IntToHex(MainFile.Size,10);
  DisplayHex(0);
  btnMoveUp.Enabled:=True;
  btnMoveDown.Enabled:=True;
  btnMoveUpLine.Enabled:=True;
  btnMoveDownLine.Enabled:=True;
  btnMoveToTop.Enabled:=True;
  btnMoveToBottom.Enabled:=True;
  edJump.Enabled:=True;
  btnSaveText.Enabled:=True;
  ScrollBar1.Max:=MainFile.Size;
  ScrollBar1.Min:=0;
  ScrollBar1.Position:=0;
  btnLoadCompare.Enabled:=True;
 end;
end;

procedure TMainForm.edXORKeyPress(Sender: TObject; var Key: char);
begin
 if Ord(Key)=13 then
 begin
  edXOR.Text:=IntToHex(StrToIntDef('$'+edXOR.Text,0),2);
  HexDumpDisplay.SetFocus;
  DisplayHex(StrToIntDef('$'+HexDumpDisplay.Cells[0,1],0));
 end
 else
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0;
end;

procedure TMainForm.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
 var ScrollPos: Integer);
begin
 DisplayHex((ScrollPos div $10)*$10);
end;

procedure TMainForm.WriteLine(F: TFileStream;line: String);
begin
 line:=line+#$0D+#$0A;
 F.Write(line[1],Length(line));
end;

procedure TMainForm.btnSaveTextClick(Sender: TObject);
var
 line: String;
 F: TFileStream;
 buffer: array of Byte;
 p,len: Byte;
begin
 SaveFile.Filename:=MainFilename+'-dump.txt';
 if SaveFile.Execute then
 begin
  F:=TFileStream.Create(SaveFile.Filename,fmCreate);
  F.Position:=0;
  MainFile.Position:=0;
  SetLength(buffer,$10);
  WriteLine(F,MainForm.Caption);
  WriteLine(F,'https://www.geraldholdsworth.co.uk https://github.com/geraldholdsworth/FileDump');
  WriteLine(F,'');
  WriteLine(F,'Filename      : '+MainFilename);
  WriteLine(F,'Total Filesize: '+IntToStr(MainFile.Size)
                   +' (0x'+IntToHex(MainFile.Size,10)+') bytes');
  WriteLine(F,'');
  WriteLine(F,'Address     00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII');
  repeat
   line:=IntToHex((MainFile.Position div $10)*$10,10)+'  ';
   len:=$10;
   if MainFile.Position+len>MainFile.Size then
    len:=MainFile.Size-MainFile.Position;
   if len>0 then
   begin
    MainFile.Read(buffer[0],len);
    for p:=0 to len-1 do
    begin
     line:=line+IntToHex(buffer[p],2)+' ';
     if p=$07 then line:=line+' ';
    end;
    line:=line+' ';
    for p:=0 to len-1 do
     if (buffer[p]>31) AND (buffer[p]<127) then line:=line+chr(buffer[p])
     else line:=line+'.';
    WriteLine(F,line);
   end;
  until MainFile.Position=MainFile.Size;
  F.Free;
 end;
end;

procedure TMainForm.DisplayHex(start: Cardinal);
var
 rows,
 line  : Cardinal;
 ch,
 len,
 key   : Byte;
 buffer: array of Byte;
 chars : String;
begin
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-1;
 key:=StrToIntDef('$'+edXOR.Text,0);
 if start+(rows*$10)>MainFile.Size then
 begin
  if MainFile.Size-(rows*$10)>=0 then start:=MainFile.Size-(rows*$10)
  else
  begin
   start:=0;
   rows:=MainFile.Size div $10;
   if MainFile.Size mod $10>0 then inc(rows);
  end;
 end;
 start:=(start div $10)*$10;
 MainFile.Position:=start;
 ScrollBar1.Position:=start;
 SetLength(buffer,$10);
 HexDumpDisplay.RowCount:=rows+1;
 line:=0;
 repeat
  if MainFile.Position+$10<MainFile.Size then len:=$10
  else len:=MainFile.Size-MainFile.Position;
  MainFile.Read(buffer[0],len);
  chars:='';
  HexDumpDisplay.Cells[0,line+1]:=IntToHex(start+line*$10,10);
  for ch:=0 to len-1 do
  begin
   HexDumpDisplay.Cells[ch+1,line+1]:=IntToHex(buffer[ch]XOR key,2);
   if (buffer[ch]XOR key>31) AND (buffer[ch]XOR key<127) then
    chars:=chars+Chr(buffer[ch]XOR key)
   else
    chars:=chars+'.';
  end;
  HexDumpDisplay.Cells[17,line+1]:=chars;
  inc(line);
 until (MainFile.Position=MainFile.Size)
 or (line+1=HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight);
end;

procedure TMainForm.edJumpKeyPress(Sender: TObject; var Key: char);
begin
 if Ord(Key)=13 then
 begin
  DisplayHex(StrToIntDef('$'+edJump.Text,0));
  edJump.Text:=IntToHex(StrToIntDef('$'+edJump.Text,0),10);
  HexDumpDisplay.SetFocus;
 end
 else
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0;
end;

procedure TMainForm.btnMoveUpClick(Sender: TObject);
var
 s,rows: Cardinal;
begin
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 if s-rows<0 then s:=0 else dec(s,rows);
 DisplayHex(s*$10);
end;

procedure TMainForm.btnMoveUpLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 if s>0 then dec(s);
 DisplayHex(s*$10);
end;

procedure TMainForm.btnMoveDownClick(Sender: TObject);
var
 s,rows: Cardinal;
begin
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 inc(s,rows);
 DisplayHex(s*$10);
end;

procedure TMainForm.btnLoadCompareClick(Sender: TObject);
begin
 if CompareFilename<>'' then
 begin
  CompareFile.Free;
  CompareFilename:='';
  StatusBar.Panels[2].Text:='';
  HexDumpDisplay.Repaint;
 end
 else
 if OpenFile.Execute then
 begin
  if CompareFilename<>'' then CompareFile.Free;
  CompareFilename:=OpenFile.Filename;
  StatusBar.Panels[2].Text:=ExtractFileName(CompareFilename);
  CompareFile:=TFileStream.Create(OpenFile.Filename,fmOpenRead);
  CompareFile.Position:=0;
  HexDumpDisplay.Repaint;
 end;
end;

procedure TMainForm.btnMoveDownLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 inc(s);
 DisplayHex(s*$10);
end;

procedure TMainForm.btnMoveToBottomClick(Sender: TObject);
begin
 DisplayHex(MainFile.Size);
end;

procedure TMainForm.btnMoveToTopClick(Sender: TObject);
begin
 DisplayHex(0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 if MainFilename<>'' then MainFile.Free;
 if CompareFilename<>'' then CompareFile.Free;
 CanClose:=True;
end;

procedure TMainForm.FormResize(Sender: TObject);
var
 s,rows: Cardinal;
begin
 if HexDumpDisplay.RowCount>1 then
 begin
  rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight);
  if HexDumpDisplay.RowCount<>rows then
  begin
   s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1]);
   DisplayHex(s);
  end;
 end;
 if MainForm.Width<>625 then MainForm.Width:=625;
end;

procedure TMainForm.HexDumpDisplayGetCellHint(Sender: TObject; ACol,
 ARow: Integer; var HintText: String);
var
 s: Byte;
begin
 s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
 if (s>31) and (s<127) then HintText:=chr(s)
 else HintText:='';
end;

procedure TMainForm.HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
begin
 if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
 and (Key<>chr(127)) AND (Key<>chr(8)) AND (Key<>chr(13)) then Key:=#0;
end;

procedure TMainForm.HexDumpDisplayPrepareCanvas(sender: TObject; aCol,
 aRow: Integer; aState: TGridDrawState);
var
 s,c: Byte;
 pos: Cardinal;
begin
 HexDumpDisplay.Font.Color:=$000000;
 if aRow>0 then
  if (aCol>0) and (aCol<17) then
  begin
   s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
   HexDumpDisplay.Font.Color:=$FF0000;
   if s=0 then HexDumpDisplay.Font.Color:=$0000FF;
   if (s>31) and (s<127) then HexDumpDisplay.Font.Color:=$00AA00;
   HexDumpDisplay.Font.Style:=[];
   if CompareFilename<>'' then
   begin
    pos:=StrToInt('$'+HexDumpDisplay.Cells[0,aRow])+aCol;
    c:=s;
    if pos<CompareFile.Size then c:=CompareFile.ReadByte;
    if c<>s then HexDumpDisplay.Canvas.Brush.Color:=$00FFFF;
    if pos>CompareFile.Size then HexDumpDisplay.Canvas.Brush.Color:=$FFFF00;
   end;
  end
  else
  begin
   HexDumpDisplay.Font.Style:=[fsBold];
   HexDumpDisplay.Canvas.Brush.Color:=HexDumpDisplay.FixedColor;
  end;
end;

procedure TMainForm.HexDumpDisplaySelectCell(Sender: TObject; aCol,
 aRow: Integer; var CanSelect: Boolean);
begin
 if aCol=17 then CanSelect:=False
 else CanSelect:=True;
end;

procedure TMainForm.HexDumpDisplaySetEditText(Sender: TObject; ACol,
 ARow: Integer; const Value: string);
begin
 If Length(value)>2 then
  HexDumpDisplay.Cells[ACol,ARow]:=LeftStr(value,2);
end;

procedure TMainForm.HexDumpDisplayValidateEntry(sender: TObject; aCol,
 aRow: Integer; const OldValue: string; var NewValue: String);
var
 p: Cardinal;
 c: String;
begin
 if aCol<17 then
 begin
  NewValue:=IntToHex(StrToIntDef('$'+NewValue,0),2);
  c:=HexDumpDisplay.Cells[17,aRow];
  if (StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])>31)
  and(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])<127) then
   c[aCol]:=chr(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow]))
  else
   c[aCol]:='.';
  HexDumpDisplay.Cells[17,aRow]:=c;
  p:=StrToIntDef('$'+HexDumpDisplay.Cells[0,aRow],0)+(aCol-1);
  MainFile.Position:=p;
  MainFile.WriteByte(StrToIntDef('$'+NewValue,0));
 end
 else
  NewValue:=OldValue;
end;

end.

