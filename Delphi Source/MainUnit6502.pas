unit MainUnit6502;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, Math, ComCtrls, Gauges,System.UITypes;

type
  TMainForm6502 = class(TForm)
    lb_FileDump: TListBox;
    lb_Disassembly: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    btn_LoadFile: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    lb_SaveDump: TSpeedButton;
    btn_Disassemble: TSpeedButton;
    lb_Comparison: TSpeedButton;
    Splitter2: TSplitter;
    Label1: TLabel;
    XORwith: TEdit;
    cb_binout: TCheckBox;
    baseAddr: TEdit;
    Label2: TLabel;
    ProgressBar1: TGauge;
    Label3: TLabel;
    btn_abort: TSpeedButton;
    procedure lb_ComparisonClick(Sender: TObject);
    function Pad(A,P: Integer): String;
    procedure btn_DisassembleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lb_SaveDumpClick(Sender: TObject);
    procedure btn_LoadFileClick(Sender: TObject);
    function IntToBin(X: Byte): String;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn_abortClick(Sender: TObject);
  private
    {datafile: TMemoryStream;}
    dumpfname: String;
    closing: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm6502: TMainForm6502;

implementation

const maxsize=1105920;
const AppName='File Dump and Comparison';
const Version='2.09';
const Copyright='(c)2012-15 GJH Software';

{$R *.DFM}

function TMainForm6502.Pad(A,P: Integer): String;
var
 T: String;
begin
  T:='                   '+IntToStr(A);
  Result:=Copy(T,Length(T)-(P-1),P);
end;

procedure TMainForm6502.lb_ComparisonClick(Sender: TObject);
var
 F: String;
 ok: Boolean;
begin
  SaveDialog1.FileName:=dumpfname+'-comparison.txt';
  if SaveDialog1.Execute then
  begin
   ok:=True;
   F:=SaveDialog1.FileName;
   if Copy(F,Length(F)-3,4)<>'.txt' then F:=F+'.txt';
   if FileExists(F) then ok:=MessageDlg(F+' already exists. Overwrite?',mtInformation,[mbYes,mbNo],0)=mrYes;
   if ok then lb_Disassembly.Items.SaveToFile(F);
  end;
end;

procedure TMainForm6502.btn_abortClick(Sender: TObject);
begin
 closing:=True;
end;

procedure TMainForm6502.btn_DisassembleClick(Sender: TObject);
var
  F1,F2: File;
  data1,data2: array[0..30000] of Byte;
  L: String;
  i,j,{totalsize,}size,amt,pos,X: Integer;
begin
 if dumpfname<>'' then
	if OpenDialog1.Execute then
  begin
   closing:=False;
   lb_Disassembly.Clear;
   lb_Comparison.Enabled:=False;
   AssignFile(F1,dumpfname);
   AssignFile(F2,OpenDialog1.FileName);
   Reset(F1,1);
   Reset(F2,1);
   size:=FileSize(F1);
   lb_Disassembly.Items.Clear;
   lb_Disassembly.Items.Add(AppName+' v'+Version+' written by Gerald Holdsworth');
   lb_Disassembly.Items.Add(Copyright);
   lb_Disassembly.Items.Add('');
   lb_Disassembly.Items.Add('Filename 1    : '+dumpfname);
   lb_Disassembly.Items.Add('Filename 2    : '+OpenDialog1.Filename);
   lb_Disassembly.Items.Add('File size 1   : '+IntToStr(size)+' bytes');
   lb_Disassembly.Items.Add('File size 2   : '+IntToStr(FileSize(F2))+' bytes');
   lb_Disassembly.Items.Add('');
   lb_Disassembly.Items.Add('Offset    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  Characters');
   pos:=0;
   repeat
    BlockRead(F1,data1,30000,amt);
    BlockRead(F2,data2,30000,amt);
    i:=0;
    while i<amt do
    begin
     L:=inttohex(pos+i,8)+'  ';
     for j:=0 to 15 do
     begin
      if data1[i+j]>data2[i+j] then
       X:=data1[i+j]-data2[i+j]
      else
       X:=data2[i+j]-data1[i+j];
      L:=L+inttohex(X,2)+' ';
     end;
     L:=L+' ';
     for j:=0 to 15 do
     begin
      if data1[i+j]>data2[i+j] then
       X:=data1[i+j]-data2[i+j]
      else
       X:=data2[i+j]-data1[i+j];
      if (X>31) AND (X<>127) then
       L:=L+chr(X)
      else
       L:=L+'.';
     end;
     lb_Disassembly.Items.Add(L);
     Application.ProcessMessages;
     i:=i+16;
    end;
    pos:=pos+amt;
   until (eof(F1)) OR (eof(F2)) or (closing);
   CloseFile(F1);
   CloseFile(F2);
   lb_Comparison.Enabled:=True;
  end;
end;

procedure TMainForm6502.btn_LoadFileClick(Sender: TObject);
var
  F: File;
  data: array[0..30000] of Byte;
  L: String;
  i,j,{totalsize,}size,amt,pos,X: Integer;
begin
	if OpenDialog1.Execute then
  begin
   closing:=False;
   {datafile.Clear;}
   ProgressBar1.Visible:=True;
   Label3.Visible:=True;
   lb_SaveDump.Enabled:=False;
   lb_Comparison.Enabled:=False;
   ProgressBar1.Progress:=0;
   btn_abort.Visible:=True;
   dumpfname:=OpenDialog1.FileName;
   AssignFile(F,OpenDialog1.Filename);
   Reset(F,1);
   size:=FileSize(F);
   ProgressBar1.MaxValue:=size;
   lb_FileDump.Items.Clear;
   lb_Disassembly.Items.Clear;
   lb_FileDump.Items.Add(AppName+' v'+Version+' written by Gerald Holdsworth');
   lb_FileDump.Items.Add(Copyright);
   lb_FileDump.Items.Add('');
   lb_FileDump.Items.Add('Filename      : '+OpenDialog1.Filename);
   lb_FileDump.Items.Add('Total filesize: '+IntToStr(size)+' (0x'+IntToHex(size,8)+') bytes');
   lb_FileDump.Items.Add('XOR key       : '+XORwith.Text);
   lb_FileDump.Items.Add('');
   if cb_binout.Checked then
   begin
    lb_FileDump.Items.Add('Offset    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F     00       01       02       03       04       05       06       07       08       09       0A       0B       0C       0D       0E       0F     Characters');
    lb_FileDump.Items.Add('Binary                                                     76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210 76543210');
   end
   else
    lb_FileDump.Items.Add('Offset    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  Characters');
   pos:=0;
   repeat
    BlockRead(F,data,30000,amt);
    {datafile.WriteBuffer(data,amt);}
    i:=0;
    while i<amt do
    begin
     L:=inttohex(pos+i+StrToInt(baseAddr.Text),8)+'  ';
     for j:=0 to 15 do
      if pos+i+j<=size then
      begin
       X:=data[i+j] XOR StrtoInt(XORwith.Text);
       L:=L+inttohex(X,2)+' ';
      end
      else
       L:=L+'   ';
     if cb_binout.Checked then
     begin
      L:=L+' ';
      for j:=0 to 15 do
       if pos+i+j<=size then
       begin
        X:=data[i+j] XOR StrtoInt(XORwith.Text);
        L:=L+IntToBin(X)+' ';
       end
      else
       L:=L+'         ';
     end;
     L:=L+' ';
     for j:=0 to 15 do
      if pos+i+j<=size then
      begin
       X:=data[i+j] XOR StrtoInt(XORwith.Text);
       if (X>31) AND (X<>127) then
        L:=L+chr(X)
       else
        L:=L+'.';
      end;
     lb_FileDump.Items.Add(L);
     Application.ProcessMessages;
     i:=i+16;
     ProgressBar1.Progress:=pos+i;
    end;
    pos:=pos+amt;
   until (eof(F)) or (closing);
   CloseFile(F);
   ProgressBar1.Visible:=False;
   Label3.Visible:=False;
   btn_Disassemble.Enabled:=True;
   lb_SaveDump.Enabled:=True;
   btn_abort.Visible:=False;
  end;
end;

procedure TMainForm6502.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 closing:=True;
end;

procedure TMainForm6502.FormCreate(Sender: TObject);
begin
  {datafile:=TMemoryStream.Create;}
  dumpfname:='';
  lb_FileDump.Items.Clear;
  lb_Disassembly.Items.Clear;
  Caption:=AppName+' v'+Version+' written by Gerald Holdsworth';
  closing:=False;
end;

procedure TMainForm6502.lb_SaveDumpClick(Sender: TObject);
var
 F: String;
 ok: Boolean;
begin
  SaveDialog1.FileName:=dumpfname+'-dump.txt';
  if SaveDialog1.Execute then
  begin
   ok:=True;
   F:=SaveDialog1.FileName;
   if Copy(F,Length(F)-3,4)<>'.txt' then F:=F+'.txt';
   if FileExists(F) then ok:=MessageDlg(F+' already exists. Overwrite?',mtInformation,[mbYes,mbNo],0)=mrYes;
   if ok then lb_FileDump.Items.SaveToFile(F);
  end;
end;

function TMainForm6502.IntToBin(X: Byte): String;
var
 S: String;
 b: Integer;
begin
 S:='';
 for b:=7 downto 0 do
  S:=S+IntToStr((X AND Trunc(IntPower(2,b))) shr b);
 Result:=S;
end;

end.
