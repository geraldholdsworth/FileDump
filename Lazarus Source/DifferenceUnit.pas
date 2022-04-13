unit DifferenceUnit;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
 ComCtrls;

type

 { TDifferenceForm }

 TDifferenceForm = class(TForm)
  btnAbort: TButton;
  Output: TMemo;
  Panel1: TPanel;
  Progress: TProgressBar;
  Timer1: TTimer;
  procedure btnAbortClick(Sender: TObject);
  procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  procedure FormShow(Sender: TObject);
  procedure Timer1Timer(Sender: TObject);
 private
  abort: Boolean;
 public

 end;

var
 DifferenceForm: TDifferenceForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TDifferenceForm }

procedure TDifferenceForm.FormShow(Sender: TObject);
begin
 Timer1.Enabled:=True;
end;

procedure TDifferenceForm.Timer1Timer(Sender: TObject);
var
 pos,
 eof        : Cardinal;
 MainBuffer,
 CompBuffer : array of Byte;
 m,c,i      : Word;
begin
 Timer1.Enabled:=False;
 Output.Clear;
 Panel1.Visible:=True;
 //Filename
 Output.Lines.Add('Filename 1     : '+MainForm.MainFilename);
 Output.Lines.Add('Filename 2     : '+MainForm.CompareFilename);
 //Filesizes
 eof:=MainForm.MainFile.Size;
 if MainForm.MainFile.Size>MainForm.CompareFile.Size then
  Output.Lines.Add('Size difference: '
                   +IntToStr(MainForm.MainFile.Size-MainForm.CompareFile.Size)
                   +' bytes bigger');
 if MainForm.MainFile.Size<MainForm.CompareFile.Size then
 begin
  Output.Lines.Add('Size difference: '
                   +IntToStr(MainForm.CompareFile.Size-MainForm.MainFile.Size)
                   +' bytes smaller');
  eof:=MainForm.CompareFile.Size;
 end;
 //Now list the differences
 Output.Lines.Add('Address     Difference');
 Output.Lines.Add('----------------------');
 abort:=False;
 pos:=0;
 MainBuffer:=nil;
 CompBuffer:=nil;
 SetLength(MainBuffer,$100);
 SetLength(CompBuffer,$100);
 while(pos<eof)and(not abort)do
 begin
  Progress.Position:=Round((pos/eof)*100);
  MainForm.MainFile.Position:=pos;
  MainForm.CompareFile.Position:=pos;
  m:=MainForm.MainFile.Read(MainBuffer[0],$100);
  c:=MainForm.CompareFile.Read(CompBuffer[0],$100);
  if m>c then m:=c;
  if m>0 then
   for i:=0 to m-1 do
    if MainBuffer[i]<>CompBuffer[i] then
     Output.Lines.Add(IntToHex(pos+i,10)+'  '
                     +IntToHex(MainBuffer[i],2)+' '
                     +IntToHex(CompBuffer[i],2));
  inc(pos,m);
  Application.ProcessMessages;
  if m=0 then abort:=True;
 end;
 Panel1.Visible:=False;
end;

procedure TDifferenceForm.btnAbortClick(Sender: TObject);
begin
 abort:=True;
end;

procedure TDifferenceForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean
 );
begin
 abort:=True;
 CanClose:=True;
end;

end.

