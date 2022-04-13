unit SaveTextUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
 ExtCtrls, StrUtils;

type

 { TSaveTextForm }

 TSaveTextForm = class(TForm)
  AbortBtn: TButton;
  pbProgress: TProgressBar;
  Timer1: TTimer;
  procedure AbortBtnClick(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure Timer1Timer(Sender: TObject);
  procedure WriteLine(F: TFileStream;line: String);
 private
  Abort: Boolean;
 public
  TxtFilename : String;
  selectStart,
  selectEnd   : QWord;
  HideZeros   : Boolean;
 end;

var
 SaveTextForm: TSaveTextForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TSaveTextForm }

{                                                                              }
{ Form is showing                                                              }
{                                                                              }
procedure TSaveTextForm.FormShow(Sender: TObject);
begin
 AbortBtn.Enabled:=False;
 //Reset the progress bar
 pbProgress.Position:=0;
 //Start the timer to fire off the main procedure, as we can't run it from here
 Timer1.Enabled:=True;
end;

{                                                                              }
{ The save routine                                                             }
{                                                                              }
procedure TSaveTextForm.Timer1Timer(Sender: TObject);
var
 line  : String;
 F     : TFileStream;
 buffer: array of Byte;
 p,len : Byte;
const
  webaddr1 = 'https://www.geraldholdsworth.co.uk';
  webaddr2 = 'https://github.com/geraldholdsworth/FileDump';
begin
 buffer:=nil;
 //Set the abort flag to false
 Abort:=False;
 //Disable the timer
 Timer1.Enabled:=False;
 //Enable the abort button
 AbortBtn.Enabled:=True;
 //Create a new file (overwrite one if already exists)
 F:=TFileStream.Create(TxtFilename,fmCreate);
 //Set to start of file
 F.Position:=0;
 //Go to the position of the main file
 MainForm.MainFile.Position:=selectStart;
 //We will read in 16 bytes at a time
 SetLength(buffer,$10);
 //Write out the header
 WriteLine(F,MainForm.Caption);
 //Web addresses
 WriteLine(F,webaddr1+' '+webaddr2);
 WriteLine(F,'');
 //Filename
 WriteLine(F,'Filename      : '+MainForm.MainFilename);
 //Filesize
 WriteLine(F,'Total Filesize: '+IntToStr(MainForm.MainFile.Size)
                  +' (0x'+IntToHex(MainForm.MainFile.Size,10)+') bytes');
 WriteLine(F,'');
 //Offset header
 line:='Address     ';
 for p:=0 to 15 do
 begin
  line:=line+IntToHex(p,2)+' ';
  if p=7 then line:=line+' '; //Extra space after 8th byte
 end;
 WriteLine(F,line+' ASCII');
 //Now the data
 repeat
  //Start the line off with the address, in hex, 10 digits long
  line:=IntToHex((MainForm.MainFile.Position div $10)*$10,10)+'  ';
  //Set the amount of data to read to 16 bytes
  len:=$10;
  //If this will take us over the total size, then adjust accordingly
  if MainForm.MainFile.Position+len>selectEnd then
   len:=selectEnd-MainForm.MainFile.Position;
  //If there is something to read, then read it
  if len>0 then
  begin
   //Read into the buffer the data
   MainForm.MainFile.Read(buffer[0],len);
   //Turn each byte into hex and output
   for p:=0 to len-1 do
   begin
    if(buffer[p]<>0)or(not HideZeros)then line:=line+IntToHex(buffer[p],2)
    else line:=line+'..';
    line:=line+' ';
    if p=$07 then line:=line+' '; //Split in the middle
   end;
   //Ensure the characters are at a set position
   line:=PadRight(line,62);
   //Now the characters
   for p:=0 to len-1 do
    if (buffer[p]>31) AND (buffer[p]<127) then
     line:=line+chr(buffer[p]) //Printable
    else
     line:=line+'.'; //Not printable
   //Write out the complete line
   WriteLine(F,line);
  end;
  //Update the progress bar
  pbProgress.Position:=Round(
        ((MainForm.MainFile.Position-selectStart)/(selectEnd-selectStart))*100);
  Application.ProcessMessages;
  //Continue until no more data
 until(MainForm.MainFile.Position>=selectEnd)or(Abort);
 //Close the file and exit
 F.Free;
 //Close the window
 ModalResult:=mrOK;
end;

{                                                                              }
{ User has aborted the process                                                 }
{                                                                              }
procedure TSaveTextForm.AbortBtnClick(Sender: TObject);
begin
 Abort:=True;
 AbortBtn.Enabled:=False;
 Application.ProcessMessages;
end;

{                                                                              }
{ Write a line of text to a text file                                          }
{                                                                              }
procedure TSaveTextForm.WriteLine(F: TFileStream;line: String);
begin
 line:=line+#$0D+#$0A; //Ensure CR/LF is added
 F.Write(line[1],Length(line)); //And write it out
end;

end.
