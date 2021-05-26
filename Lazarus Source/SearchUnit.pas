unit SearchUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

 { TSearchForm }

 TSearchForm = class(TForm)
  AbortBtn: TButton;
  edJump: TEdit;
  edTextFind: TEdit;
  edSequence: TEdit;
  Label1: TLabel;
  Label2: TLabel;
  Label3: TLabel;
  SearchProgress: TProgressBar;
  procedure AbortBtnClick(Sender: TObject);
  procedure edJumpKeyPress(Sender: TObject; var Key: char);
  procedure edSequenceKeyPress(Sender: TObject; var Key: char);
  procedure edTextFindKeyPress(Sender: TObject; var Key: char);
  procedure SearchSeq(seq: array of Byte);
  function KeyPressed(var Key: Char): Boolean;
 private
  abort: Boolean;
 public

 end;

var
 SearchForm: TSearchForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TSearchForm }

{                                                                              }
{ User has pressed a key on the Jump edit box                                  }
{                                                                              }
procedure TSearchForm.edJumpKeyPress(Sender: TObject; var Key: char);
begin
 if KeyPressed(Key) then
 begin
  //Update the grid with a valid entry
  MainForm.DisplayHex(StrToIntDef('$'+edJump.Text,0));
  //Update the edit box with a valid entry
  edJump.Text:=IntToHex(StrToIntDef('$'+edJump.Text,0),edJump.MaxLength);
  //Remove focus
  MainForm.HexDumpDisplay.SetFocus;
 end;
end;

{                                                                              }
{ User has pressed a key on the find sequence edit box                         }
{                                                                              }
procedure TSearchForm.edSequenceKeyPress(Sender: TObject; var Key: char);
var
 txt   : String;
 seq   : array of Byte;
 tmp   : array of String;
 i,c   : Integer;
 b     : Char;
begin
 seq:=nil;
 tmp:=nil;
 if(KeyPressed(Key))and(Length(edSequence.Text)>1)then
 begin
  //Split the sequence into pairs of two digits
  txt:='';
  c:=0;
  for i:=0 to Length(edSequence.Text)-1 do
  begin
   b:=edSequence.Text[i+1];
   //Make sure only 0-9 and A-F are in the sequence
   if b in ['0'..'9']+['A'..'F']+['a'..'f'] then
   begin
    txt:=txt+b;
    inc(c);
    //Separate each pair by a space
    if c mod 2=0 then txt:=txt+' ';
   end;
  end;
  if Length(txt)>0 then
  begin
   //Convert to upper case
   txt:=UpperCase(txt);
   //If the last 'pair' is a single digit, add a zero
   if txt[Length(txt)]<>' ' then
   begin
    txt:=txt+' ';
    txt[Length(txt)]:=txt[Length(txt)-1];
    txt[Length(txt)-1]:='0';
   end else txt:=LeftStr(txt,Length(txt)-1); //Otherwise remove the extra space
  end;
  //We check again as the length might have changed since the last check
  if Length(txt)>0 then
  begin
   //Update the sequence entry
   edSequence.Text:=txt;
   //Split the string into an array of strings
   tmp:=txt.Split(' ');
   //Then convert each pair into a byte
   SetLength(seq,Length(tmp));
   for i:=0 to Length(tmp)-1 do
    seq[i]:=StrToInt('$'+tmp[i]);
   //Now do the search
   SearchSeq(seq);
   //Return focus to our control
   edSequence.SetFocus;
  end;
 end;
end;

{                                                                              }
{ User has aborted the search                                                  }
{                                                                              }
procedure TSearchForm.AbortBtnClick(Sender: TObject);
begin
 abort:=True;
end;

{                                                                              }
{ User has pressed a key on the find text edit box                             }
{                                                                              }
procedure TSearchForm.edTextFindKeyPress(Sender: TObject; var Key: char);
var
 i     : Integer;
 seq   : array of Byte;
begin
 seq:=nil;
 if(Key=#13)and(Length(edTextFind.Text)>1)then
 begin
  //Convert the string into a byte sequence
  SetLength(seq,Length(edTextFind.Text));
  for i:=0 to Length(seq)-1 do
   seq[i]:=Ord(edTextFind.Text[i+1]);
  SearchSeq(seq);
  //Return focus to our control
  edTextFind.SetFocus;
 end;
end;

{                                                                              }
{ Search the file for a sequence of bytes                                      }
{                                                                              }
procedure TSearchForm.SearchSeq(seq: array of Byte);
var
 i,j   : Cardinal;
 buffer: array of byte;
 found : Boolean;
 b     : Byte;
begin
 buffer:=nil;
 //Display the searching progress bar and abort button
 SearchProgress.Visible:=True;
 AbortBtn.Visible      :=True;
 //Initialise the progress bar
 SearchProgress.Min:=0;
 SearchProgress.Max:=MainForm.MainFile.Size;
 SearchProgress.Position:=0;
 //And change the cursor to an hourglass
 Cursor:=crHourglass;
 //Disable the controls
 edJump.Enabled    :=False;
 edTextFind.Enabled:=False;
 edSequence.Enabled:=False;
 //Reflect the changes
 Application.ProcessMessages;
 //Set up a buffer the same length as the text
 SetLength(buffer,Length(seq));
 //Initialise the flags
 found:=False;
 abort:=False;
 //Go through the file, starting at the current second from top position
 i:=StrtoInt('$'+MainForm.HexDumpDisplay.Cells[0,2]);
 while(i<MainForm.MainFile.Size)and(not abort)do
 begin
  //Get each byte
  MainForm.MainFile.Position:=i;
  b:=MainForm.MainFile.ReadByte;
  //And compare it to the first byte of the search
  if b=seq[0] then
  begin
   //If it matches, step back and fill the buffer from the file
   MainForm.MainFile.Position:=i;
   MainForm.MainFile.Read(buffer[0],Length(buffer));
   //Does it match?
   found:=True;
   for j:=0 to Length(buffer)-1 do
    if buffer[j]<>seq[j] then found:=False;
   //If found, move the display to the area
   if found then
   begin
    MainForm.DisplayHex(i);
    //Fill the Jump edit box with the address
    edJump.Text:=IntToHex(i,edJump.MaxLength);
    //And exit the loop
    abort:=True;
   end;
  end;
  //Move the pointer on
  inc(i);
  //Update the progress indicator
  if i mod $1000=0 then
  begin
   SearchProgress.Position:=i;
   Application.ProcessMessages;
  end;
 end;
 //Revert back
 SearchProgress.Visible:=False;
 AbortBtn.Visible      :=False;
 Cursor                :=crDefault;
 edJump.Enabled        :=True;
 edTextFind.Enabled    :=True;
 edSequence.Enabled    :=True;
end;

{                                                                              }
{ Generic key press function, checking for hex entry                           }
{                                                                              }
function TSearchForm.KeyPressed(var Key: Char): Boolean;
begin
 Result:=False; //Only returns true if enter is pressed
 if Ord(Key)=13 then Result:=True //13 = CR...i.e. Enter key
 else //Ensure it is a number or A to F (i.e. Hex), or Delete/Backspace
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0; //If not, invalidate
end;

end.
