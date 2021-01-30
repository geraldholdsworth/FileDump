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
  pbProgress: TProgressBar;
  SaveFile: TSaveDialog;
  ScrollBar1: TScrollBar;
  btnLoadCompare: TSpeedButton;
  btnCloseFile: TSpeedButton;
  StatusBar: TStatusBar;
  procedure btnCloseFileClick(Sender: TObject);
  procedure btnLoadCompareClick(Sender: TObject);
  procedure btnMoveDownClick(Sender: TObject);
  procedure btnMoveDownLineClick(Sender: TObject);
  procedure btnMoveToBottomClick(Sender: TObject);
  procedure btnMoveToTopClick(Sender: TObject);
  procedure btnMoveUpClick(Sender: TObject);
  procedure btnMoveUpLineClick(Sender: TObject);
  procedure btnOpenClick(Sender: TObject);
  procedure LoadFile(filename: String);
  procedure edXORKeyPress(Sender: TObject; var Key: char);
  procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
   var ScrollPos: Integer);
  procedure WriteLine(F: TFileStream;line: String);
  procedure btnSaveTextClick(Sender: TObject);
  procedure DisplayHex(start: Cardinal);
  procedure edJumpKeyPress(Sender: TObject; var Key: char);
  procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure ResetApplication;
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

{                                                                              }
{ Procedure to run when the application first runs                             }
{                                                                              }
procedure TMainForm.FormShow(Sender: TObject);
var
 c: Integer;
begin
 //Set up the String Grid
 HexDumpDisplay.FixedCols:=1;
 HexDumpDisplay.FixedRows:=1;
 HexDumpDisplay.RowCount :=1;
 //Header
 HexDumpDisplay.ColWidths[0] :=80;
 HexDumpDisplay.Cells[0,0]   :='Address';
 HexDumpDisplay.ColWidths[17]:=120;
 HexDumpDisplay.Cells[17,0]  :='ASCII';
 for c:=1 to 16 do
 begin
  HexDumpDisplay.ColWidths[c]:=25;
  HexDumpDisplay.Cells[c,0]  :=IntToHex(c-1,2);
 end;
 ResetApplication;
 //Set up the form
 MainForm.Width:=635;
 Caption:=Application.Title+' v3.04 by Gerald J Holdsworth';
end;

{                                                                              }
{ Reset the application                                                        }
{                                                                              }
procedure TMainForm.ResetApplication;
begin
 //Reset the file's filename tracker
 MainFilename:='';
 //Disable the controls
 btnMoveUp.Enabled      :=False;
 btnMoveDown.Enabled    :=False;
 btnMoveUpLine.Enabled  :=False;
 btnMoveDownLine.Enabled:=False;
 btnMoveToTop.Enabled   :=False;
 btnMoveToBottom.Enabled:=False;
 edJump.Enabled         :=False;
 edXOR.Enabled          :=False;
 btnSaveText.Enabled    :=False;
 btnCloseFile.Enabled   :=False;
 btnLoadCompare.Enabled :=False;
 ScrollBar1.Enabled     :=False;
 //Empty the grid
 HexDumpDisplay.RowCount:=1;
end;

{                                                                              }
{ Procedure to run when the open button is clicked                             }
{                                                                              }
procedure TMainForm.btnOpenClick(Sender: TObject);
begin
 //Open the dialogue box
 if OpenFile.Execute then
  LoadFile(OpenFile.FileName);
end;

{                                                                              }
{ Load a file                                                                  }
{                                                                              }
procedure TMainForm.LoadFile(filename: String);
begin
 //If the files are already open, then close them
 if MainFilename<>'' then MainFile.Free;
 if CompareFilename<>'' then
 begin
  CompareFile.Free;
  //And reset the variables
  CompareFilename:='';
  StatusBar.Panels[2].Text:='';
 end;
 try
  //And open it, as in Read and Write mode
  MainFile:=TFileStream.Create(filename,fmOpenReadWrite OR fmShareDenyNone);
  //Set the main file's filename
  MainFilename:=filename;
  StatusBar.Panels[0].Text:=ExtractFileName(MainFilename);
  //Display the size, in hex
  StatusBar.Panels[1].Text:=IntToHex(MainFile.Size,10);
  //Enable the controls
  btnMoveUp.Enabled      :=True;
  btnMoveDown.Enabled    :=True;
  btnMoveUpLine.Enabled  :=True;
  btnMoveDownLine.Enabled:=True;
  btnMoveToTop.Enabled   :=True;
  btnMoveToBottom.Enabled:=True;
  edJump.Enabled         :=True;
  edXOR.Enabled          :=True;
  btnSaveText.Enabled    :=True;
  btnCloseFile.Enabled   :=True;
  btnLoadCompare.Enabled :=True;
  //Setup the scrollbar
  ScrollBar1.Max:=MainFile.Size;
  ScrollBar1.Min:=0;
  ScrollBar1.Position:=0;
  ScrollBar1.Enabled:=True;
 except
  ShowMessage('Could not open file "'+filename+'"');
 end;
 //Populate the grid with the start of the file
 DisplayHex(0);
end;

{                                                                              }
{ Procedure to run when the user presses a key while in the XOR key box        }
{                                                                              }
procedure TMainForm.edXORKeyPress(Sender: TObject; var Key: char);
begin
 //13 = CR...i.e. Enter key
 if Ord(Key)=13 then //Action the result
 begin
  //Validate the entry
  edXOR.Text:=IntToHex(StrToIntDef('$'+edXOR.Text,0),2);
  //Remove focus from the control
  HexDumpDisplay.SetFocus;
  //Refresh the grid
  DisplayHex(StrToIntDef('$'+HexDumpDisplay.Cells[0,1],0));
 end
 else //Ensure it is a number or A to F (i.e. Hex), or Delete/Backspace
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0; //If not, invalidate
end;

{                                                                              }
{ User has dropped a file onto the window                                      }
{                                                                              }
procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 LoadFile(FileNames[0]);
end;

{                                                                              }
{ User has pressed a key on the Jump edit box                                  }
{                                                                              }
procedure TMainForm.edJumpKeyPress(Sender: TObject; var Key: char);
begin
 //13 = CR...i.e. Enter key
 if Ord(Key)=13 then //Action the result
 begin
  //Update the grid with a valid entry
  DisplayHex(StrToIntDef('$'+edJump.Text,0));
  //Update the edit box with a valid entry
  edJump.Text:=IntToHex(StrToIntDef('$'+edJump.Text,0),10);
  //Remove focus
  HexDumpDisplay.SetFocus;
 end
 else //Ensure it is a number or A to F (i.e. Hex), or Delete/Backspace
  if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
  and (Key<>chr(127)) AND (Key<>chr(8)) then Key:=#0; //If not, invalidate
end;

{                                                                              }
{ Procedure to run when the scroll bar moves                                   }
{                                                                              }
procedure TMainForm.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
 var ScrollPos: Integer);
begin
 //Just move the display on to the position dictated by the scroll bar
 DisplayHex((ScrollPos div $10)*$10);
end;

{                                                                              }
{ Write a line of text to a text file                                          }
{                                                                              }
procedure TMainForm.WriteLine(F: TFileStream;line: String);
begin
 line:=line+#$0D+#$0A; //Ensure CR/LF is added
 F.Write(line[1],Length(line)); //And write it out
end;

{                                                                              }
{ User has clicked on the Save As Text File button                             }
{                                                                              }
procedure TMainForm.btnSaveTextClick(Sender: TObject);
var
 line: String;
 F: TFileStream;
 buffer: array of Byte;
 p,len: Byte;
begin
 buffer:=nil;
 //Adapt the filename
 SaveFile.Filename:=MainFilename+'-dump.txt';
 //And open the dialogue box
 if SaveFile.Execute then
 begin
  //Show the progress bar
  pbProgress.Visible:=True;
  pbProgress.Position:=0;
  //Create a new file (overwrite one if already exists)
  F:=TFileStream.Create(SaveFile.Filename,fmCreate);
  //Set to start of file
  F.Position:=0;
  //Go to the position of the main file
  MainFile.Position:=0;
  //We will read in 16 bytes at a time
  SetLength(buffer,$10);
  //Write out the header
  WriteLine(F,MainForm.Caption);
  WriteLine(F,'https://www.geraldholdsworth.co.uk https://github.com/geraldholdsworth/FileDump');
  WriteLine(F,'');
  WriteLine(F,'Filename      : '+MainFilename);
  WriteLine(F,'Total Filesize: '+IntToStr(MainFile.Size)
                   +' (0x'+IntToHex(MainFile.Size,10)+') bytes');
  WriteLine(F,'');
  WriteLine(F,'Address     00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F  ASCII');
  //Now the data
  repeat
   //Start the line off with the address, in hex, 10 digits long
   line:=IntToHex((MainFile.Position div $10)*$10,10)+'  ';
   //Set the amount of data to read to 16 bytes
   len:=$10;
   //If this will take us over the total size, then adjust accordingly
   if MainFile.Position+len>MainFile.Size then
    len:=MainFile.Size-MainFile.Position;
   //If there is something to read, then read it
   if len>0 then
   begin
    //Read into the buffer the data
    MainFile.Read(buffer[0],len);
    //Turn each byte into hex and output
    for p:=0 to len-1 do
    begin
     line:=line+IntToHex(buffer[p],2)+' ';
     if p=$07 then line:=line+' '; //Split in the middle
    end;
    //Extra space to separate from the characters
    line:=line+' ';
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
   pbProgress.Position:=Round((MainFile.Position/MainFile.Size)*100);
   Application.ProcessMessages;
   //Continue until no more data
  until MainFile.Position=MainFile.Size;
  //Close the file and exit
  F.Free;
  //Hide the progress bar
  pbProgress.Visible:=False;
 end;
end;

{                                                                              }
{ Populates the grid, starting at address 'start'                              }
{                                                                              }
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
 buffer:=nil;
 if MainFilename='' then exit; //No file open, then leave
 //How many rows are visible on the form?
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-1;
 if rows=0 then exit; //None, then leave
 //Get the XOR key
 key:=StrToIntDef('$'+edXOR.Text,0);
 //Will this take us beyond the end of the file?
 if start+(rows*$10)>MainFile.Size then
 begin
  //Then adjust accordingly
  if MainFile.Size-(rows*$10)>=0 then
   start:=MainFile.Size-(rows*$10)
  else
  begin //If the entire file is smaller than the space available, adjust
   start:=0;
   rows:=MainFile.Size div $10;
   if MainFile.Size mod $10>0 then inc(rows);
  end;
 end;
 //Ensure the start address is on a 16-byte boundary
 start:=(start div $10)*$10;
 //Position ourselves in the file
 MainFile.Position:=start;
 //And the scroll bar
 ScrollBar1.Position:=start;
 //Setup the array for 16 bytes max
 SetLength(buffer,$10);
 //Make sure there are the appropriate number of rows
 HexDumpDisplay.RowCount:=rows+1; //+1 is the header 
 //Start at line 0
 line:=0;
 repeat
  //We will be reading in 16 bytes at a time
  if MainFile.Position+$10<MainFile.Size then len:=$10
  else len:=MainFile.Size-MainFile.Position; //Unless we run out of bytes
  //Read them in
  MainFile.Read(buffer[0],len);
  //Clear the character column string
  chars:='';
  //Display the address in the first column - to 10 digits
  HexDumpDisplay.Cells[0,line+1]:=IntToHex(start+line*$10,10);
  //Go through the data just read in
  for ch:=0 to len-1 do
  begin
   //Display each one as hex - colours are dealt with elsewhere
   HexDumpDisplay.Cells[ch+1,line+1]:=IntToHex(buffer[ch]XOR key,2);
   //Add add to the character column
   if (buffer[ch]XOR key>31) AND (buffer[ch]XOR key<127) then
    chars:=chars+Chr(buffer[ch]XOR key) //Printable
   else
    chars:=chars+'.';                   //Not printable
  end;
  //Display the characters in the final coluumn
  HexDumpDisplay.Cells[17,line+1]:=chars;
  //And move onto the next line
  inc(line);
 until (MainFile.Position=MainFile.Size) //Continue until the end of the file
 or (line+1=HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight); //Or form
end;

{                                                                              }
{ Navigation buttons - move up a page                                          }
{                                                                              }
procedure TMainForm.btnMoveUpClick(Sender: TObject);
var
 s,rows: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //And the number of rows available
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 //Move towards the beginning of the file (or start if close enough)
 if s-rows<0 then s:=0 else dec(s,rows);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move up a line                                          }
{                                                                              }
procedure TMainForm.btnMoveUpLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Can we move up?
 if s>0 then dec(s);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move down a page                                        }
{                                                                              }
procedure TMainForm.btnMoveDownClick(Sender: TObject);
var
 s,rows: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //And the number of rows available
 rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight)-2;
 //And we'll move down towards the end of the file
 inc(s,rows);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move down a line                                        }
{                                                                              }
procedure TMainForm.btnMoveDownLineClick(Sender: TObject);
var
 s: Cardinal;
begin
 //Get the current top position
 s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1])div$10;
 //Move down one line
 inc(s);
 //Update the display
 DisplayHex(s*$10);
end;

{                                                                              }
{ Navigation buttons - move to the end of the file                             }
{                                                                              }
procedure TMainForm.btnMoveToBottomClick(Sender: TObject);
begin
 //Just update the display with the filesize
 DisplayHex(MainFile.Size);
end;

{                                                                              }
{ Navigation buttons - move to the beginning of the file                       }
{                                                                              }
procedure TMainForm.btnMoveToTopClick(Sender: TObject);
begin
 //Just update the display with zero
 DisplayHex(0);
end;

{                                                                              }
{ User is loading a file to compare                                            }
{                                                                              }
procedure TMainForm.btnLoadCompareClick(Sender: TObject);
begin
 //Is there one already open?
 if CompareFilename<>'' then
 begin
  //Then close it
  CompareFile.Free;
  //And update the tracking variables
  CompareFilename:='';
  StatusBar.Panels[2].Text:='';
  //And refresh the grid
  HexDumpDisplay.Repaint;
 end
 else
 //Othewise, open the dialogue box
 if OpenFile.Execute then
 begin
  //Make sure we don't have a file already open
  if CompareFilename<>'' then CompareFile.Free;
  try
   //Open the file as Read Only
   CompareFile:=TFileStream.Create(OpenFile.Filename,fmOpenRead OR fmShareDenyNone);
   //And position at the start (this is not actually required)
   CompareFile.Position:=0;
   //Update the tracking variable
   CompareFilename:=OpenFile.Filename;
   //And the status bar
   StatusBar.Panels[2].Text:=ExtractFileName(CompareFilename);
   //Refresh the grid
   HexDumpDisplay.Repaint;
  except
   ShowMessage('Could not open file "'+OpenFile.Filename+'"');
  end;
  //Refresh the grid
  HexDumpDisplay.Repaint;
 end;
end;

{                                                                              }
{ User is closing the file                                                     }
{                                                                              }
procedure TMainForm.btnCloseFileClick(Sender: TObject);
begin
 ResetApplication;
 StatusBar.Panels[0].Text:='';
 StatusBar.Panels[1].Text:='';
 StatusBar.Panels[2].Text:='';
 MainFile.Free;
 if CompareFilename<>'' then CompareFile.Free;
 CompareFilename:='';
end;

{                                                                              }
{ User is closing the application                                              }
{                                                                              }
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 //Close any open files
 if MainFilename<>'' then MainFile.Free;
 if CompareFilename<>'' then CompareFile.Free;
 //Signal the OK to close
 CanClose:=True;
end;

{                                                                              }
{ User is resizing the form                                                    }
{                                                                              }
procedure TMainForm.FormResize(Sender: TObject);
var
 s,rows: Cardinal;
begin
 //Have we got any rows displayed?
 if HexDumpDisplay.RowCount>1 then
 begin
  //Then find out how many we can now display
  rows:=(HexDumpDisplay.Height div HexDumpDisplay.DefaultRowHeight);
  //Has it changed?
  if HexDumpDisplay.RowCount<>rows then
  begin
   //Then update the grid with the new number of rows
   s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1]);
   DisplayHex(s);
  end;
 end;
end;

{                                                                              }
{ This doesn't actually appear to do anything                                  }
{                                                                              }
procedure TMainForm.HexDumpDisplayGetCellHint(Sender: TObject; ACol,
 ARow: Integer; var HintText: String);
var
 s: Byte;
begin
 s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
 if (s>31) and (s<127) then HintText:=chr(s)
 else HintText:='';
end;

{                                                                              }
{ User has pressed a key while editing a value on the grid                     }
{                                                                              }
procedure TMainForm.HexDumpDisplayKeyPress(Sender: TObject; var Key: char);
begin
 //Verify it is in hex, delete, backspace or Enter
 if not(Key in ['0'..'9']+['A'..'F']+['a'..'f'])
 and (Key<>chr(127)) AND (Key<>chr(8)) AND (Key<>chr(13)) then Key:=#0;
end;

{                                                                              }
{ This recolours the individual cells                                          }
{                                                                              }
procedure TMainForm.HexDumpDisplayPrepareCanvas(sender: TObject; aCol,
 aRow: Integer; aState: TGridDrawState);
var
 s,c: Byte;
 pos: Cardinal;
begin
 //Default font colour is black, if everything else fails
 HexDumpDisplay.Font.Color:=$000000;
 //We're only colouring below the header row
 if aRow>0 then
  //And the hex cells
  if (aCol>0) and (aCol<17) then
  begin
   //Get the value in the cell
   s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
   //Default font colour is Blue
   HexDumpDisplay.Font.Color:=$FF0000;
   //If it is zero, then it is Red
   if s=0 then HexDumpDisplay.Font.Color:=$0000FF;
   //If it is printable, then it is Green (darkish shade)
   if (s>31) and (s<127) then HexDumpDisplay.Font.Color:=$00AA00;
   //No styles are being applied
   HexDumpDisplay.Font.Style:=[];
   //We are also needing to recolour the background, if we are comparing
   if CompareFilename<>'' then
   begin
    //So get the position within the file
    pos:=StrToInt('$'+HexDumpDisplay.Cells[0,aRow])+aCol;
    //Start off with a match
    c:=s;
    if pos-1<CompareFile.Size then
    begin
     //Move to position within the comparison file
     CompareFile.Position:=pos-1;
     //And read the byte
     c:=CompareFile.ReadByte;
    end;
    //If they don't match, colour the background Yellow
    if c<>s then HexDumpDisplay.Canvas.Brush.Color:=$00FFFF;
    //If we are beyond the end of the comparison file, colour the background Cyan
    if pos>CompareFile.Size then HexDumpDisplay.Canvas.Brush.Color:=$FFFF00;
   end;
  end
  else
  begin
   //Colour the character column to match the header and address
   HexDumpDisplay.Font.Style:=[fsBold];
   HexDumpDisplay.Canvas.Brush.Color:=HexDumpDisplay.FixedColor;
  end;
end;

{                                                                              }
{ User is trying to edit a cell                                                }
{                                                                              }
procedure TMainForm.HexDumpDisplaySelectCell(Sender: TObject; aCol,
 aRow: Integer; var CanSelect: Boolean);
begin
 //We can only edit cells 1 to 16, and not the character column
 if aCol=17 then CanSelect:=False
 else CanSelect:=True;
end;

{                                                                              }
{ This fires everytime a change is made                                        }
{                                                                              }
procedure TMainForm.HexDumpDisplaySetEditText(Sender: TObject; ACol,
 ARow: Integer; const Value: string);
begin
 //We need to make sure that we don't have more than 2 character length
 If Length(value)>2 then
  HexDumpDisplay.Cells[ACol,ARow]:=LeftStr(value,2);
end;

{                                                                              }
{ User has finished editing the cell, so now validate                          }
{                                                                              }
procedure TMainForm.HexDumpDisplayValidateEntry(sender: TObject; aCol,
 aRow: Integer; const OldValue: string; var NewValue: String);
var
 p: Cardinal;
 c: String;
begin
 //We can only edit cells 1 to 16
 if aCol<17 then
 begin
  //Ensure it is a valid hex number, not more that 2 digits
  NewValue:=IntToHex(StrToIntDef('$'+NewValue,0),2);
  //Get the character display text
  c:=HexDumpDisplay.Cells[17,aRow];
  //And update to the new value
  if (StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])>31)
  and(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])<127) then
   c[aCol]:=chr(StrToInt('$'+HexDumpDisplay.Cells[aCol,aRow])) //Printable
  else
   c[aCol]:='.';                                               //Not printable
  //And update the cell
  HexDumpDisplay.Cells[17,aRow]:=c;
  //Work out the position within the file
  p:=StrToIntDef('$'+HexDumpDisplay.Cells[0,aRow],0)+(aCol-1);
  //Move to it
  MainFile.Position:=p;
  //And update it, within the file
  MainFile.WriteByte(StrToIntDef('$'+NewValue,0));
 end
 else //Otherwise, change back to what it was before
  NewValue:=OldValue;
end;

end.

