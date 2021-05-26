unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,
 ExtCtrls, StdCtrls, ComCtrls,Global, Types;

type

 { TMainForm }

 TMainForm = class(TForm)
  btnMoveDown: TSpeedButton;
  btnMoveToBottom: TSpeedButton;
  btnMoveDownLine: TSpeedButton;
  btnMoveToTop: TSpeedButton;
  btnMoveUpLine: TSpeedButton;
  edXOR: TEdit;
  HexDumpDisplay: TStringGrid;
  ButtonImages: TImageList;
  NavImages: TImageList;
  Label2: TLabel;
  OpenFile: TOpenDialog;
  EntryPanel: TPanel;
  NavigationPanel: TPanel;
  btnMoveUp: TSpeedButton;
  SaveFile: TSaveDialog;
  ScrollBar1: TScrollBar;
  StatusBar: TStatusBar;
  FileModification: TTimer;
  MainToolBar: TToolBar;
  btnOpen: TToolButton;
  btnCloseFile: TToolButton;
  btnSaveText: TToolButton;
  btnLoadCompare: TToolButton;
  btnSearch: TToolButton;
  btnOptions: TToolButton;
  procedure btnCloseFileClick(Sender: TObject);
  procedure btnLoadCompareClick(Sender: TObject);
  procedure btnMoveDownClick(Sender: TObject);
  procedure btnMoveDownLineClick(Sender: TObject);
  procedure btnMoveToBottomClick(Sender: TObject);
  procedure btnMoveToTopClick(Sender: TObject);
  procedure btnMoveUpClick(Sender: TObject);
  procedure btnMoveUpLineClick(Sender: TObject);
  procedure btnOpenClick(Sender: TObject);
  procedure btnOptionsClick(Sender: TObject);
  procedure btnSearchClick(Sender: TObject);
  procedure FileModificationTimer(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure HexDumpDisplayMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  procedure LoadFile(filename: String);
  procedure edXORKeyPress(Sender: TObject; var Key: char);
  procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
   var ScrollPos: Integer);
  procedure btnSaveTextClick(Sender: TObject);
  procedure DisplayHex(start: Cardinal);
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
  //Last modified date of each file
  MainFileDate,
  CompareFileDate : Integer;
 public
  //Configurable colour scheme
  ZeroColour,
  DefaultColour,
  FontColour,
  ASCIIColour,
  DiffColour,
  OverColour,
  HeaderColour,
  AddressColour,
  CharColour,
  CellBGColour,
  AltBGColour    : TColor;
  //Filestreams for the two files
  MainFile,
  CompareFile     : TFileStream;
  //Filenames for the two files
  MainFileName,
  CompareFileName : String;
  //Default colour scheme
  const           // RRGGBB  display colours
   dZeroColour    = $0000FF; //Red - FG colour for any zeros
   dDefaultColour = $FF0000; //Blue - FG default colour for the cells
   dFontColour    = $000000; //Black - FG default colour for other cells
   dASCIIColour   = $00AA00; //Green - FG colour for printable values
   dDiffColour    = $00FFFF; //Yellow - BG colour for different values
   dOverColour    = $FFFF00; //Cyan - BG colour for different length files
   dHeaderColour  = $FFA547; //BG Colour for the display header and address col
   dAddressColour = $FFA547; //BG Colour for the address column;
   dCharColour    = $FF94C7; //BG Colour for the character column
   dCellBGColour  = $F0F0F0; //BG Colour for the cells
   dAltBGColour   = $F0FBFF; //BG Colour for the alternate cells
   //Application version
   AppVersion    = '3.07';
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

uses SaveTextUnit,OptionsUnit,SearchUnit;

{ TMainForm }

{                                                                              }
{ Procedure to run when the application first runs                             }
{                                                                              }
procedure TMainForm.FormShow(Sender: TObject);
var
 c: Integer;
begin
 //Set up the String Grid
 HexDumpDisplay.FixedCols     :=1;
 HexDumpDisplay.FixedRows     :=1;
 HexDumpDisplay.RowCount      :=1;
 HexDumpDisplay.FixedColor    :=HeaderColour;
 HexDumpDisplay.Color         :=CellBGColour;
 HexDumpDisplay.AlternateColor:=AltBGColour;
 HexDumpDisplay.Font.Size     :=Round(8*Monitor.PixelsPerInch/DesignTimePPI);
 //Header
 HexDumpDisplay.ColWidths[0]  :=Round(80*Monitor.PixelsPerInch/DesignTimePPI);
 HexDumpDisplay.Cells[0,0]    :='Address';
 HexDumpDisplay.ColWidths[17] :=Round(120*Monitor.PixelsPerInch/DesignTimePPI);
 HexDumpDisplay.Cells[17,0]   :='ASCII';
 for c:=1 to 16 do
 begin
  HexDumpDisplay.ColWidths[c] :=Round(25*Monitor.PixelsPerInch/DesignTimePPI);
  HexDumpDisplay.Cells[c,0]   :=IntToHex(c-1,2);
 end;
 ResetApplication;
 //Set up the form
 MainForm.Width:=Round(635*Monitor.PixelsPerInch/DesignTimePPI);
 MainForm.Constraints.MaxWidth:=Round(635*Monitor.PixelsPerInch/DesignTimePPI);
 MainForm.Constraints.MinHeight:=Round(290*Monitor.PixelsPerInch/DesignTimePPI);
 MainForm.Constraints.MinWidth:=Round(635*Monitor.PixelsPerInch/DesignTimePPI);
 Caption:=Application.Title+' v'+AppVersion+' by Gerald J Holdsworth';
 //Size the toolbar
 MainToolBar.Height:=Round(32*Monitor.PixelsPerInch/DesignTimePPI);
 MainToolBar.ButtonHeight:=MainToolBar.Height;
 MainToolBar.ButtonWidth :=MainToolBar.Height;
 MainToolBar.ImagesWidth :=MainToolBar.ButtonWidth;
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
 edXOR.Enabled          :=False;
 btnSaveText.Enabled    :=False;
 btnCloseFile.Enabled   :=False;
 btnLoadCompare.Enabled :=False;
 btnSearch.Enabled      :=False;
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
{ Show the options unit                                                        }
{                                                                              }
procedure TMainForm.btnOptionsClick(Sender: TObject);
begin
 //Show the options form
 OptionsForm.ShowModal;
 //If user clicked on OK then update the display
 if OptionsForm.ModalResult=mrOK then
 begin
  HexDumpDisplay.FixedColor    :=HeaderColour;
  HexDumpDisplay.Color         :=CellBGColour;
  HexDumpDisplay.AlternateColor:=AltBGColour;
  //Save the configurable colour scheme to the registry
  SetRegValI('ZeroColour'   ,ZeroColour);
  SetRegValI('DefaultColour',DefaultColour);
  SetRegValI('FontColour'   ,FontColour);
  SetRegValI('ASCIIColour'  ,ASCIIColour);
  SetRegValI('DiffColour'   ,DiffColour);
  SetRegValI('OverColour'   ,OverColour);
  SetRegValI('HeaderColour' ,HeaderColour);
  SetRegValI('AddressColour',AddressColour);
  SetRegValI('CharColour'   ,CharColour);
  SetRegValI('CellBgColour' ,CellBGColour);
  SetRegValI('AltBGColour'  ,AltBGColour);
 end;
end;

{                                                                              }
{ Open the search form                                                         }
{                                                                              }
procedure TMainForm.btnSearchClick(Sender: TObject);
begin
 SearchForm.Show;
end;

{                                                                              }
{ Check to see if file has been modified                                       }
{                                                                              }
procedure TMainForm.FileModificationTimer(Sender: TObject);
var
 dorefresh: Boolean;
 s        : Cardinal;
begin
 //Whether to check or not
 dorefresh:=False;
 //Check the main file
 if MainFileName<>'' then
  if MainFileDate<>FileAge(MainFileName)       then dorefresh:=True;
 //Check the compared file
 if CompareFileName<>'' then
  if CompareFileDate<>FileAge(CompareFileName) then dorefresh:=True;
 //Now refresh as one of the two files has changed
 if dorefresh then
 begin
  //Get the current top position
  s:=StrtoInt('$'+HexDumpDisplay.Cells[0,1]);
  //Update the display
  DisplayHex(s);
 end;
end;

{                                                                              }
{ Create the form                                                              }
{                                                                              }
procedure TMainForm.FormCreate(Sender: TObject);
begin
 //Retreive the configurable colour scheme from the registry, or use the defaults
 ZeroColour   :=GetRegValI('ZeroColour'   ,dZeroColour);
 DefaultColour:=GetRegValI('DefaultColour',dDefaultColour);
 FontColour   :=GetRegValI('FontColour'   ,dFontColour);
 ASCIIColour  :=GetRegValI('ASCIIColour'  ,dASCIIColour);
 DiffColour   :=GetRegValI('DiffColour'   ,dDiffColour);
 OverColour   :=GetRegValI('OverColour'   ,dOverColour);
 HeaderColour :=GetRegValI('HeaderColour' ,dHeaderColour);
 AddressColour:=GetRegValI('AddressColour',dAddressColour);
 CharColour   :=GetRegValI('CharColour'   ,dCharColour);
 CellBGColour :=GetRegValI('CellBgColour' ,dCellBGColour);
 AltBGColour  :=GetRegValI('AltBGColour'  ,dAltBGColour);
end;

{                                                                              }
{ User is scrolling using the mouse wheel                                      }
{                                                                              }
procedure TMainForm.HexDumpDisplayMouseWheel(Sender: TObject;
 Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean
 );
begin
 if MainFilename<>'' then
 begin
  if WheelDelta<0 then btnMoveDownLineClick(Sender);
  if WheelDelta>0 then btnMoveUpLineClick(Sender);
  Handled:=True;
 end;
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
  edXOR.Enabled          :=True;
  btnSaveText.Enabled    :=True;
  btnCloseFile.Enabled   :=True;
  btnLoadCompare.Enabled :=True;
  btnSearch.Enabled      :=True;
  //Setup the scrollbar
  ScrollBar1.Max     :=MainFile.Size;
  ScrollBar1.Min     :=0;
  ScrollBar1.Position:=0;
  ScrollBar1.Enabled :=True;
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
 if SearchForm.KeyPressed(Key) then
 begin
  //Validate the entry
  edXOR.Text:=IntToHex(StrToIntDef('$'+edXOR.Text,0),2);
  //Remove focus from the control
  HexDumpDisplay.SetFocus;
  //Refresh the grid
  DisplayHex(StrToIntDef('$'+HexDumpDisplay.Cells[0,1],0));
 end;
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
{ Procedure to run when the scroll bar moves                                   }
{                                                                              }
procedure TMainForm.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
 var ScrollPos: Integer);
begin
 //Just move the display on to the position dictated by the scroll bar
 DisplayHex((ScrollPos div $10)*$10);
end;

{                                                                              }
{ User has clicked on the Save As Text File button                             }
{                                                                              }
procedure TMainForm.btnSaveTextClick(Sender: TObject);
begin
 //Adapt the filename
 SaveFile.Filename:=MainFilename+'-dump.txt';
 //And open the dialogue box
 if SaveFile.Execute then
 begin
  //Send the filename to the save text form
  SaveTextForm.TxtFilename:=SaveFile.Filename;
  //And show it, modally - this will do all the hard work
  SaveTextForm.ShowModal;
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
 //Update the last modified date/time
 MainFileDate:=FileAge(MainFilename);
 if CompareFilename<>'' then CompareFileDate:=FileAge(CompareFilename);
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
  begin
   start:=MainFile.Size-(rows*$10);
   //start will be reset to a $10 boundary, so need to move it on if there is more
   if start mod $10>0 then inc(start,$10);
  end
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
  if line+1<HexDumpDisplay.RowCount then
   HexDumpDisplay.Cells[0,line+1]:=IntToHex(start+line*$10,10);
  //Go through the data just read in
  for ch:=0 to len-1 do
  begin
   //Display each one as hex - colours are dealt with elsewhere
   if(ch+1<HexDumpDisplay.ColCount)and(line+1<HexDumpDisplay.RowCount)then
    HexDumpDisplay.Cells[ch+1,line+1]:=IntToHex(buffer[ch]XOR key,2);
   //Add add to the character column
   if (buffer[ch]XOR key>31) AND (buffer[ch]XOR key<127) then
    chars:=chars+Chr(buffer[ch]XOR key) //Printable
   else
    chars:=chars+'.';                   //Not printable
  end;
  //Are there more cells than data?
  if len<$10 then
   for ch:=len to $0F do
    if(ch+1<HexDumpDisplay.ColCount)and(line+1<HexDumpDisplay.RowCount)then
     HexDumpDisplay.Cells[ch+1,line+1]:=' ';
  //Display the characters in the final coluumn
  if line+1<HexDumpDisplay.RowCount then
   HexDumpDisplay.Cells[17,line+1]:=chars;
  //And move onto the next line
  inc(line);
 until (MainFile.Position-$10>=MainFile.Size-1) //Continue until the end of the file
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
 p: Cardinal;
const
 codes: array[0..31] of String=('nul','soh','stx','etx','eot','enq','ack','bel',
                                'bs' ,'ht' ,'lf' ,'vt' ,'ff' ,'cr' ,'so' ,'si' ,
                                'dle','dc1','dc2','dc3','dc4','nak','syn','etb',
                                'can','em' ,'sub','esc','fs' ,'gs' ,'rs' ,'us');
begin
 HintText:='';
 if(aRow<1)then exit;
 //Not comparing files, so just show the ASCII character
 if CompareFilename='' then
 begin
  s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
  //Printable
  if(s>31)and(s<127)then HintText:=chr(s);
  //Control codes
  if s<32  then HintText:=UpperCase(codes[s]);
  if s=127 then HintText:='DEL';
 end
 else //We are comparing files, so show the value of the compared file
 begin
  //Get the current position
  p:=StrtoInt('$'+HexDumpDisplay.Cells[0,aRow])+aCol;
  //Move to position within the comparison file
  if p-1<CompareFile.Size then
  begin
   CompareFile.Position:=p-1;
   //And read the byte
   s:=CompareFile.ReadByte;
   HintText:=IntToHex(s,2);
  end;
 end;
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
 HexDumpDisplay.Font.Color:=FontColour;
 //We're only colouring below the header row
 if aRow>0 then
  //And the hex cells
  if (aCol>0) and (aCol<17) then
  begin
   //Get the value in the cell
   s:=StrToIntDef('$'+HexDumpDisplay.Cells[aCol,aRow],0);
   //Default font colour is Blue
   HexDumpDisplay.Font.Color:=DefaultColour;
   //If it is zero, then it is Red
   if s=0 then HexDumpDisplay.Font.Color:=ZeroColour;
   //If it is printable, then it is Green (darkish shade)
   if (s>31) and (s<127) then HexDumpDisplay.Font.Color:=ASCIIColour;
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
    if c<>s then HexDumpDisplay.Canvas.Brush.Color:=DiffColour;
    //If we are beyond the end of the comparison file, colour the background Cyan
    if pos>CompareFile.Size then HexDumpDisplay.Canvas.Brush.Color:=OverColour;
   end;
  end
  else
  begin
   //Colour the character column to match the header and address
   HexDumpDisplay.Font.Style:=[fsBold];
   if aCol=0 then  HexDumpDisplay.Canvas.Brush.Color:=AddressColour;
   if aCol=17 then HexDumpDisplay.Canvas.Brush.Color:=CharColour;
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
