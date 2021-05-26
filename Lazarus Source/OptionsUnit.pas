unit OptionsUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

 { TOptionsForm }

 TOptionsForm = class(TForm)
  ResetBtn: TBitBtn;
  CancelBtn: TBitBtn;
  ColorDialog1: TColorDialog;
  OKBtn: TBitBtn;
  GroupBox1: TGroupBox;
  lbZeroColour: TLabel;
  lbCharColour: TLabel;
  lbDefaultColour: TLabel;
  lbASCIIColour: TLabel;
  lbDiffColour: TLabel;
  lbOverColour: TLabel;
  lbCellBGColour: TLabel;
  lbAltBGColour: TLabel;
  lbHeaderColour: TLabel;
  lbAddressColour: TLabel;
  procedure FormShow(Sender: TObject);
  procedure lbZeroColourClick(Sender: TObject);
  procedure OKBtnClick(Sender: TObject);
  procedure ResetBtnClick(Sender: TObject);
 private

 public

 end;

var
 OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TOptionsForm }

{                                                                              }
{ Form is showing                                                              }
{                                                                              }
procedure TOptionsForm.FormShow(Sender: TObject);
begin
 //Set up the labels with the appropriate colours
 lbZeroColour.Font.Color   :=MainForm.ZeroColour;
 lbZeroColour.Color        :=MainForm.CellBGColour;
 lbCharColour.Font.Color   :=MainForm.FontColour;
 lbCharColour.Color        :=MainForm.CharColour;
 lbDefaultColour.Font.Color:=MainForm.DefaultColour;
 lbDefaultColour.Color     :=MainForm.CellBGColour;
 lbASCIIColour.Font.Color  :=MainForm.ASCIIColour;
 lbASCIIColour.Color       :=MainForm.CellBGColour;
 lbDiffColour.Font.Color   :=MainForm.DefaultColour;
 lbDiffColour.Color        :=MainForm.DiffColour;
 lbOverColour.Font.Color   :=MainForm.DefaultColour;
 lbOverColour.Color        :=MainForm.OverColour;
 lbCellBGColour.Font.Color :=MainForm.DefaultColour;
 lbCellBGColour.Color      :=MainForm.CellBGColour;
 lbAltBGColour.Font.Color  :=MainForm.DefaultColour;
 lbAltBGColour.Color       :=MainForm.AltBGColour;
 lbHeaderColour.Font.Color :=MainForm.FontColour;
 lbHeaderColour.Color      :=MainForm.HeaderColour;
 lbAddressColour.Font.Color:=MainForm.FontColour;
 lbAddressColour.Color     :=MainForm.AddressColour;
end;

{                                                                              }
{ User has clicked on a label to change the colour                             }
{                                                                              }
procedure TOptionsForm.lbZeroColourClick(Sender: TObject);
var
 control: TLabel;
begin
 //Ensure it is a TLabel
 if Sender is TLabel then
 begin
  //Which label?
  control:=TLabel(Sender);
  //Changing a foreground colour
  if(control=lbZeroColour)
  or(control=lbDefaultColour)
  or(control=lbASCIIColour)  then ColorDialog1.Color:=control.Font.Color;
  //Changing a background colour
  if(control=lbDiffColour)
  or(control=lbOverColour)
  or(control=lbHeaderColour)
  or(control=lbAddressColour)
  or(control=lbCharColour)
  or(control=lbCellBGColour)
  or(control=lbAltBGColour)  then ColorDialog1.Color:=control.Color;
  //Show the colour dialogue
  if ColorDialog1.Execute then
  begin
   //Foreground colour has been changed
   if(control=lbZeroColour)
   or(control=lbDefaultColour)
   or(control=lbASCIIColour) then control.Font.Color:=ColorDialog1.Color;
   //Background colour has been changed
   if(control=lbDiffColour)
   or(control=lbOverColour)
   or(control=lbHeaderColour)
   or(control=lbAddressColour)
   or(control=lbCharColour)
   or(control=lbCellBGColour)
   or(control=lbAltBGColour) then control.Color:=ColorDialog1.Color;
   //Update the other label controls, where appropriate
   lbZeroColour.Color        :=lbCellBGColour.Color;
   lbDefaultColour.Color     :=lbCellBGColour.Color;
   lbASCIIColour.Color       :=lbCellBGColour.Color;
   lbDiffColour.Font.Color   :=lbDefaultColour.Font.Color;
   lbOverColour.Font.Color   :=lbDefaultColour.Font.Color;
   lbCellBGColour.Font.Color :=lbDefaultColour.Font.Color;
   lbAltBGColour.Font.Color  :=lbDefaultColour.Font.Color;
  end;
 end;
end;

{                                                                              }
{ User has clicked on OK                                                       }
{                                                                              }
procedure TOptionsForm.OKBtnClick(Sender: TObject);
begin
 //Get the configurable colour scheme from the labels and save to the global vars
 MainForm.ZeroColour   :=lbZeroColour.Font.Color;
 MainForm.DefaultColour:=lbDefaultColour.Font.Color;
 MainForm.ASCIIColour  :=lbASCIIColour.Font.Color;
 MainForm.DiffColour   :=lbDiffColour.Color;
 MainForm.OverColour   :=lbOverColour.Color;
 MainForm.HeaderColour :=lbHeaderColour.Color;
 MainForm.AddressColour:=lbAddressColour.Color;
 MainForm.CharColour   :=lbCharColour.Color;
 MainForm.CellBGColour :=lbCellBGColour.Color;
 MainForm.AltBGColour  :=lbAltBGColour.Color;
end;

{                                                                              }
{ Reset the scheme to the defaults                                             }
{                                                                              }
procedure TOptionsForm.ResetBtnClick(Sender: TObject);
begin
 lbZeroColour.Font.Color   :=MainForm.dZeroColour;
 lbZeroColour.Color        :=MainForm.dCellBGColour;
 lbCharColour.Font.Color   :=MainForm.dFontColour;
 lbCharColour.Color        :=MainForm.dCharColour;
 lbDefaultColour.Font.Color:=MainForm.dDefaultColour;
 lbDefaultColour.Color     :=MainForm.dCellBGColour;
 lbASCIIColour.Font.Color  :=MainForm.dASCIIColour;
 lbASCIIColour.Color       :=MainForm.dCellBGColour;
 lbDiffColour.Font.Color   :=MainForm.dDefaultColour;
 lbDiffColour.Color        :=MainForm.dDiffColour;
 lbOverColour.Font.Color   :=MainForm.dDefaultColour;
 lbOverColour.Color        :=MainForm.dOverColour;
 lbCellBGColour.Font.Color :=MainForm.dDefaultColour;
 lbCellBGColour.Color      :=MainForm.dCellBGColour;
 lbAltBGColour.Font.Color  :=MainForm.dDefaultColour;
 lbAltBGColour.Color       :=MainForm.dAltBGColour;
 lbHeaderColour.Font.Color :=MainForm.dFontColour;
 lbHeaderColour.Color      :=MainForm.dHeaderColour;
 lbAddressColour.Font.Color:=MainForm.dFontColour;
 lbAddressColour.Color     :=MainForm.dAddressColour;
end;

end.
