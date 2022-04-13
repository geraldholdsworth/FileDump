unit SelectUnit;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

 { TSelectForm }

 TSelectForm = class(TForm)
  Cancelbtn: TBitBtn;
  cbHideZeros: TCheckBox;
  OutputWhole: TCheckBox;
  edFrom: TEdit;
  edTo: TEdit;
  Label1: TLabel;
  Label2: TLabel;
  OKbtn: TBitBtn;
  procedure edFromKeyPress(Sender: TObject; var Key: char);
  procedure FormShow(Sender: TObject);
  procedure OKbtnClick(Sender: TObject);
  procedure OutputWholeChange(Sender: TObject);
 private

 public
  selectStart,
  selectEnd,
  maxAddr     : QWord;
 end;

var
 SelectForm: TSelectForm;

implementation

{$R *.lfm}

uses SearchUnit;

{ TSelectForm }

procedure TSelectForm.edFromKeyPress(Sender: TObject; var Key: char);
begin
 SearchForm.KeyPressed(Key);
end;

procedure TSelectForm.FormShow(Sender: TObject);
begin
 //Setup the form
 edFrom.Text        :=IntToHex(selectStart,edFrom.MaxLength);
 edTo.Text          :=IntToHex(selectEnd  ,edTo.MaxLength);
 OutputWhole.Checked:=True;
 cbHideZeros.Checked:=False;
 edFrom.Enabled     :=not OutputWhole.Checked;
 edTo.Enabled       :=not OutputWhole.Checked;
end;

procedure TSelectForm.OKbtnClick(Sender: TObject);
begin
 //Update the variables, from the fields
 selectStart:=(StrToIntDef('$'+edFrom.Text,0)div$10)*$10;//Starts on a boundary
 selectEnd  := StrToIntDef('$'+edTo.Text,0);
 //Range check
 if selectStart>=maxAddr-$10 then selectStart:=0;
 if(selectEnd>maxAddr)or(selectEnd<=selectStart+$10)then selectEnd:=maxAddr;
end;

procedure TSelectForm.OutputWholeChange(Sender: TObject);
begin
 //Enable or disable the selection, depending on the tick box
 edFrom.Enabled:=not OutputWhole.Checked;
 edTo.Enabled  :=not OutputWhole.Checked;
end;

end.

