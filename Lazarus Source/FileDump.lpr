program FileDump;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, MainUnit, SaveTextUnit, OptionsUnit, SearchUnit
 { you can add units after this };

{$R *.res}

begin
 RequireDerivedFormResource:=True;
 Application.Title:='File Dump';
 Application.Scaled:=True;
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.CreateForm(TSaveTextForm, SaveTextForm);
 Application.CreateForm(TOptionsForm, OptionsForm);
 Application.CreateForm(TSearchForm, SearchForm);
 Application.Run;
end.

