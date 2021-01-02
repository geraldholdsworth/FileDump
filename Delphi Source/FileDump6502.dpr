program FileDump6502;

uses
  Forms,
  MainUnit6502 in 'MainUnit6502.pas' {MainForm6502};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'File Dump and Comparison';
  Application.CreateForm(TMainForm6502, MainForm6502);
  Application.Run;
end.
