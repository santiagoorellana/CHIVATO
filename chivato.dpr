
///////////////////////////////////////////////////////////////////////////////
// Nombre: chivato
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 2/07/2016
///////////////////////////////////////////////////////////////////////////////

program chivato;

uses
  Forms,
  Windows,
  DateUtils,
  SysUtils,
  UFormMain in 'UFormMain.pas' {MainForm},
  UQueryPassword in 'UQueryPassword.pas',
  UFormStayOnTop in 'UFormStayOnTop.pas' {StayOnTopForm},
  UStoreDataControl in 'UStoreDataControl.pas',
  UStoreMonitor in 'UStoreMonitor.pas',
  UAccessControl in 'UAccessControl.pas',
  UAntiReversing in 'UAntiReversing.pas',
  UViewDetail in 'UViewDetail.pas' {FormViewDetail},
  USHA256 in 'USHA256.pas',
  UCryptBase in 'UCryptBase.pas',
  UBase64 in 'UBase64.pas',
  UQueryNewPassword in 'UQueryNewPassword.pas',
  UFormOrigin in 'UFormOrigin.pas' {FormOrigin},
  UViewStatistics in 'UViewStatistics.pas' {FormViewStatistics},
  UCommon in 'UCommon.pas',
  UQuerySearchText in 'UQuerySearchText.pas',
  UViewPresentDevices in 'UViewPresentDevices.pas' {FormViewPresentDevices},
  UConvert in 'UConvert.pas' {FormConvert},
  UFormInterface in 'UFormInterface.pas' {FormInterface};

{$R *.res}

var Semaphore: THandle;

begin
//Si ya está ejecutándose, termina la ejecución de esta instancia.
try
   Semaphore := CreateSemaphore(nil, 0, 1, PChar(CreateNameFor(cFileInit, cSemaphoreChivato)));
except
   Semaphore := 0;
end;   
if ((Semaphore <> 0) and (GetLastError = ERROR_ALREADY_EXISTS)) then
   begin
   CloseHandle(Semaphore);
   Halt;
   end;

//Inicia la aplicación.
Application.Initialize;

//Abre el formulario principal.
Application.CreateForm(TMainForm, MainForm);
  //Ejecuta la aplicación.
Application.Run;

end.
