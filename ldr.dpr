
///////////////////////////////////////////////////////////////////////////////
// Nombre: ldr
// Autor: Santiago A. Orellana P�rez (Chago)
// Creado: 23/07/2016
// Objetivo: Implementa el cargador de Chivato.
///////////////////////////////////////////////////////////////////////////////

program ldr;

uses
  Forms,
  Windows,
  ULoader in 'ULoader.pas' {Form1},
  UAntiReversing in 'UAntiReversing.pas',
  UCommon in 'UCommon.pas';

{$R *.res}


var Semaphore: THandle;
begin

//Si ya est� ejecut�ndose, termina la ejecuci�n de esta instancia.
Semaphore := CreateSemaphore(nil, 0, 1, PChar(CreateNameFor(cFileInit, cSemaphoreLoader)));
if ((Semaphore <> 0) and (GetLastError = ERROR_ALREADY_EXISTS)) then
   begin
   CloseHandle(Semaphore);
   Halt;
   end;

  //Ejecuta la aplicaci�n y el formulario principal.
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
