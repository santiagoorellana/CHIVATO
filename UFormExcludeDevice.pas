
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormExcludeDevice
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 22/12/2016
// Objetivo: Implementa el formulario que permite establecer una lista
//           de dispositivos que se deben excluir del monitoreo.
///////////////////////////////////////////////////////////////////////////////

unit UFormExcludeDevice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Buttons, UQueryDeviceSerial, Menus;

type
  TFormExcludeDevice = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    ActionList1: TActionList;
    ActionAddToList: TAction;
    ActionClearList: TAction;
    ActionSaveList: TAction;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ActionDelSelect: TAction;
    BitBtn5: TBitBtn;
    BitBtn1: TBitBtn;
    ActionClose: TAction;
    ActionLoadFile: TAction;
    ActionSaveFile: TAction;
    PopupMenu1: TPopupMenu;
    Guardarficherodetexto1: TMenuItem;
    Cargarficherodetexto1: TMenuItem;
    Agregaralalista1: TMenuItem;
    Eliminarseleccin1: TMenuItem;
    N1: TMenuItem;
    Vaciarlista1: TMenuItem;
    Guardarlista1: TMenuItem;
    
    procedure ActionAddToListExecute(Sender: TObject);
    procedure ActionClearListExecute(Sender: TObject);
    procedure ActionSaveListExecute(Sender: TObject);
    procedure ActionDelSelectExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionLoadFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses UFormMain;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Obtiene la lista de dispositivos excluidos.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.FormCreate(Sender: TObject);
begin
ListBox1.Items.Text := MainForm.StoreMonitor.ExcludeDevices.Text;
end;

//-----------------------------------------------------------------------------
// Agrega un nuevo dispositivo a la lista de exclusión.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionAddToListExecute(Sender: TObject);
var serial: DWORD;
begin
if InputQueryDeviceSerial(serial) then          //Pide el número de serie del dispositivo.
   if serial <> 0 then                          //Si se pudo obtener un número de serie...
      ListBox1.Items.Add(IntToHex(serial, 8));  //Guarda el número de serie en la lista.
end;

//-----------------------------------------------------------------------------
// Borra la lista de dispositivos completa.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionClearListExecute(Sender: TObject);
begin
ListBox1.Clear;
end;

//-----------------------------------------------------------------------------
// Borra de la lista el elemento seleccionado.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionDelSelectExecute(Sender: TObject);
begin
ListBox1.DeleteSelected;
end;

//-----------------------------------------------------------------------------
// Guarda la lista de dispositivos excluidos.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionSaveListExecute(Sender: TObject);
begin
MainForm.StoreMonitor.ExcludeDevices.Text := ListBox1.Items.Text;
MainForm.StoreMonitor.SaveExcludeDevicesList;
end;

//-----------------------------------------------------------------------------
// Cierra la ventana sin hacer más nada.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Carga la lista desde un fichero de texto.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionLoadFileExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
dlg := TOpenDialog.Create(nil);
dlg.DefaultExt := 'txt';
dlg.Filter := 'Ficheros de texto|*.txt|Todos|*.*';
if dlg.Execute then
   if FileExists(dlg.FileName) then
      ListBox1.Items.LoadFromFile(dlg.FileName);
end;

//-----------------------------------------------------------------------------
// Guarda la lista en un fichero de texto.
//-----------------------------------------------------------------------------
procedure TFormExcludeDevice.ActionSaveFileExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
dlg := TSaveDialog.Create(nil);
dlg.DefaultExt := 'txt';
dlg.Filter := 'Ficheros de texto|*.txt|Todos|*.*';
if dlg.Execute then
   ListBox1.Items.SaveToFile(dlg.FileName);
end;


procedure TFormExcludeDevice.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
