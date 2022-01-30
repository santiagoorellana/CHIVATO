
///////////////////////////////////////////////////////////////////////////////
// Nombre: UAutomaticExport
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 30/12/2016
// Objetivo: Implementa el formulario que permite establecer la lista
//           de dispositivos hacia los cuales se deben exportar
//           automáticamente los datos del programa.
///////////////////////////////////////////////////////////////////////////////

unit UAutomaticExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, StdCtrls, Buttons, UQueryDeviceSerial,
  UAntireversing;

type
  TFormAutomaticExport = class(TForm)
    ActionList1: TActionList;
    ActionAddToList: TAction;
    ActionClearList: TAction;
    ActionSaveList: TAction;
    ActionDelSelect: TAction;
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
    GroupBox2: TGroupBox;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    BitBtn2: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    
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

uses UFormMain, UStoreMonitor;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Obtiene la lista de dispositivos excluidos.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.FormCreate(Sender: TObject);
begin
//Inicia la lista de dispositivos.
ListBox1.Items.Text := MainForm.StoreMonitor.DevToAutoExport.Text;

//Inicia el selector de formato de exportación.
ComboBox1.Items.Add('TXT (Notepad, WordPad)');
ComboBox1.Items.Add('CSV (Excel, Notepad)');
ComboBox1.Items.Add('HTML (Firefox, Explorer)');
ComboBox1.Items.Add('XML (Jefe, Notepad++)');
ComboBox1.Items.Add('INI (Jefe, Notepad++)');
ComboBox1.ItemIndex := MainForm.StoreMonitor.AutoExportFormat;
end;

//-----------------------------------------------------------------------------
// Agrega un nuevo dispositivo a la lista de exclusión.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionAddToListExecute(Sender: TObject);
var serial: DWORD;
begin
if InputQueryDeviceSerial(serial) then          //Pide el número de serie del dispositivo.
   if serial <> 0 then                          //Si se pudo obtener un número de serie...
      ListBox1.Items.Add(IntToHex(serial, 8));  //Guarda el número de serie en la lista.
end;

//-----------------------------------------------------------------------------
// Borra la lista de dispositivos completa.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionClearListExecute(Sender: TObject);
begin
ListBox1.Clear;
end;

//-----------------------------------------------------------------------------
// Borra de la lista el elemento seleccionado.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionDelSelectExecute(Sender: TObject);
begin
ListBox1.DeleteSelected;
end;

//-----------------------------------------------------------------------------
// Guarda la lista de dispositivos excluidos.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionSaveListExecute(Sender: TObject);
const tit1 = 'FALTAN DATOS';
const msg1 = 'Seleccione el formato de exportación.';
begin
if ComboBox1.ItemIndex >= 0 then
   begin
   MainForm.StoreMonitor.DevToAutoExport.Text := ListBox1.Items.Text;
   MainForm.StoreMonitor.AutoExportFormat := ComboBox1.ItemIndex;
   MainForm.StoreMonitor.SaveAutoExportDevicesList;
   end
else
   Application.MessageBox(msg1, tit1, MB_OK);
end;

//-----------------------------------------------------------------------------
// Cierra la ventana sin hacer más nada.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Carga la lista desde un fichero de texto.
//-----------------------------------------------------------------------------
procedure TFormAutomaticExport.ActionLoadFileExecute(Sender: TObject);
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
procedure TFormAutomaticExport.ActionSaveFileExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
dlg := TSaveDialog.Create(nil);
dlg.DefaultExt := 'txt';
dlg.Filter := 'Ficheros de texto|*.txt|Todos|*.*';
if dlg.Execute then
   ListBox1.Items.SaveToFile(dlg.FileName);
end;


procedure TFormAutomaticExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
