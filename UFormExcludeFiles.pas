
///////////////////////////////////////////////////////////////////////////////
// Nombre: UFormExcludeFiles
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 22/12/2016
// Objetivo: Implementa el formulario que permite establecer una lista
//           de tipos de ficheros que se deben excluir del monitoreo.
///////////////////////////////////////////////////////////////////////////////

unit UFormExcludeFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, Buttons, UQueryFileExtension, UCommon, Menus;

type
  TFormExcludeFiles = class(TForm)
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

    procedure FormCreate(Sender: TObject);
    procedure ActionAddToListExecute(Sender: TObject);
    procedure ActionClearListExecute(Sender: TObject);
    procedure ActionSaveListExecute(Sender: TObject);
    procedure ActionDelSelectExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionLoadFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    { Public declarations }
  end;

implementation

uses UFormMain, UStoreMonitor;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Obtiene la lista de ficheros excluidos.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.FormCreate(Sender: TObject);
begin
ListBox1.Items.Text := MainForm.StoreMonitor.ExcludeFiles.Text;
end;

//-----------------------------------------------------------------------------
// Agrega un nuevo fichero a la lista de exclusión.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionAddToListExecute(Sender: TObject);
var Extension, ExtensionList: String;
begin
ExtensionList := GetFileExtensionsList;                     //Obtiene la lista de las extensiones.
if InputQueryFileExtension(Extension, ExtensionList) then   //Pide la extensión del tipo de fichero.
   if Extension <> '' then                                  //Si se ha introducido una extensión...
      ListBox1.Items.Add(Extension);                        //Guarda la extensión en la lista.
end;

//-----------------------------------------------------------------------------
// Borra la lista de ficheros completa.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionClearListExecute(Sender: TObject);
begin
ListBox1.Clear;
end;

//-----------------------------------------------------------------------------
// Borra de la lista el elemento seleccionado.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionDelSelectExecute(Sender: TObject);
begin
ListBox1.DeleteSelected;
end;

//-----------------------------------------------------------------------------
// Guarda la lista de ficheros excluidos.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionSaveListExecute(Sender: TObject);
begin
MainForm.StoreMonitor.ExcludeFiles.Text := ListBox1.Items.Text;
MainForm.StoreMonitor.SaveExcludeFilesList;
end;

//-----------------------------------------------------------------------------
// Cierra la ventana sin hacer más nada.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Carga la lista desde un fichero de texto.
//-----------------------------------------------------------------------------
procedure TFormExcludeFiles.ActionLoadFileExecute(Sender: TObject);
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
procedure TFormExcludeFiles.ActionSaveFileExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
dlg := TSaveDialog.Create(nil);
dlg.DefaultExt := 'txt';
dlg.Filter := 'Ficheros de texto|*.txt|Todos|*.*';
if dlg.Execute then
   ListBox1.Items.SaveToFile(dlg.FileName);
end;

procedure TFormExcludeFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
