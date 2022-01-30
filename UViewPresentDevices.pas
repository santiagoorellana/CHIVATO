
///////////////////////////////////////////////////////////////////////////////
// Nombre: UViewPresentDevices
// Autor: Santiago A. Orellana Pérez (Chago)
// Creado: 28/12/2016
// Objetivo: Implementa el formulario que permite ver los dispositivos
//           de almacenamiento actualmente conectados.
///////////////////////////////////////////////////////////////////////////////

unit UViewPresentDevices;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,UStoreMonitor, UCommon,
  ActnList, ToolWin, Menus, ExtCtrls;

type
  TFormViewPresentDevices = class(TForm)
    TreeView1: TTreeView;
    ActionList1: TActionList;
    ActionClose: TAction;
    ActionReset: TAction;
    ActionSaveAsTXT: TAction;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionResetExecute(Sender: TObject);
    procedure ActionSaveAsTXTExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ShowPresentDevices;
  public
    { Public declarations }
  end;

implementation

uses UFormMain;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Inicia el formulario.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.FormCreate(Sender: TObject);
begin
ShowPresentDevices;
end;

//-----------------------------------------------------------------------------
// Cierra el formulario.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.ActionCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Actualiza los datos que se muestran.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.ActionResetExecute(Sender: TObject);
begin
ShowPresentDevices;
end;

//-----------------------------------------------------------------------------
// Guarda los datos en un fichero de texto.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.ActionSaveAsTXTExecute(Sender: TObject);
var dlg: TOpenDialog;
begin
dlg := TSaveDialog.Create(nil);
dlg.DefaultExt := 'txt';
dlg.Filter := 'Ficheros de texto|*.txt|Todos|*.*';
if dlg.Execute then
   TreeView1.SaveToFile(dlg.FileName);
end;

//-----------------------------------------------------------------------------
// Llena la lista con los dispositivos actualmente conectados.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.ShowPresentDevices;
var n: Integer;
    Info: TStoreInfo;
    Node: TTreeNode;
    NameLabel: String;
begin
TreeView1.Items.Clear;
for n := 2 to cLastDevice do
    if MainForm.StoreMonitor.GetStore(n, Info) then
       begin
       //Agrega el nodo principal que representa al dispositivo.
       NameLabel := Info.sName + ' (' + Info.sLeter + ':)';
       Node := TreeView1.Items.Add(TTreeNode.Create(TreeView1.Items), NameLabel);

       //Agrega los nodos con los datos del dispositivo.
       TreeView1.Items.AddChild(Node, 'Tipo: ' + MainForm.StoreMonitor.TypeToString(Info.sType));
       TreeView1.Items.AddChild(Node, 'Serie: ' + IntToHex(Info.sSerial, 8));
       TreeView1.Items.AddChild(Node, 'Capacidad: ' + ToXByte(Info.sCapacity / cGByte));
       TreeView1.Items.AddChild(Node, 'Conectado: ' + DateTimeToStr(Info.sDateTimeConect));
       TreeView1.Items.AddChild(Node, 'Duración: ' + ToXTime(Info.sTimeConectionMin));
       end;
end;

//-----------------------------------------------------------------------------
// Para que el formulario se libere cuando se cierre.
//-----------------------------------------------------------------------------
procedure TFormViewPresentDevices.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
