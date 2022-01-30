unit UFormOrigin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UFormStayOnTop, StdCtrls, ActnList, Buttons, UCommon;

type
  TFormOrigin = class(TStayOnTopForm)
    LabelTitle: TLabel;
    LabelPhone: TLabel;
    LabelCountryYear: TLabel;
    ActionList1: TActionList;
    ActionFormOriginClose: TAction;
    BitBtn1: TBitBtn;
    LabelRights: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ActionFormOriginCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOrigin: TFormOrigin;

implementation

{$R *.dfm}

uses UFormMain, UAntiReversing;

//-----------------------------------------------------------------------------
// Establece dinámicamente los texto de las etiquetas.
//-----------------------------------------------------------------------------
procedure TFormOrigin.FormCreate(Sender: TObject);
begin
LabelTitle.Caption := DecStr(cAppTitle) + ' ' + IntToStr(cAppVersion)+'.'+IntToStr(cAppSubVersion);
LabelPhone.Caption := DecStr(cAppPhone) + DecStr('M*S)ND<i6b=hCp:!R');    //' (Chago)'
LabelCountryYear.Caption := DecStr(cAppCountryYear);  
LabelRights.Caption := DecStr(cRights);
Caption := DecStr(cAppTitle) + ' (Origen)';
end;

//-----------------------------------------------------------------------------
// Cierra el formulario.
//-----------------------------------------------------------------------------
procedure TFormOrigin.ActionFormOriginCloseExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Para que el formulario se libere.
//-----------------------------------------------------------------------------
procedure TFormOrigin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
