unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LCLTranslator;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSelectRoot: TButton;
    Label1: TLabel;
    LoadConfig: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Start: TButton;
    TreeView1: TTreeView;
    procedure btnLoadConfigClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    FRootDir: string;
    FConfig: TStringList;
    procedure ProcessLevel(ParentNode: TTreeNode; BasePath: string; Level: Integer);
    procedure SaveConfig;
    procedure LoadConfigFile(const FileName: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazFileUtils;


// Iniciar creación estructura
procedure TForm1.btnStartClick(Sender: TObject);
begin
  SetDefaultLang('es');
  // Integrar la selección de carpeta raíz
  if not SelectDirectory('Seleccionar la carpeta raíz', '', FRootDir) then
      begin
        ShowMessage('No se seleccionó la carpeta raíz.');
        Exit;
      end;

  // Mostrar la carpeta seleccionada
  ShowMessage('Raíz: ' + FRootDir);

  // Validación por si la carpeta no existe por alguna razón
  if not DirectoryExists(FRootDir) then
      begin
        ShowMessage('La carpeta raíz seleccionada no existe.');
        Exit;
      end;

  // Iniciar el proceso
  TreeView1.Items.Clear;
  FConfig := TStringList.Create;
  try
    ProcessLevel(nil, FRootDir, 1);
    // Al terminar:
    if MessageDlg('¿Guardar configuración?', mtConfirmation, [mbYES, mbNO], 0) = mrYES then
      SaveConfig;
  finally
    FConfig.Free;
  end;
end;

procedure TForm1.ProcessLevel(ParentNode: TTreeNode; BasePath: string; Level: Integer);
var
  FolderName: string;
  Node: TTreeNode;
  NewPath: string;
  ParentFolderName: string;
begin
  if Assigned(ParentNode) then
    ParentFolderName := ParentNode.Text
  else
    ParentFolderName := 'Raíz';

  repeat
    FolderName := InputBox('Carpeta dentro de "' + ParentFolderName + '"',
      'Nombre de la nueva carpeta:', '');
    if FolderName = '' then Break;

    NewPath := IncludeTrailingPathDelimiter(BasePath) + FolderName;
    if not DirectoryExists(NewPath) then
      ForceDirectories(NewPath);

    if Assigned(ParentNode) then
      Node := TreeView1.Items.AddChild(ParentNode, FolderName)
    else
      Node := TreeView1.Items.Add(nil, FolderName);

    FConfig.Add(StringOfChar('\', Level - 1) + FolderName);

    // Preguntar si desea agregar subcarpetas
    if MessageDlg(Format('¿Agregar subcarpetas dentro de "%s"?', [FolderName]),
      mtConfirmation, [mbYES, mbNo], 0) = mrYES then
      ProcessLevel(Node, NewPath, Level + 1);

    // Preguntar si desea agregar otra carpeta dentro de la actual carpeta padre
  until MessageDlg(Format('¿Agregar otra carpeta dentro de "%s"?', [ParentFolderName]),
                   mtConfirmation, [mbYES, mbNO], 0) <> mrYES;
end;


// Guardar configuración
procedure TForm1.SaveConfig;

begin
  if SaveDialog.Execute then
  begin
    FConfig.SaveToFile(SaveDialog.FileName, TEncoding.ANSI);
    ShowMessage('Configuración guardada en ' + SaveDialog.FileName);
  end;
end;

// Carga configuración
procedure TForm1.btnLoadConfigClick(Sender: TObject);
begin
  if MessageDlg('¿Cargar una configuración guardada?', mtConfirmation, [mbYES, mbNO], 0) = mrYES then
  begin
    if OpenDialog.Execute then
    begin
      LoadConfigFile(OpenDialog.FileName); //Importa configuración
      TreeView1.Items.Clear;
      FConfig := TStringList.Create;
      try
        FConfig.LoadFromFile(OpenDialog.FileName, TEncoding.ANSI);
      finally
        FConfig.Free;
      end;
    end;
  end;
end;

// Cargar configuración: selecciona carpeta base e interpreta config
procedure TForm1.LoadConfigFile(const FileName: string);
var
  Lines: TStringList;
  i, Level: Integer;
  FolderName: string;
  PathStack: array of string;
  NodeStack: array of TTreeNode;
  FullPath: string;
  NewNode: TTreeNode;
begin
  if not SelectDirectory('Selecciona la carpeta base donde recargar la estructura', '', FRootDir) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName, TEncoding.ANSI);

    TreeView1.Items.BeginUpdate;
    TreeView1.Items.Clear;

    // Reservamos un tamaño inicial para los stacks (por ejemplo, hasta 20 niveles)
    SetLength(PathStack, 20);
    SetLength(NodeStack, 20);

    for i := 0 to Lines.Count - 1 do
    begin
      // === 1. Detectar nivel por cantidad de '\' iniciales ===
      Level := 0;
      while (Level < Length(Lines[i])) and (Lines[i][Level + 1] = '\') do
        Inc(Level);

      FolderName := Trim(Copy(Lines[i], Level + 1, MaxInt));
      if FolderName = '' then Continue;

      // Aseguramos tamaño suficiente del stack dinámico
      if Length(PathStack) <= Level then
      begin
        SetLength(PathStack, Level + 5);
        SetLength(NodeStack, Level + 5);
      end;

      // === 2. Construir la ruta completa según el nivel ===
      if Level = 0 then
        PathStack[Level] := IncludeTrailingPathDelimiter(FRootDir) + FolderName
      else
        PathStack[Level] := IncludeTrailingPathDelimiter(PathStack[Level - 1]) + FolderName;

      FullPath := PathStack[Level];

      // === 3. Crear carpeta si no existe ===
      if not DirectoryExists(FullPath) then
        ForceDirectories(FullPath);

      // === 4. Agregar al TreeView ===
      if Level = 0 then
        NewNode := TreeView1.Items.Add(nil, FolderName)
      else
        NewNode := TreeView1.Items.AddChild(NodeStack[Level - 1], FolderName);

      NodeStack[Level] := NewNode;
    end;

    TreeView1.FullExpand;
    ShowMessage('Estructura importada correctamente.');
  finally
    Lines.Free;
    TreeView1.Items.EndUpdate;
  end;
  end;
end.
