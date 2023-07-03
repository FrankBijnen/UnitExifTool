unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ValEdit, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.Imaging.jpeg,  // For other image types 'use' the appropriate unit, and adjust the Mask property of FileListbox1
  UnitExifTool;      // The unit to use for TExifTool

type

{ Specialized class }

// Note: You dont have to create this class, you can also build a string where each line is terminated by CR LF.
// And send that string with ExifTool.Send()
// See: BtnSetFileDatesFromExifClick

  TMyExifTool = class(TExifTool)
  private
    procedure ShiftDates(const AImage: string;
                         const DateTimeShift: string);
    procedure GetLensTags(const AImage: string);
    procedure GetGPSTags(const AImage: string);
    procedure GetExifAndXmpTags(const AImage: string);
  end;

  TForm1 = class(TForm)
    PnlExif: TPanel;
    ExifLog: TMemo;
    LvExif: TListView;
    PnlFunctions: TPanel;
    PnlFiles: TPanel;
    FileListBox1: TFileListBox;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    Splitter1: TSplitter;
    Image1: TImage;
    BtnLensTags: TButton;
    BtnGpsTags: TButton;
    BtnExifDateForward: TButton;
    BtnExifDateBackward: TButton;
    BtnSetFileDatesFromExif: TButton;
    procedure FileListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnLensTagsClick(Sender: TObject);
    procedure BtnGpsTagsClick(Sender: TObject);
    procedure BtnExifDateForwardClick(Sender: TObject);
    procedure BtnExifDateBackwardClick(Sender: TObject);
    procedure BtnSetFileDatesFromExifClick(Sender: TObject);
  private
    { Private declarations }
    ExifTool: TMyExifTool;

    procedure ShiftExifDates(const Shift: string);
    procedure CheckExifError(const Content: TContent);
    procedure ExifToolReady(Sender: TObject; Content: TContent);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ Helper for Content reading. See OnReady event. }

function NextField(var AString: string; const ADelimiter: string): string;
var Indx: integer;
begin
  Indx := Pos(ADelimiter, AString);
  if Indx < 1 then
  begin
    result := AString;
    AString := '';
  end
  else
  begin
    result := Copy(AString, 1, Indx - 1);
    Delete(AString, 1, Indx);
  end;
end;

{ Specialized Class }

procedure TMyExifTool.ShiftDates(const AImage: string;
                                 const DateTimeShift: string);
var Cmds: TStringList;
begin
  Cmds := TStringList.Create;
  try
    Cmds.Add('-ignoreMinorErrors');
    Cmds.Add('-preserve');
    Cmds.Add('-overwrite_original');
    Cmds.Add('-wm'); // Only update existing tags
    Cmds.Add('w');
    Cmds.Add('-AllDates' + DateTimeShift);
    Cmds.Add(AImage);
    Cmds.Add('-wm'); // Restore write mode
    Cmds.Add('wcg');
    Send(Cmds);
  finally
    Cmds.Free;
  end;
end;

procedure TMyExifTool.GetLensTags(const AImage: string);
var Cmds: TStringList;
begin
  // this needs 2 executes. LensId in 2 seperate outputs!
  Cmds := TStringList.Create;
  try
    Cmds.Add('-ignoreMinorErrors');
    Cmds.Add('-quiet'); // Dont want ready string for first part!
    Cmds.Add('-short1');
    Cmds.Add('-forcePrint');
    Cmds.Add('-Dateformat');
    Cmds.Add('%Y-%m-%dT%H:%M:%S'); // default!
    Cmds.Add('-FileCreateDate');
    Cmds.Add('-DateTimeOriginal');
    Cmds.Add('-Model');
    Cmds.Add('-LensId');
    Cmds.Add(AImage);
    Cmds.Add('-execute');

    Cmds.Add('-short1'); // Now we do
    Cmds.Add('-forcePrint');
    Cmds.Add('--PrintConv');
    Cmds.Add('-LensId');
    Cmds.Add(AImage);
    Send(Cmds);
  finally
    Cmds.Free;
  end;
end;

procedure TMyExifTool.GetGPSTags(const AImage: string);
var Cmds: TStringList;
begin
  Cmds := TStringList.Create;
  try
    Cmds.Add('-ignoreMinorErrors');
    Cmds.Add('-short1');
    Cmds.Add('--PrintConv');
    Cmds.Add('-gpslongitude');
    Cmds.Add('-gpslatitude');
    Cmds.Add('-gpsaltitude');
    Cmds.Add(AImage);
    Send(Cmds);
  finally
    Cmds.Free;
  end;
end;

procedure TMyExifTool.GetExifAndXmpTags(const AImage: string);
var Cmds: TStringList;
begin
  Cmds := TStringList.Create;
  try
    Cmds.Add('-ignoreMinorErrors');
    Cmds.Add('-short1');
    Cmds.Add('-Xmp:all');
    Cmds.Add('-Exif:all');
    Cmds.Add(AImage);
    Send(Cmds);
  finally
    Cmds.Free;
  end;
end;

{ Form methods }

procedure TForm1.ShiftExifDates(const Shift: string);
begin
//'+=0:0:0 2:0:0'
// + or - move forward, backward in time
// =
// 0:0:0 Year:month:date
// _ space
// 2:0:0 Hour:minutes:seconds

  ExifTool.ShiftDates(FileListBox1.FileName, Shift);

  // Wait for Exiftool to complete
  ExifTool.WaitFor;

  // Read modifief values
  ExifTool.GetExifAndXmpTags(FileListBox1.FileName);
end;

procedure TForm1.BtnExifDateForwardClick(Sender: TObject);
begin
  ShiftExifDates('+=0:0:0 2:0:0');
end;

procedure TForm1.BtnExifDateBackwardClick(Sender: TObject);
begin
  ShiftExifDates('-=0:0:0 1:0:0');
end;

procedure TForm1.BtnGpsTagsClick(Sender: TObject);
begin
  ExifTool.GetGpsTags(FileListBox1.FileName);
end;

procedure TForm1.BtnLensTagsClick(Sender: TObject);
begin
  ExifTool.GetLensTags(FileListBox1.FileName);
end;

// Calling Send directly. Dont forget to add #13 + #10 after each line. Also the last one!
procedure TForm1.BtnSetFileDatesFromExifClick(Sender: TObject);
begin
  ExifTool.Send('-filemodifydate<datetimeoriginal' + #13 + #10 +
                '-filecreatedate<datetimeoriginal' + #13 + #10 +
                 FileListBox1.FileName + #13 + #10);

  // Wait for Exiftool to complete
  ExifTool.WaitFor;

  // Read modified values
  ExifTool.GetExifAndXmpTags(FileListBox1.FileName);
end;

procedure TForm1.CheckExifError(const Content: TContent);
begin
  // Output from standard Error
  if (Content.Error <> '') then
    ExifLog.Lines.add(format('%u=>%s=%s', [Content.ExecNum, Content.Request, Content.Error]));
end;

procedure TForm1.ExifToolReady(Sender: TObject; Content: TContent);
var ExifResponse: string;
    Item: string;
    Value: string;
    LvItem: TListItem;
begin
  CheckExifError(Content);

  LvExif.Items.BeginUpdate;
  try
    LvExif.Items.Clear;
    ExifLog.Lines.Add(Format('Exiftool Execnum: %u', [Content.ExecNum]));

    ExifResponse := Content.Response;
    while Length(ExifResponse) > 0 do
    begin
      Value := NextField(ExifResponse, chr(10));
      Item := Trim(NextField(Value, ':'));

      // if there is no ":" it is usually a message!
      if (Value = '') then
      begin
        ExifLog.Lines.Add(Format('%u=>%s', [Content.ExecNum, Item]));
        continue;
      end;

      LvItem := LvExif.Items.Add;
      LvItem.Caption := Item;
      LvItem.SubItems.Add(Value);
    end;
  finally
    LvExif.Items.EndUpdate;
  end;
end;

procedure TForm1.FileListBox1Click(Sender: TObject);
begin
  // Clear log
  ExifLog.Lines.Clear;

  // We can (re)set the Execnum. This number will be returned in the OnReady event
  Exiftool.ExecNum := FileListBox1.ItemIndex;

  // Call Exiftool. Note: Control returns immediately, If you want to want to wait, call ExitTool.WaitFor
  ExifTool.GetExifAndXmpTags(FileListBox1.FileName);

  // Show image
  Image1.Picture.LoadFromFile(FileListBox1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LvExif.Items.Add.Caption := 'Select an image';
  ExifTool := TMyExifTool.Create;
  ExifTool.OnReady := ExifToolReady;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ExifTool.Free;
end;

end.
