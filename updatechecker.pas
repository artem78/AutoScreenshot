unit UpdateChecker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ButtonPanel;

procedure CheckForUpdates(ASilent: Boolean = False);


implementation

uses
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, uUtilsMore,
  uLocalization, uUtils, uAutoScreen, LCLIntf,
  fphttpclient, opensslsockets, fpjson, jsonparser, StrUtils;

{$R *.lfm}

type

  TUpdateFormState = (ufsHidden, ufsFetchingData, ufsHasUpdates,
                      ufsNoUpdates, ufsError);

  { TUpdateCheckerForm }

  TUpdateCheckerForm = class(TForm)
    MsgLabel: TLabel;
    ProgressBar: TProgressBar;
    ButtonPanel1: TButtonPanel;
    procedure FormCreate(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FState: TUpdateFormState;

    procedure SetState(AState: TUpdateFormState);
  public
    LatestVersion: TProgramVersion;
    DownloadUrl, ChangeLog, ErrorMsg: String;

    property State: TUpdateFormState write SetState;
  end;


  TSuccessCallback = procedure (AVer: TProgramVersion; AUrl: String;
                                AChangelog: String) {of object};
  TFailCallback = procedure (AErr: exception) {of object};

  { TUpdateCheckerThread }

  TUpdateCheckerThread = class(TThread)
  private
    SuccessCallback: TSuccessCallback;
    FailCallback: TFailCallback;
    LatestVersion: TProgramVersion;
    DownloadUrl, ChangeLog: String;
    LastError: Exception;
    UserAgent: String;

    procedure NotifySuccess;
    procedure NotifyFail;
  protected
    procedure Execute; override;
  public
    constructor Create(ASucessCallback: TSuccessCallback = Nil;
                       AFailCallback: TFailCallback = Nil;
                       AUserAgent: String = 'Update Checker');
  end;


const
  ApiUrl =
{$IFOPT D+}
    'https://eo873h2zv0emseb.m.pipedream.net/'
{$ELSE}
    'https://api.github.com/repos/artem78/AutoScreenshot/releases'
{$ENDIF}
  ;

var
  UpdateCheckerForm: TUpdateCheckerForm;
  {CheckerThread} UpdateCheckerThread: TUpdateCheckerThread;
  SilentUpdateChecking: Boolean;

procedure OnCheckForUpdatesFinished(ANewVersion: TProgramVersion;
        ADownloadUrl: String; AChangeLog: String);
var
  CurrentVersion: TProgramVersion;
begin
  CurrentVersion := TProgramVersion.Create(GetProgramVersionStr());

  if ANewVersion > CurrentVersion then
  begin
    UpdateCheckerForm.LatestVersion := ANewVersion;
    UpdateCheckerForm.DownloadUrl   := ADownloadUrl;
    UpdateCheckerForm.ChangeLog     := AChangeLog;
    UpdateCheckerForm.State         := ufsHasUpdates;
  end
  else
  begin
    if not SilentUpdateChecking then
    begin
      UpdateCheckerForm.State := ufsNoUpdates;
    end;
  end;

  UpdateCheckerThread := Nil;
end;

procedure OnCheckForUpdatesFailed(AErr: Exception);
begin
  UpdateCheckerForm.ErrorMsg := AErr{.Message}.ToString;
  UpdateCheckerForm.State := ufsError;

  UpdateCheckerThread := Nil;
end;

procedure CheckForUpdates(ASilent: Boolean);
var
  UserAgent: String;
  CurrentVersion: TProgramVersion;
  //UpdateCheckerThread: TUpdateCheckerThread;

begin
  SilentUpdateChecking := ASilent;

  CurrentVersion := TProgramVersion.Create(GetProgramVersionStr());

  Ini.WriteDateTime(DefaultConfigIniSection, 'LastCheckForUpdates', Now);

  UserAgent := Format('%s v%s Update Checker',
          [Application.Title, CurrentVersion.ToString()]);
  UpdateCheckerThread := TUpdateCheckerThread.Create(@OnCheckForUpdatesFinished,
          @OnCheckForUpdatesFailed, UserAgent);
  UpdateCheckerThread.Start;

  //if not Assigned(UpdateCheckerForm) then
    UpdateCheckerForm := TUpdateCheckerForm.Create(MainForm);

  if not ASilent then
  begin
    UpdateCheckerForm.State := ufsFetchingData;
  end;

  //UpdateCheckerThread.Free;
end;

{ TUpdateCheckerThread }

procedure TUpdateCheckerThread.NotifySuccess;
begin
  if Assigned(SuccessCallback) then
    SuccessCallback(LatestVersion, DownloadUrl, ChangeLog);
end;

procedure TUpdateCheckerThread.NotifyFail;
begin
  if Assigned(FailCallback) then
    FailCallback(LastError);
end;

procedure TUpdateCheckerThread.Execute;
var
  ResponseStr: String;
  Client: TFPHTTPClient;
  JsonData: TJSONData;
  JsonArrayEnum: TBaseJSONEnumerator;
  TagName: String;
  Version: TProgramVersion;
begin
  LatestVersion := TProgramVersion.Create();
  DownloadUrl := '';
  ChangeLog := '';

  Client := TFPHTTPClient.Create(Nil);
  try
    try
      Client.AllowRedirect := True;
      Client.AddHeader('Accept', 'application/vnd.github.v3+json');
      Client.AddHeader('User-Agent', UserAgent);
      ResponseStr := Client.Get(ApiUrl);

      JsonData := GetJSON(ResponseStr);
      try
        JsonArrayEnum := TJSONArray(JsonData).GetEnumerator;
        try
          while JsonArrayEnum.MoveNext do
          begin
            TagName := JsonArrayEnum.Current.Value.GetPath('tag_name').AsString;
            // Skip other OS specific release
            {$IfDef Windows}
            if ContainsText(TagName, 'linux') then
              Continue;
            {$EndIf}
            {$IfDef Linux}
            if ContainsText(TagName, {'windows'} 'win') then
              Continue;
            {$EndIf}

            Version := TProgramVersion.Create(ExtractDelimited(1, TagName, ['-']));
            if Version > LatestVersion then
            begin
              LatestVersion := Version;
              DownloadUrl   := JsonArrayEnum.Current.Value.GetPath('html_url').AsString;
              ChangeLog     := JsonArrayEnum.Current.Value.GetPath('body').AsString;
            end;
          end;
        finally
          FreeAndNil(JsonArrayEnum)
        end;

        Synchronize(@NotifySuccess);
      finally
        JsonData.Free;
      end;

    except on E: Exception do
      begin
        LastError := E;
        Synchronize(@NotifyFail);
      end;
    end;
  finally
    Client.Free;
  end;
end;

constructor TUpdateCheckerThread.Create(ASucessCallback: TSuccessCallback;
                           AFailCallback: TFailCallback; AUserAgent: String);
begin
  SuccessCallback := ASucessCallback;
  FailCallback := AFailCallback;
  UserAgent := AUserAgent;

  FreeOnTerminate := True;

  inherited Create(True);
end;



{ TUpdateCheckerForm }

procedure TUpdateCheckerForm.FormCreate(Sender: TObject);
begin
  Caption := Localizer.I18N('CheckForUpdates');
  with ButtonPanel1 do
  begin
    CloseButton.Caption  := Localizer.I18N('Close');
    OKButton.Caption     := Localizer.I18N('Yes');
    CancelButton.Caption := Localizer.I18N('No');
  end;
  State := ufsHidden;
end;

procedure TUpdateCheckerForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdateCheckerForm.OKButtonClick(Sender: TObject);
begin
  OpenURL(DownloadUrl);
  Close;
end;

procedure TUpdateCheckerForm.SetState(AState: TUpdateFormState);
var
  CurrentVersion: TProgramVersion;
  Msg: {TStringBuilder} TAnsiStringBuilder;
begin
  FState := AState;

  if FState = ufsHidden then
  begin
    Hide;
    Exit;
  end;

  Msg := TAnsiStringBuilder.Create();
  try
    case FState of
      ufsFetchingData:
        begin
          Msg.Append(Localizer.I18N('CheckingForUpdates'));

          ButtonPanel1.Hide;
        end;

      ufsNoUpdates:
        begin
          Msg.Append(Localizer.I18N('NoUpdatesFound'));

          ButtonPanel1.ShowButtons := [pbClose];
          ButtonPanel1.Show;
        end;

      ufsHasUpdates:
        begin
          CurrentVersion := TProgramVersion.Create(GetProgramVersionStr());
          Msg.AppendFormat(Localizer.I18N('UpdateFound'),
                   [LatestVersion.ToString(True), CurrentVersion.ToString(True)])
             .AppendLine
             .AppendLine
             .AppendLine(ChangeLog.Trim)
             //.AppendLine
             .AppendLine
             .Append(Localizer.I18N('AskDownloadUpdate'));

          ButtonPanel1.ShowButtons := [pbOK, pbCancel];
          ButtonPanel1.Show;
        end;

      ufsError:
        begin
          Msg.AppendLine(Localizer.I18N('UpdateCheckFailed'));
          if not ErrorMsg.IsEmpty then
          begin
             Msg.AppendLine
                .Append(ErrorMsg);
          end;

          ButtonPanel1.ShowButtons := [pbClose];
          ButtonPanel1.Show;
        end;
    end;

    MsgLabel.Caption := Msg.ToString;

    ProgressBar.Visible := FState = ufsFetchingData;

    MoveToDefaultPosition;
    if not Visible then
      Show;

  finally
    Msg.Free;
  end;
end;

initialization
  UpdateCheckerThread := Nil;
finalization;
  if Assigned(UpdateCheckerThread) then
    FreeAndNil(UpdateCheckerThread);
end.

