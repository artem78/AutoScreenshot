unit UpdateChecker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure CheckForUpdates(ASilent: Boolean = False);


implementation

uses
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, uUtilsMore,
  uLocalization, uUtils, uAutoScreen, LCLIntf,
  fphttpclient, opensslsockets, fpjson, jsonparser;

{$R *.lfm}

type

  { TUpdateCheckerForm }

  TUpdateCheckerForm = class(TForm)
    StatusLabel: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private

  public

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
    'https://eojiuvjshd8kbzt.m.pipedream.net'
{$ELSE}
    'https://api.github.com/repos/artem78/AutoScreenshot/releases/latest'
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
  Msg: {TStringBuilder} TAnsiStringBuilder;
begin
  //ShowMessage(AVer.ToString()+LineEnding+AUrl+LineEnding+AChangelog);

  if Assigned(UpdateCheckerForm) then
    FreeAndNil(UpdateCheckerForm);

  CurrentVersion := TProgramVersion.Create(GetProgramVersionStr());

  if ANewVersion > CurrentVersion then
  begin
    Msg := TAnsiStringBuilder.Create();
    try
      Msg.AppendFormat(Localizer.I18N('UpdateFound'),
               [ANewVersion.ToString(True), CurrentVersion.ToString(True)]);
      Msg.AppendLine('');
      Msg.AppendLine('');
      Msg.AppendLine(AChangeLog);
      Msg.AppendLine('');
      Msg.AppendLine(Localizer.I18N('AskDownloadUpdate'));
      //if MessageDlg(Msg.ToString, mtInformation, mbYesNo, 0) = mrYes then
      if QuestionDlg('', Msg.ToString, {mtCustom} mtInformation,
             [mrYes, Localizer.I18N('Yes'), mrNo, Localizer.I18N('No')], '') = mrYes then
        OpenURL(ADownloadUrl);
    finally
      Msg.Free
    end;
  end
  else
  begin
    if not SilentUpdateChecking then
      ShowMessage(Localizer.I18N('NoUpdatesFound'));
  end;

  UpdateCheckerThread := Nil;
end;

procedure OnCheckForUpdatesFailed(AErr: Exception);
begin
  if Assigned(UpdateCheckerForm) then
    FreeAndNil(UpdateCheckerForm);

  MessageDlg(Localizer.I18N('UpdateCheckFailed'), AErr{.Message}.ToString,
             mtError, [mbOK], 0);

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

  if not ASilent then
  begin
    if not Assigned(UpdateCheckerForm) then
      UpdateCheckerForm := TUpdateCheckerForm.Create({Self}MainForm);

    UpdateCheckerForm.Show;
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
        LatestVersion := TProgramVersion.Create(JsonData.GetPath('tag_name').AsString);
        DownloadUrl := JsonData.GetPath('html_url').AsString;
        ChangeLog := JsonData.GetPath('body').AsString;

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
  StatusLabel.Caption := Localizer.I18N('CheckingForUpdates');
end;

initialization
  UpdateCheckerThread := Nil;
finalization;
  if Assigned(UpdateCheckerThread) then
    FreeAndNil(UpdateCheckerThread);
end.

