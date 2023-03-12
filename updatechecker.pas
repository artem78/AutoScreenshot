unit UpdateChecker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uUtilsMore;

type
  TSuccessCallback = procedure (AVer: TProgramVersion; AUrl: String;
                                AChangelog: String) of object;
  TFailCallback = procedure (AErr: exception) of object;

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

implementation

uses
  fphttpclient, opensslsockets, fpjson, jsonparser;

const
  ApiUrl =
{$IFOPT D+}
    'https://eojiuvjshd8kbzt.m.pipedream.net'
{$ELSE}
    'https://api.github.com/repos/artem78/AutoScreenshot/releases/latest'
{$ENDIF}
  ;

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

end.

