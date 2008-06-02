-module(twitter).
-compile(export_all).
-include("twoorl.hrl").

post(Url, Username, Password, Params) ->
    Encoded = binary_to_list(
		base64:encode(Username ++ ":" ++ Password)),
    Headers =
	[{"Authorization", "Basic " ++ Encoded}],
    ContentType = "application/x-www-form-urlencoded",
    Body = twoorl_util:join([[Field,$=,yaws_api:url_encode(Val)] || {Field,Val} <- Params], "&"),
    http:request(
      post,
      {Url, Headers, ContentType, iolist_to_binary(Body)}, [], []).

update(Username, Password, Status) ->
    post("http://twitter.com/statuses/update.json", Username, Password, [{"status", Status}]).

verify_credentials(Username, Password) ->
    case post("http://twitter.com/account/verify_credentials.json", Username, Password, []) of
	{ok, {{_, Status, _}, _Headers, _Body}} ->
	    case Status of
		200 ->
		    ok;
		401 ->
		    {error, unauthorized};
		_ ->
		    {error, {unexpected_status, Status}}
	    end;
	Res ->
	    {error, Res}
    end.
