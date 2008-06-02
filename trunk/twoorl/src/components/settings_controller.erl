-module(settings_controller).
-export([index/1]).
-include("twoorl.hrl").

index(A) ->
    twoorl_util:auth(A, fun(Usr) -> process_request(A, Usr) end).


process_request(A, Usr) ->
    case yaws_arg:method(A) of
	'POST' ->
	    Params = yaws_api:parse_post(A),
	    Enabled = proplists:get_value("twitter_enabled", Params)  == "on",
	    Fun =
		if Enabled ->
			fun(Field, Val) ->
				case Val of
				    [] ->
					FName = case Field of
						    "twitter_username" ->
							"Twitter username";
						    "twitter_password" ->
							"Twitter password"
						end,
					{error, {missing_field, FName}};
				    _ ->
					ok
				end
			end;
		   true ->
			fun(_Field, _Val) ->
				ok
			end
		end,
	    
	    {[TwitterUsername, TwitterPassword], Errs} =
		erlyweb_forms:validate(
		  Params,
		  ["twitter_username", "twitter_password"],
		  Fun),
	    Checked = if Enabled ->
			      <<"checked">>;
			 true ->
			      []
		      end,
	    EmptyUsername = TwitterUsername == [],
	    EmptyPassword = TwitterPassword == [],
	    Errs1 =
		if not EmptyUsername andalso not EmptyPassword ->
			case twitter:verify_credentials(TwitterUsername, TwitterPassword) of
			    ok ->
				[];
			    {error, unauthorized} ->
				[twitter_unauthorized];
			    {error, Err} ->
				?Error("twitter authorization error: ~p ~p ~p",
				       [TwitterUsername, TwitterPassword, Err]),
				[twitter_authorization_error]
			end;
		   true ->
			[]
		end,
	    Errs2 = Errs ++ Errs1,
	    Messages =
		case Errs2 of
		    [] ->
			Usr1 =
			    usr:set_fields(
			      Usr,
			      [%{updated_on, {call, {now, []}}},
			       {twitter_username, TwitterUsername},
			       {twitter_password, TwitterPassword},
			       {twitter_enabled, if Enabled ->
							 1;
						    true ->
							 0
						 end}]),
			Usr2 = Usr1:save(),
			twoorl_util:update_session(A,Usr2),
			[{updated, <<"your settings">>}];
		    _ ->
			[]
		end,
	    {data, {TwitterUsername, TwitterPassword, Checked, Errs2,
		    Messages}};
	_ ->
	    {data, {str(usr:twitter_username(Usr)),
		    str(usr:twitter_password(Usr)),
		    case usr:twitter_enabled(Usr) of
			1 ->
			    <<"checked">>;
			0 ->
			    []
		    end,
		    [],
		    []}}
    end.

str(undefined) -> [];
str(Val) -> Val.
    

	    

