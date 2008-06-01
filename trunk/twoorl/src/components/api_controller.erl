%% This file is part of Twoorl.
%% 
%% Twoorl is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Twoorl is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Twoorl.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Copyright Yariv Sadan, 2008
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan, 2008

-module(api_controller).
-export([send/1, follow/1]).
-include("twoorl.hrl").

send(A) ->
    twoorl_util:auth(
      A,
      fun(Usr) ->
	      Params = yaws_api:parse_post(A),
	      {[Body], Errs} =
		  erlyweb_forms:validate(
		    Params, ["msg"],
		    fun("msg", Val) ->
			    case Val of
				[] ->
				    {error, empty_msg};
				_ ->
				    ok
			    end
		    end),
	      case Errs of
		  [] ->
		      {Body1, Names} = process_body(Body),
		      Msg = msg:new_with([{usr_username, Usr:username()},
					  {usr_id, Usr:id()},
					  {body, Body1},
					  {body_raw, Body}]),
		      Msg1 = Msg:save(),

		      Names1 = [tl(Name) || Name <- Names],

		      Recipients = 
			  usr:find({username, in, lists:usort(Names1)}),
		      
		      Replies =
			  [reply:new_with(
			     [{usr_id, Recipient:id()},
			      {msg_id, Msg1:id()}]) || Recipient <-
							  Recipients],
		      reply:insert(Replies),
		      
		      case proplists:get_value("get_html", Params) of
			  "true" ->
			      %% to get the timestamp (inefficient,
			      %% but who cares :) )
			      Msg2 = msg:find_id(Msg1:id()),
			      {ewc, timeline, show_msg, [A, Msg2]};
			  _ ->
			      {data, "ok"}
		      end;
		  _ ->

		      %% TODO need decent error reporting
		      exit(Errs)
	      end
      end).

follow(A) ->
    twoorl_util:auth(
      A,
      fun(Usr) ->
	      {[{Username, OtherUsrId}, Val], Errs} =
		  erlyweb_forms:validate(
		    A, ["username", "value"],
		    fun(F, V) ->
			    if V == [] ->
				    {error, {missing_field, F}};
			       F == "username" ->
				    OtherUsr = usr:find_first(
						 {username,'=',V}),
				    if OtherUsr == undefined ->
					    {error, {invalid_value, {F, V}}};
				       true ->
					    {ok, {V, OtherUsr:id()}}
				    end;
			       F == "value" ->
				    if V == "0" orelse V == "1" ->
					    {ok, V == "1"};
				       true ->
					    {error, {invalid_value, {F, V}}}
				    end
			    end
		    end),
	      Errs2 =
		  if Errs == [] ->
			  if Val ->
				  following:insert(
				    following:new_with(
				      [{usr_id1,Usr:id()},
				       {usr_id2,OtherUsrId},
				       {usr_username1,Usr:username()},
				       {usr_username2,Username}])),
				  [];
			     true ->
				  case following:delete_where(
					 {'and',
					  [
					   {usr_id1,'=',Usr:id()},
					   {usr_id2,'=',OtherUsrId}]}) of
				      0 ->
					  [{error, {not_following, Username}}];
				      1 ->
					  []
				  end
			  end;
		     true ->
			  Errs
		  end,

	      %% TODO need decent error reporting
	      if Errs2 == [] ->
		      {data, "ok"};
		 true ->
		      exit(Errs2)
	      end
      end).

	      
process_body(Body) ->
    Body1 = twoorl_util:htmlize(Body),
    Body2 = add_tinyurl_links(Body1),
    add_reply_links(Body2).

add_reply_links(Body) ->
    %% regexp:parse("@[A-Za-z0-9_]+")
    Re = {concat,64,
            {pclosure,{char_class,[95,{48,57},{97,122},{65,90}]}}},
    {Body1, Names, _LenDiff} = 
	twoorl_util:replace_matches(
	  Body, Re, fun([_ | Name] = Val) ->
			    twoorl_util:user_link(Name, Val)
		    end, ?MAX_TWOORL_LEN),
    {Body1, Names}.
	      
add_tinyurl_links(Body) ->
    %% regexp:parse("http://[^\s]+")
    Re = {concat,
	  {concat,
	   {concat,
	    {concat,{concat,{concat,{concat,104,116},116},112},58},
	    47},
	   47},
	  {pclosure,{comp_class," "}}},

    {Body2, _Links, _LenDiff} =
	twoorl_util:replace_matches(
	  Body, Re, fun twoorl_util:get_tinyurl/1, ?MAX_TWOORL_LEN),
    Body2.

