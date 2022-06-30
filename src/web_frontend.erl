-module(web_frontend).
-export([init/2]).
-include("data.hrl").

% { account_service, Count, create_account, #account {account_number, name, surname, amount} }

% single_account(Account) -> 
%   io_lib:format(<< "
% <ul><li> Name: ~p Surname: ~p Balance: ~p Id: ~p </li> </ul>~n
% " >>, [Account#account.firstname, Account#account.surname, Account#account.amount, Account#account.account_number]).

success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
      <p> It could take several minutes until the account is ready for transfers </p>
      <a href=\"/\"> Back </a>
    " >>.


form() ->
    << "
<h3> Open Account </h3>
<form method=\"post\" action=\"/accounts/open\">
  <label for=\"accounts_firstname\"> Firstname </label>
  <input type=\"text\" id=\"accounts_firstname\" name=\"accounts_firstname\" />

  <label for=\"accounts_secondname\"> Secondname </label>
  <input type=\"text\" id=\"accounts_secondname\" name=\"accounts_secondname\" />

  <input type=\"submit\" value=\"Open account\" />
</form>" >>.



init(Req, add) ->
    lager:info("Creating new account"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),

    Accountnumber = database:unique_account_number(),
    Firstname = maps:get(<<"accounts_firstname">>, KeyValues),
    Secondname = maps:get(<<"accounts_secondname">>, KeyValues),
    Amount = 1000,

    NewAccount = #account{account_number = Accountnumber, firstname = Firstname, surname = Secondname, amount = Amount},
    
    database:put_account(NewAccount),
    accounts_feed:broadcast_new_account(NewAccount),
    events:put_event(NewAccount),
    lager:info("Created account with account number ~p", [Accountnumber]),

    Body = io_lib:format(success(), [Accountnumber]),  

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
    
    {ok, Req2, []};

init(Req0, index) ->
    ResponseBody = form(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           ResponseBody,
                           Req0),
    {ok, Req, []}.
