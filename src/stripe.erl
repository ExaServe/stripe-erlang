%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et
-module(stripe).

-export([token_create/11, token_create_bank/3]).
-export([customer_create/4,customer_create/5, customer_get/2, customer_update/4,customer_delete/2]).
-export([card_create/3,card_delete/3]).
-export([charge_customer/5, charge_card/5,charge_customer_card/6,charge_source/5,charge_source/6]).

-export([subscription_update/3, subscription_update/5,
  subscription_update/6, subscription_cancel/2, subscription_cancel/3]).
-export([customer/2, event/1, invoiceitem/1]).
-export([recipient_create/6, recipient_update/6]).
-export([transfer_create/5, transfer_cancel/1]).
-export([invoiceitem_create/4]).
-export([gen_paginated_url/1, gen_paginated_url/2,
  gen_paginated_url/3, gen_paginated_url/4]).
-export([get_all_customers/1, get_num_customers/1]).
-export([auth_key/0]).

-export([source/2]).
-export([sources_3d_secure/5]).
-include("stripe.hrl").

-define(VSN_BIN, <<"0.8.0">>).
-define(VSN_STR, binary_to_list(?VSN_BIN)).


%% test
%% -compile(export_all).

%%

% Stripe limit for paginated requests, change
% this number if stripe changes it in the future
% See: https://stripe.com/docs/api#pagination
-define(STRIPE_LIST_LIMIT, 100).
%%%--------------------------------------------------------------------
%%% Charging
%%%--------------------------------------------------------------------
-spec charge_customer(price(), currency(), customer_id(), desc(),auth_key()) -> result.
charge_customer(Amount, Currency, Customer, Desc,AuthKey) ->
  charge(Amount, Currency, {customer, Customer}, Desc,AuthKey).

-spec charge_card(price(), currency(), token_id(), desc(),auth_key()) -> result.
charge_card(Amount, Currency, Card, Desc,AuthKey) ->
  charge(Amount, Currency, {card, Card}, Desc,AuthKey).

-spec charge_source(price(), currency(), token_id(), desc(),auth_key()) -> result.
charge_source(Amount, Currency, Card, Desc,AuthKey) ->
  charge(Amount, Currency, {source, Card}, Desc,AuthKey).

-spec charge_source(price(), currency(), token_id(), customer_id(), desc(), auth_key()) -> result.
charge_source(Amount, Currency, Card, Customer, Desc,AuthKey) ->
  charge_customer_card(Amount, Currency, Card, Customer, Desc,AuthKey).

-spec charge_customer_card(price(), currency(), card_id(),customer_id(), desc(),auth_key()) -> result.
charge_customer_card(Amount, Currency, Card, Customer, Desc,AuthKey) when Amount > 50 ->
  Fields = [{amount, Amount},
    {currency, Currency},
    {source,Card},
    {customer,Customer},
    {description, Desc},
    {auth_key,AuthKey}],
  request_charge(Fields).

-spec charge(price(), currency(),
    {customer, customer_id()} | {card, token_id()}, desc(),auth_key()) -> result.
charge(Amount, Currency, CustomerOrCard, Desc,AuthKey) when
  Amount > 50 andalso is_tuple(CustomerOrCard) ->
  Fields = [{amount, Amount},
    {currency, Currency},
    CustomerOrCard,
    {description, Desc},
    {auth_key,AuthKey}],
  request_charge(Fields).

%%%--------------------------------------------------------------------
%%% Customer Creation
%%%--------------------------------------------------------------------
-spec customer_create(token_id(), email(), desc(),auth_key()) -> result.
customer_create(Card, Email, Desc,AuthKey) ->
  Fields = [{card, Card},
    {email, Email},
    {description, Desc},
    {auth_key,AuthKey}],
  request_customer_create(Fields).

%%%--------------------------------------------------------------------
%%% Customer Creation
%%%--------------------------------------------------------------------
-spec customer_create(token_id(), email(), binary(), desc(),auth_key()) -> result.
customer_create(Card, Email, ClientId, Desc,AuthKey) ->
  Fields = [{card, Card},
    {email, Email},
    {description, Desc},
    {'metadata[clientId_id]',ClientId},
    {auth_key,AuthKey}],
  request_customer_create(Fields).

%%%--------------------------------------------------------------------
%%% Customer Delete
%%%--------------------------------------------------------------------
-spec customer_delete(customer_id(),auth_key()) -> result.
customer_delete(CustomerId,AuthKey) ->
  request_customer_delete(CustomerId,AuthKey).


%%%--------------------------------------------------------------------
%%% Customer Fetching
%%%--------------------------------------------------------------------
-spec customer_get(customer_id(),auth_key()) -> result.
customer_get(CustomerId,AuthKey) ->
  request_customer(CustomerId,AuthKey).

%%%--------------------------------------------------------------------
%%% Customer Updating
%%%--------------------------------------------------------------------
-spec customer_update(customer_id(), token_id(), email(),any()) -> result.
customer_update(CustomerId, Token, Email,AuthKey) ->
  Fields = [{"card", Token},
    {"email", Email},
    {auth_key,AuthKey}],
  request_customer_update(CustomerId, Fields).

card_create(CustomerId, Token,AuthKey)->
  Fields = [{"card", Token},
    {auth_key,AuthKey}],
  request_card_create(CustomerId, Fields).

card_delete(CustomerId, CardId,AuthKey)->
  Fields = [{auth_key,AuthKey}],
  request_card_delete(CustomerId,CardId,Fields).


-spec sources_3d_secure(source_id(),price(),currency(),binary()|list(),any())->any().
sources_3d_secure(SourceId,Amount,Currency,ReturnUrl,AuthKey)->
  Fields =
    [{auth_key,AuthKey},
      {"amount",Amount},
      {"currency",Currency},
      {"type",<<"three_d_secure">>},
      {"redirect[return_url]",ReturnUrl},
      {"three_d_secure[card]",SourceId}
    ],

  request_sources_3d_secure(Fields).
%%%--------------------------------------------------------------------
%%% Token Generation
%%%--------------------------------------------------------------------
token_create(CardNumber, ExpMon, ExpYr, Cvc,Name, Addr1, Addr2, Zip, State, Country,AuthKey) ->
  Fields = [{"card[number]", CardNumber},
    {"card[exp_month]", ExpMon},
    {"card[exp_year]", ExpYr},
    {"card[cvc]", Cvc},
    {"card[name]", Name},
    {"card[address_line1]", Addr1},
    {"card[address_line2]", Addr2},
    {"card[address_zip]", Zip},
    {"card[address_state]", State},
    {"card[address_country]", Country},
    {"card[address_country]", Country},
    {auth_key,AuthKey}],
  request_token_create(Fields).

token_create_bank(Country, RoutingNumber, AccountNumber) ->
  Fields =
    [{"bank_account[country]", Country},
    {"bank_account[routing_number]", RoutingNumber},
    {"bank_account[account_number]", AccountNumber}],
  request_token_create(Fields).

%%%--------------------------------------------------------------------
%%% subscription updating/creation and removal
%%%--------------------------------------------------------------------
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd) ->
  subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, "").
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, Quantity) ->
  Fields = [{"plan", Plan},
    {"coupon", Coupon},
    {"prorate", Prorate},
    {"trial_end", TrialEnd},
    {"quantity", Quantity}],
  request_subscription(subscribe, Customer, Fields).

subscription_update(Customer, Subscription, Fields) ->
  request_subscription(update, Customer, Subscription, Fields).

subscription_cancel(Customer, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Fields, AtPeriodEnd).

subscription_cancel(Customer, Subscription, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Subscription, Fields, AtPeriodEnd).

%%%--------------------------------------------------------------------
%%% Recipient Management
%%%--------------------------------------------------------------------
recipient_create(Name, Type, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
    {type, Type},
    {tax_id, TaxId},
    {bank_account, Bank},
    {email, Email},
    {description, Desc}],
  request_recipient_create(Fields).

recipient_update(RecipientId, Name, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
    {tax_id, TaxId},
    {bank_account, Bank},
    {email, Email},
    {description, Desc}],
  request_recipient_update(RecipientId, Fields).

%%%--------------------------------------------------------------------
%%% Transfers (Payout) Management
%%%--------------------------------------------------------------------
transfer_create(Amount, Currency, RecipientId, Desc, StatementDesc) ->
  Fields = [{amount, Amount},
    {currency, Currency},
    {recipient, RecipientId},
    {description, Desc},
    {statement_descriptor, StatementDesc}],
  request_transfer_create(Fields).

transfer_cancel(TransferId) ->
  request_transfer_cancel(TransferId).

%%%--------------------------------------------------------------------
%%% event retrieval
%%%--------------------------------------------------------------------
event(EventId) ->
  request_event(EventId).

customer(CustomerId,AuthKey) ->
  request_customer(CustomerId,AuthKey).


source(SourceId, AuthKey)->
  request_source(SourceId, AuthKey).

%%%--------------------------------------------------------------------
%%% InvoiceItem Support
%%%--------------------------------------------------------------------

invoiceitem(InvoiceItemId) ->
  request_invoiceitem(InvoiceItemId).

invoiceitem_create(Customer, Amount, Currency, Description) ->
  Fields = [{customer, Customer},
    {amount, Amount},
    {currency, Currency},
    {description, Description}],
  request_invoiceitem_create(Fields).

%%%--------------------------------------------------------------------
%%% Pagination Support
%%%--------------------------------------------------------------------

get_all_customers(AuthKey) ->
  request_all_customers(AuthKey).

get_num_customers(Count) ->
  request_num_customers(Count).

%%%--------------------------------------------------------------------
%%% request generation and sending
%%%--------------------------------------------------------------------
request_charge(Fields) ->
  request(charges, post, Fields).

request_event(EventId) ->
  request_run(gen_event_url(EventId), get, []).

request_source(SourceId, AuthKey)->
  request_run(gen_source_url(SourceId),get,[{auth_key,AuthKey}]).

request_customer(CustomerId,AuthKey) ->
  request_run(gen_customer_url(CustomerId), get, [{auth_key,AuthKey}]).

request_customer_delete(CustomerId,AuthKey) ->
  request_run(gen_customer_url(CustomerId), delete, [{auth_key,AuthKey}]).

request_card_create(CustomerId,Fields)->
  request_run(gen_sources_url(CustomerId), post, Fields).

request_card_delete(CustomerId,CardId,Fields)->
  request_run(gen_sources_url(CustomerId,CardId), delete, Fields).

request_sources_3d_secure(Fields)->
  request_run(gen_sources_url(), post, Fields).

request_invoiceitem(InvoiceItemId) ->
  request_run(gen_invoiceitem_url(InvoiceItemId), get, []).

request_invoiceitem_create(Fields) ->
  request(invoiceitems, post, Fields).

request_customer_create(Fields) ->
  request(customers, post, Fields).

request_customer_update(CustomerId, Fields) ->
  request_run(gen_customer_url(CustomerId), post, Fields).

request_token_create(Fields) ->
  request(tokens, post, Fields).

request_recipient_create(Fields) ->
  request(recipients, post, Fields).

request_recipient_update(RecipientId, Fields) ->
  request_run(gen_recipient_url(RecipientId), post, Fields).

request_transfer_create(Fields) ->
  request(transfers, post, Fields).

request_transfer_cancel(TransferId) ->
  request_run(gen_transfer_cancel_url(TransferId), post, []).

request(Action, post, Fields) ->
  URL = gen_url(Action),
  request_run(URL, post, Fields).

request_subscription(subscribe, Customer, Fields) ->
  request_run(gen_subscription_url(Customer), post, Fields).

request_subscription(update, Customer, Subscription, Fields) ->
  request_run(gen_subscription_url(Customer, Subscription), post, Fields);

request_subscription(unsubscribe, Customer, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer), delete, Fields).

request_subscription(unsubscribe, Customer, Subscription, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer, Subscription) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Subscription,Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer, Subscription), delete, Fields).

request_all_customers(AuthKey) ->
  request_all(customers,AuthKey).

request_num_customers(Count) when Count =< ?STRIPE_LIST_LIMIT ->
  request_run(gen_paginated_url(customers, Count), get, []);
request_num_customers(Count) ->
  error_logger:error_msg("Requested ~p customers when ~p is the maximum allowed~n",
    [Count, ?STRIPE_LIST_LIMIT]).

%% Request all items in a pagination supported type
%% This will continue to call ?STRIPE_LIST_LIMIT items
%% until no items are remaining. If attempting to test
%% be sure to set large timeouts for listing huge accounts
request_all(Type,AuthKey) ->
  request_all(Type, [],AuthKey).
request_all(Type, StartingAfter,AuthKey) ->
  case request_run_all(gen_paginated_url(Type,
    ?STRIPE_LIST_LIMIT,
    StartingAfter),[{auth_key,AuthKey}]) of
    {error, Reason} ->
      {error, Reason};
    {false, Results} ->
      Results#stripe_list.data;
    {true, Results} ->
      TypeList = Results#stripe_list.data,
      LastElement = lists:last(TypeList),
      LastElementId = get_record_id(LastElement),
      TypeList ++ request_all(Type, LastElementId,AuthKey)
  end.

get_record_id(Type) when is_record(Type, stripe_customer) ->
  Type#stripe_customer.id;
get_record_id(Type) when is_record(Type, stripe_charge) ->
  Type#stripe_charge.id.

request_run(URL, Method, Fields) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
    {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
    {"Authorization", auth_key(Fields)}],
  Type = "application/x-www-form-urlencoded",
  Body = gen_args(Fields),

  Request = case Method of
  % get and delete are body-less http requests
              get -> {URL, Headers};
              delete -> {URL, Headers};
              _ -> {URL, Headers, Type, Body}
            end,
  Requested = httpc:request(Method, Request, [], []),

  resolve(Requested).

%% Much like request_run/3 except that a tuple is returned with the
%% results indicating more results are available
%% Returns:
%%   {error, Reason} - Same as request_run
%%   {false, Results} - No more results left, returns current page list
%%   {true, Results} - There are more results left, returns current page list
request_run_all(URL,Fields) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
    {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
    {"Authorization", auth_key(Fields)}],
  Request = {URL, Headers},
  Requested = httpc:request(get, Request, [], []),

  case resolve(Requested) of
    {error, _} = Error ->
      Error;
    Results ->
      {has_more(Requested), Results}
  end.

%% Simple function that checks if the body has more results in a paginated query
has_more({ok, {{_HTTPVer, _StatusCode, _Reason}, _Headers, Body}}) ->
  DecodedResult = mochijson2:decode(Body, [{format, proplist}]),
  proplists:get_value(<<"has_more">>, DecodedResult).

%%%--------------------------------------------------------------------
%%% response parsing
%%%--------------------------------------------------------------------
resolve({ok, {{_HTTPVer, StatusCode, _Reason}, _Headers, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({ok, {StatusCode, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({error, Reason}) ->
  {error, Reason}.

-spec resolve_status(pos_integer(), json()) ->
  #stripe_card{} | #stripe_token{} | #stripe_event{} |
  #stripe_customer{} | #stripe_error{}.
% success range conditions stolen from stripe-python
resolve_status(HTTPStatus, SuccessBody) when
  HTTPStatus >= 200 andalso HTTPStatus < 300 ->
  json_to_record(SuccessBody);
resolve_status(HTTPStatus, ErrorBody) ->
  json_to_error(HTTPStatus, ErrorBody).

%%%--------------------------------------------------------------------
%%% Json to local type object records
%%%--------------------------------------------------------------------
-define(NRAPI, <<"Not Returned by API">>).
-define(V(X), proplists:get_value(atom_to_binary(X, utf8),
  DecodedResult, ?NRAPI)).

json_to_record(Json) when is_list(Json) andalso is_tuple(hd(Json)) ->
  json_to_record(proplists:get_value(<<"object">>, Json), Json);

json_to_record(Body) when is_list(Body) orelse is_binary(Body) ->
  DecodedResult = mochijson2:decode(Body, [{format, proplist}]),
  json_to_record(DecodedResult).

% Yes, these are verbose and dumb because we don't have runtime record/object
% capabilities.  In a way, it's nice being explicit up front.
-spec json_to_record(stripe_object_name(), proplist()) -> #stripe_list{}.
json_to_record(<<"list">>, DecodedResult) ->
  Data = ?V(data),
  #stripe_list{data = [json_to_record(Object) || Object <- Data]};

json_to_record(<<"list_to_proplist">>, DecodedResult) ->
  Data = ?V(data),
  [json_to_record(Object) || Object <- Data];

json_to_record(<<"event">>, DecodedResult) ->
  Data = ?V(data),
  Object = proplists:get_value(<<"object">>, Data),
  ObjectName = proplists:get_value(<<"object">>, Object),
  #stripe_event{id      = ?V(id),
    type    = ?V(type),
    created = ?V(created),
    data    = json_to_record(ObjectName, Object)};

json_to_record(<<"charge">>, DecodedResult) ->
  #stripe_charge{id           = ?V(id),
    created      = ?V(created),
    amount       = ?V(amount),
    balance_transaction = ?V(balance_transaction),
    currency     = check_to_atom(?V(currency)),
    description  = ?V(description),
    livemode     = ?V(livemode),
    paid         = ?V(paid),
    refunded     = ?V(refunded),
    customer     = ?V(customer),
    failure_code = ?V(failure_code),
    failure_message = ?V(failure_message),
    card         = proplist_to_card(?V(card))};

json_to_record(<<"token">>, DecodedResult) ->
  #stripe_token{id        = ?V(id),
    used      = ?V(used),
    livemode  = ?V(livemode),
    card = proplist_to_card(?V(card)),
    bank_account = proplist_to_bank_account(?V(bank_account))};

json_to_record(<<"customer">>, DecodedResult) ->
  Data=json_to_record(<<"list_to_proplist">>,proplists:get_value(<<"sources">>,DecodedResult)),

  #stripe_customer{id              = ?V(id),
    description     = ?V(description),
    livemode        = ?V(livemode),
    created         = ?V(created),
    email           = ?V(email),
    delinquent      = ?V(delinquent),
    data            = Data,
    discount        = json_to_record(<<"discount">>, ?V(discount)),
    account_balance = ?V(account_balance)};

% We don't have eunit tests for discount decoding yet.  Use at your own risk.
json_to_record(<<"discount">>, null) -> null;
json_to_record(<<"discount">>, DecodedResult) ->
  #stripe_discount{coupon   = json_to_record(coupon, ?V(coupon)),
    start    = ?V(start),
    'end'    = ?V('end'),
    customer = ?V(customer)
  };

% We don't have eunit tests for coupon decoding yet.  Use at your own risk.
json_to_record(<<"coupon">>, null) -> null;
json_to_record(<<"coupon">>, DecodedResult) ->
  #stripe_coupon{id                 = ?V(id),
    percent_off        = ?V(percent_off),
    amount_off         = ?V(amount_off),
    currency           = check_to_atom(?V(currency)),
    duration           = ?V(duration),
    redeem_by          = ?V(redeem_by),
    max_redemptions    = ?V(max_redemptions),
    times_redeemed     = ?V(times_redeemed),
    duration_in_months = ?V(duration_in_months)
  };

json_to_record(<<"subscription">>, null) -> null;
json_to_record(<<"subscription">>, DecodedResult) when is_list(DecodedResult) ->
  #stripe_subscription{id                   = ?V(id),
    status               = check_to_atom(?V(status)),
    current_period_start = ?V(current_period_start),
    current_period_end   = ?V(current_period_end),
    trial_start          = ?V(trial_start),
    trial_end            = ?V(trial_end),
    ended_at             = ?V(ended_at),
    canceled_at          = ?V(canceled_at),
    customer             = ?V(customer),
    start                = ?V(start),
    quantity             = ?V(quantity),
    plan                 = proplist_to_plan(?V(plan))};

json_to_record(<<"invoiceitem">>, DecodedResult) ->
  #stripe_invoiceitem{id           = ?V(id),
    amount       = ?V(amount),
    currency     = check_to_atom(?V(currency)),
    customer     = ?V(customer),
    date         = ?V(date),
    description  = ?V(description),
    proration    = ?V(proration)};

json_to_record(<<"recipient">>, DecodedResult) ->
  #stripe_recipient{id           = ?V(id),
    created      = ?V(created),
    type         = check_to_atom(?V(type)),
    active_account = proplist_to_bank_account(?V(active_account)),
    verified     = ?V(verified),
    description  = ?V(description),
    name         = ?V(name),
    email        = ?V(email)};

json_to_record(<<"transfer">>, DecodedResult) ->
  #stripe_transfer{id           = ?V(id),
      amount       = ?V(amount),
      currency     = check_to_atom(?V(currency)),
      date         = ?V(date),
      balance_transaction = ?V(balance_transaction),
      status       = check_to_atom(?V(status)),
      account      = proplist_to_bank_account(?V(account)),
      description  = ?V(description),
      recipient    = ?V(recipient),
      statement_descriptor = ?V(statement_descriptor)};

json_to_record(undefined, [{<<"deleted">>, Status}, {<<"id">>, ObjectId}]) ->
    #stripe_delete{id     = ObjectId,
                   status = Status};

json_to_record(<<"card">>,DecodedResult) ->

  #stripe_card{
      id                  = ?V(id),
      name                = ?V(name),
      last4               = ?V(last4),
      exp_month           = ?V(exp_month),
      exp_year            = ?V(exp_year),
      brand               = ?V(brand),
      cvc_check           = check_to_atom(?V(cvc_check)),
      address_line1_check = check_to_atom(?V(address_line1_check)),
      address_zip_check   = check_to_atom(?V(address_zip_check)),
      country             = ?V(country)};

json_to_record(<<"source">>,DecodedResult)->
  Type= proplists:get_value(<<"type">>,DecodedResult,<<>>),
  json_to_record({<<"source">>,Type},DecodedResult);


json_to_record({<<"source">>,<<"three_d_secure">>},DecodedResult)->

  Redirect=proplists:get_value(<<"redirect">>,DecodedResult,[]),
  RedirectUrl= proplists:get_value(<<"url">>,Redirect,<<"/">>),
  RedirectReturnUrl= proplists:get_value(<<"return_url">>,Redirect,<<"/">>),
  RedirectStatus= proplists:get_value(<<"status">>,Redirect,<<>>),


  ThreeDSecure=proplists:get_value(<<"three_d_secure">>,DecodedResult,[]),
  SourceCardId= proplists:get_value(<<"card">>,ThreeDSecure,<<>>),
  SourceCardCustomer= proplists:get_value(<<"customer">>,ThreeDSecure,<<>>),
  SourceCardAuth= proplists:get_value(<<"authenticated">>,ThreeDSecure,false),

  #stripe_source_three_d{
    id                            =   ?V(id),
    amount                        =   ?V(amount),
    currency                      =   ?V(currency),
    client_secret                 =   ?V(client_secret),
    flow                          =   ?V(flow),
    redirect_return_url           =   RedirectReturnUrl,
    redirect_status               =   RedirectStatus,
    redirect_url                  =   RedirectUrl,
    status                        =   ?V(status),
    usage                         =   ?V(usage),
    three_d_secure_card           =   SourceCardId,
    three_d_secure_customer       =   SourceCardCustomer,
    three_d_secure_authenticated  =   SourceCardAuth
  };


json_to_record({<<"source">>,<<"card">>},DecodedResult)->

  SourceCard= proplists:get_value(<<"card">>,DecodedResult,[]),
  Name= proplists:get_value(<<"name">>,SourceCard,<<>>),
  Last4= proplists:get_value(<<"last4">>,SourceCard,<<>>),
  ExpYear= proplists:get_value(<<"exp_year">>,SourceCard,<<>>),
  ExpMonth= proplists:get_value(<<"exp_month">>,SourceCard,<<>>),
  Country= proplists:get_value(<<"country">>,SourceCard,<<>>),
  ThreeDSecure= proplists:get_value(<<"three_d_secure">>,SourceCard,<<>>),
  Brand= proplists:get_value(<<"brand">>,SourceCard,<<>>),


  #stripe_source_card{
    id                            =   ?V(id),
    client_secret                 =   ?V(client_secret),
    status                        =   ?V(status),
    customer                      =   ?V(customer),
    usage                         =   ?V(usage),
    brand                         =   Brand,
    name                          =   Name,
    last4                         =   Last4,
    exp_month                     =   ExpMonth,
    exp_year                      =   ExpYear,
    country                       =   Country,
    three_d_secure                =   ThreeDSecure
  };

json_to_record({<<"source">>,_},DecodedResult)->

  Redirect=proplists:get_value(<<"redirect">>,DecodedResult,[]),
  RedirectUrl= proplists:get_value(<<"url">>,Redirect,<<"/">>),
  RedirectStatus= proplists:get_value(<<"status">>,Redirect,<<>>),
  ThreeDSecure=proplists:get_value(<<"three_d_secure">>,DecodedResult,[]),
  SourceCardId= proplists:get_value(<<"card">>,ThreeDSecure,<<>>),
  SourceCardAuth= proplists:get_value(<<"authenticated">>,ThreeDSecure,false),

  #stripe_source{
    id =?V(id),
    client_secret=?V(client_secret),
    flow=?V(flow),
    redirect_url=RedirectUrl,
    redirect_status=RedirectStatus,
    status=?V(status),
    amount= ?V(amount),
    currency=?V(currency),
    three_d_secure_card_id=SourceCardId,
    three_d_secure_card_authenticated=SourceCardAuth,
    raw_data = DecodedResult
  };



json_to_record(Type, DecodedResult) ->
  error_logger:error_msg({unimplemented, ?MODULE, json_to_record, Type, DecodedResult}),
  {not_implemented_yet, Type, DecodedResult}.

proplist_to_card(null) -> null;
proplist_to_card(A) when is_binary(A) -> A;
proplist_to_card(Card) ->
  DecodedResult = Card,
  #stripe_card{name                = ?V(name),
      last4               = ?V(last4),
      exp_month           = ?V(exp_month),
      exp_year            = ?V(exp_year),
      brand               = ?V(brand),
      cvc_check           = check_to_atom(?V(cvc_check)),
      address_line1_check = check_to_atom(?V(address_line1_check)),
      address_zip_check   = check_to_atom(?V(address_zip_check)),
      country             = ?V(country)}.

proplist_to_plan(Plan) ->
  DecodedResult = Plan,
  #stripe_plan{id             = ?V(id),
      currency       = check_to_atom(?V(currency)),
      interval       = ?V(interval),
      interval_count = ?V(interval_count),
      name           = ?V(name),
      amount         = ?V(amount),
      livemode       = ?V(livemode)}.

proplist_to_bank_account(null) -> null;
proplist_to_bank_account(A) when is_binary(A) -> A;
proplist_to_bank_account(BankAccount) ->
  DecodedResult = BankAccount,
  #stripe_bank_account{fingerprint = ?V(fingerprint),
      bank_name  = ?V(bank_name),
      last4      = ?V(last4),
      country    = ?V(country),
      validated  = ?V(validated),
      description = ?V(description),
      recipient  = ?V(recipient),
      statement_descriptor = ?V(statement_descriptor)}.

check_to_atom(null) -> null;
check_to_atom(A) when is_atom(A) -> A;
check_to_atom(Check) when is_binary(Check) -> binary_to_atom(Check, utf8).


% error range conditions stolen from stripe-python
json_to_error(ErrCode, Body) ->
  ErrCodeMeaning = case ErrCode of
                     400 -> missing_param;
                     401 -> bad_api_key;
                     402 -> params_ok_but_request_failed;
                     404 -> notfound;
                     E when E >= 500 -> stripe_server_error;
                     E when E =:= 403 orelse E > 404 -> stripe_api_error;
                     _ -> unknown_error
                   end,
  json_to_error(ErrCode, ErrCodeMeaning, Body).


% Let's use a common error object/record instead of breaking out per-type
% errors.  We can match on error types easily.
json_to_error(ErrCode, ErrCodeMeaning, Body) ->
  PreDecoded = mochijson2:decode(Body, [{format, proplist}]),
  DecodedResult = proplists:get_value(<<"error">>, PreDecoded),
  #stripe_error{type    = check_to_atom(?V(type)),
    code    = check_to_atom(?V(code)),
    http_error_code = ErrCode,
    http_error_code_meaning = ErrCodeMeaning,
    message = ?V(message),
    param   = ?V(param)}.

%%%--------------------------------------------------------------------
%%% value helpers
%%%--------------------------------------------------------------------
ua_json() ->
  Props = [{<<"bindings_version">>, ?VSN_BIN},
    {<<"lang">>, <<"erlang">>},
    {<<"publisher">>, <<"mattsta">>}],
  binary_to_list(iolist_to_binary(mochijson2:encode(Props))).

auth_key(Fields) ->
  case proplists:get_value(auth_key,Fields,<<>>)of
    <<>>->
      auth_key();
    AuthKey->
      Token = binary_to_list(AuthKey),
      Auth = base64:encode_to_string(Token ++ ":"),
      "Basic " ++ Auth
  end.

auth_key() ->
  Token = env(auth_token),
  Auth = base64:encode_to_string(Token ++ ":"),
  "Basic " ++ Auth.

env(What) ->
  case env(What, diediedie) of
    diediedie -> throw({<<"You must define this in your app:">>, What});
    Else -> Else
  end.

env(What, Default) ->
  case application:get_env(stripe, What) of
    {ok, Found} -> Found;
    undefined -> Default
  end.

-spec gen_args(proplist()) -> string().
gen_args([]) -> "";
gen_args(Fields) when is_list(Fields) andalso is_tuple(hd(Fields)) ->
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>> andalso K =/= auth_key],
  mochiweb_util:urlencode(OnlyWithValues).

gen_url(Action) when is_atom(Action) ->
  gen_url(atom_to_list(Action));
gen_url(Action) when is_list(Action) ->
  "https://api.stripe.com/v1/" ++ Action.

gen_customer_url(CustomerId) when is_binary(CustomerId) ->
  gen_customer_url(binary_to_list(CustomerId));
gen_customer_url(CustomerId) when is_list(CustomerId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId.

gen_recipient_url(RecipientId) when is_binary(RecipientId) ->
  gen_recipient_url(binary_to_list(RecipientId));
gen_recipient_url(RecipientId) when is_list(RecipientId) ->
  "https://api.stripe.com/v1/recipients/" ++ RecipientId.

gen_transfer_cancel_url(TransferId) when is_binary(TransferId) ->
  gen_transfer_cancel_url(binary_to_list(TransferId));
gen_transfer_cancel_url(TransferId) when is_list(TransferId) ->
  "https://api.stripe.com/v1/transfers/" ++ TransferId ++ "/cancel".

gen_invoiceitem_url(InvoiceItemId) when is_binary(InvoiceItemId) ->
  gen_invoiceitem_url(binary_to_list(InvoiceItemId));
gen_invoiceitem_url(InvoiceItemId) when is_list(InvoiceItemId) ->
  "https://api.stripe.com/v1/invoiceitems/" ++ InvoiceItemId.

gen_subscription_url(Customer) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer));
gen_subscription_url(Customer) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscription".

gen_subscription_url(Customer, Subscription) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer), Subscription);
gen_subscription_url(Customer, Subscription) when is_binary(Subscription) ->
  gen_subscription_url(Customer, binary_to_list(Subscription));
gen_subscription_url(Customer, Subscription) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscriptions/" ++ Subscription.

gen_event_url(EventId) when is_binary(EventId) ->
  gen_event_url(binary_to_list(EventId));
gen_event_url(EventId) when is_list(EventId) ->
  "https://api.stripe.com/v1/events/" ++ EventId.


gen_paginated_url(Type) ->
  gen_paginated_url(Type, 10, [], []).

gen_paginated_url(Type, Limit) ->
  gen_paginated_url(Type, Limit, [], []).

gen_paginated_url(Type, Limit, StartingAfter) ->
  gen_paginated_url(Type, Limit, StartingAfter, []).

gen_paginated_url(Type, Limit, StartingAfter, EndingBefore) ->
  Arguments = gen_args([{"limit", Limit},
    {"starting_after", StartingAfter},
    {"ending_before", EndingBefore}]),
  gen_paginated_base_url(Type) ++ Arguments.

gen_paginated_base_url(charges) ->
  "https://api.stripe.com/v1/charges?";
gen_paginated_base_url(customers) ->
  "https://api.stripe.com/v1/customers?";
gen_paginated_base_url(invoices) ->
  "https://api.stripe.com/v1/invoices?".




gen_source_url(SourceId)  when is_binary(SourceId) ->
  gen_source_url(binary_to_list(SourceId));
gen_source_url(SourceId) when is_list(SourceId)->
  "https://api.stripe.com/v1/sources/"++SourceId.


gen_sources_url() ->
  "https://api.stripe.com/v1/sources".

gen_sources_url(CustomerId) when is_binary(CustomerId) ->
  gen_sources_url(binary_to_list(CustomerId));
gen_sources_url(CustomerId) when is_list(CustomerId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId ++"/sources".

gen_sources_url(CustomerId,CardId) when is_binary(CustomerId) and is_binary(CardId) ->
  gen_sources_url(binary_to_list(CustomerId),binary_to_list(CardId));
gen_sources_url(CustomerId,CardId) when is_list(CustomerId) and is_binary(CardId) ->
  gen_sources_url(CustomerId,binary_to_list(CardId));
gen_sources_url(CustomerId,CardId) when is_binary(CustomerId) and is_list(CardId) ->
  gen_sources_url(binary_to_list(CustomerId),CardId);
gen_sources_url(CustomerId,CardId) when is_list(CustomerId) and is_list(CardId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId ++"/sources/"++CardId.