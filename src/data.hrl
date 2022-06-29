-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account,
    {account_number :: account_number(),
     firstname :: binary(),
     surname :: binary(),
     amount :: money()}).