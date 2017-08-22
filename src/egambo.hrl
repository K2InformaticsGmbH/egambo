-ifndef(_EGAMBO_HRL_).
-define(_EGAMBO_HRL_, true).

-define(LOG_TAG, "_GMBO_").
-include_lib("dderl/src/dderl.hrl").

-type egAccountId() :: ddEntityId().    % imem AccountId

-define(PLAYER_GID(__PlayerId), {global, {player, __PlayerId}}).

-endif.
