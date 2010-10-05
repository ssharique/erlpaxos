% Maximum number instances that each agent can open simultaneously
% Instances are declared close when some value reaches the application
-define(MAX_OPEN_INSTANCES, 30).

% The agent does not send values immediately both up to application
% and down to proposer, it wakes up every AGENT_BUFFER_TIME milliseconds
% specifically for that task
-define(AGENT_BUFFER_TIME, 1600).

% Probability of losing a packet, simulates unreliable network
% set to 0 for reliable network, 100 to send no messages.
-define(PACKET_LOSS_PROBABILITY, 6).

% Proposer timeout  for collecting promises after 
% the prepare request is sent (in milliseconds)
-define(PROMISE_WAIT_TIMEOUT, 2000).

% Proposer timeout for receiving a confirmation after 
% the accept request is sent (in milliseconds)
-define(CONFIRMATION_WAIT_TIMEOUT, 3000).

% When learner receives WAIT_BEFORE_NOOP values with id > x
% whithout receiving x, it declares x dead (sends to app 
% 'noop as value')
-define(WAIT_BEFORE_NOOP, 45).

% -----------------------------------------------------------------

% Max number of proposers allowed
% (10, 100, 1000, ...)
-define(MAX_NUM_OF_PROPOSERS, 100).

-define(START_N, 87).
