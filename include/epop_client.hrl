-record(sk, {user,            % User name
    	     addr,            % Address
    	     sockfd,          % Socket filedesc.
    	     port=110,        % The POP3 server port number
    	     apop=false,      % Use APOP authentication if true.
    	     snoop=false,     % Trace on/off
             encryption=tcp :: tcp | ssl | tls, % Requested encryption
    	     ssl=undefined    % Actual encryption: SSL on/off
    	     }).
