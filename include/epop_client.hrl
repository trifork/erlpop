-record(sk, {user,            % User name
    	     addr,            % Address
    	     sockfd,          % Socket filedesc.
    	     port=110,        % The POP3 server port number
    	     apop=false,      % Use APOP authentication if true.
    	     snoop=false,     % Trace on/off
    	     ssl=false        % SSL on/off
    	     }).