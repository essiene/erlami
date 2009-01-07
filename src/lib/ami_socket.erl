% AMI socket is a special tcp socket that makes it easy to handle
% connected or disconnected ami states and can easily reconnect
% when the endpoint comes back online.
%
% States are:
%   Open-NotConnected
%   Open-Connected
%   Closed
% 
% This socket mimics the semantics of a normal gen_tcp socket
% connection and can operate in active -> true | once| false modes
% also. The only difference is the Signals delivered when active 
% is true or once are:
%    {ami, Socket, Data}, 
%    {ami_close, Socket}, 
%    {ami_error, Socket, Reason}
%
-module(ami_socket).
-behaviour(gen_fsm).
