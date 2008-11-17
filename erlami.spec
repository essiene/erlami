Summary: Asterisk AMI library for Erlang
Name: erlami
Version: 0.3
Release: 1
License: Custom
Group: System/Development
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-build
Prereq: asterisk >= 1.4


%description
Erlami is an Asterisk AMI library for Erlang. It allows Erlang applications
to speak the Asterisk AMI protocol, either from a client perspective or from
a server perspective.

This implementation includes a barebones AMI Simulator (AmiSym) for testing
purposes only.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
make DESTDIR=%buildroot install

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
/usr/bin/*
/usr/lib/erlang/lib/*
