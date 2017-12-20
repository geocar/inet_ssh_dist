# inet\_ssh_dist

`inet_ssh_dist` is an Erlang distribution (network protocol) that runs *over* ssh.

Configuration could be as simple as,

    -proto_dist inet_ssh

and then everything goes over ssh. Even [epmd](http://www1.erlang.org/doc/man/epmd.html).

* No you don't need a special SSH server.
* No you don't need the [Erlang SSH server](http://erlang.org/doc/apps/ssh/using_ssh.html) (and in fact, this isn't even supported yet)

An ssh `known_hosts` must already be populated since the distribution will refuse to accept new hosts on an ad-hoc basis.

## Using as a road-warrior

If your Erlang cluster is already using `inet_dist_tcp` and you can ssh to your nodes, then you don't have to do anything
except use `-proto_dist inet_ssh` on your road-warrior machine, and tell Erlang what your node username is, e.g.

    -proto_dist inet_ssh -ssh_default_user erlang

## Using for cluster-communication

Note this isn't very fast, but if you can't get IPSEC working this is a lot easier than getting TLS working right.

### Creating a user for restricted port forwarding

First, make sure you know what port Erlang is going to use, by adding the following to the command line:

    -ssh_default_user erlang
    -inet_dist_listen_min 10069
    -inet_dist_listen_max 10069

Edit `/etc/ssh/sshd/config`:

    Match User erlang
      AllowTcpForwarding yes
      X11Forwarding no
      PermitTunnel no
      GatewayPorts no
      AllowAgentForwarding no
      PermitOpen localhost:10069
      ForceCommand echo 'This is a restricted account'

Reload sshd.  Then create the user:

    sudo useradd -m erlang

You can add keys manually (but not using `ssh-copy-id`).

### Adding an SSH key that can only be used for Erlang

First, make sure you know what port Erlang is going to use, by adding the following to the command line:

    -ssh_default_user erlang
    -inet_dist_listen_min 10069
    -inet_dist_listen_max 10069

Then add to the beginning of every restricted key in your `~/.ssh/authorized_keys` file:

    command="echo 'This is a restricted account'",no-agent-forwarding,no-X11-forwarding,permitopen="localhost:10069"

Here's a complete example, but obviously use your own key:

    command="echo 'This is a restricted account'",no-agent-forwarding,no-X11-forwarding,permitopen="localhost:10069" ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAQEAqrlwlnH3xpHntabqfgGBrMOPc83ShDCzTNskx+wQ30sScsONjikuyKQ0FV34RDGhSsd3VpNE8hUpYTVPPCI0wDgOZrUSKWGSbN9s6q1OOcaKRnuOxBguFdgimDemFuQ3VFj1hzZ0ZHt9tq442AQjpDdxHb8KBiiu/qziTvPVP0hzO7xty3ebBxxuRn7vSnKqswM8PQOqJXksok38PoxTDL2l9Nuz5vhl6gS8KA7szlGpve+EnYNgr9ob0QEm5TqKFbYwpaSuOCEQivc/m3urNUIis80sHP/PWFVK4sPc48cpvn6Tzosx+GK5j2KMynJVOES4Hc8LyRWysssBFQyZhw== rsa-key-20000000

### Verifying you've got a restricted SSH setup

First, make sure what doesn't work:

    ssh erlang@host #connection should be closed
    ssh erlang@host /bin/date #connection should be closed and date is not executed
    ssh -N -D 10069 erlang@host #wrong forwarding type
    ssh -N -L 8080:127.0.0.1:80 erlang@host #wrong port number
    sftp erlang@host #wrong subsystem

then verify what works:

    ssh -N -L 1234:localhost:10069 erlang@host
