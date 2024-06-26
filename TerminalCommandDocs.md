Shell Commands Documentation

Below is the documentation for the available shell commands, including descriptions and usage examples.
---------
cmd-uptime
Description: Displays the system uptime, the number of users currently logged in, and the load averages.
Usage: (cmd-uptime nil)
---------
cmd-ln
Description: Creates a symbolic link to a file.
Usage: (cmd-ln '("<target>" "<linkname>"))

<targe-t>: The file to which the symbolic link points.
<linkname>: The name of the symbolic link to be created.
---------
cmd-clear
Description: Clears the terminal screen.
Usage: (cmd-clear nil)
---------
cmd-history
Description: Displays the command history.
Usage: (cmd-history nil)
---------
cmd-alias
Description: Sets an alias for a command.
Usage: (cmd-alias '("<name>" "<command>"))

<name>: The alias name.
<command>: The command that the alias represents.
---------
cmd-unalias
Description: Removes an alias.
Usage: (cmd-unalias '("<name>"))

<name>: The alias name to be removed.
---------
cmd-export
Description: Sets an environment variable.
Usage: (cmd-export '("<name>" "<value>"))

<name>: The name of the environment variable.
<value>: The value to set for the environment variable.
---------
cmd-env
Description: Lists all environment variables.
Usage: (cmd-env nil)
---------
cmd-set
Description: Sets a shell variable.
Usage: (cmd-set '("<name>" "<value>"))

<name>: The name of the shell variable.
<value>: The value to set for the shell variable.
---------
cmd-unset
Description: Removes a shell variable.
Usage: (cmd-unset '("<name>"))

<name>: The name of the shell variable to be removed.
---------
cmd-bg
Description: Moves a job to the background.
Usage: (cmd-bg '("<job-id>"))

<job-id>: The ID of the job to be moved to the background.
---------
cmd-fg
Description: Brings a job to the foreground.
Usage: (cmd-fg '("<job-id>"))

<job-id>: The ID of the job to be brought to the foreground.
---------
cmd-jobs
Description: Lists all jobs.
Usage: (cmd-jobs nil)
---------
cmd-nice
Description: Sets the priority of a process when starting it.
Usage: (cmd-nice '("<priority>" "<command>"))

<priority>: The priority level to set.
<command>: The command to run with the specified priority.
---------
cmd-renice
Description: Changes the priority of a running process.
Usage: (cmd-renice '("<priority>" "<pid>"))

<priority>: The new priority level.
<pid>: The process ID of the running process to change the priority of.
---------
cmd-shutdown
Description: Shuts down the system.
Usage: (cmd-shutdown nil)
---------
cmd-reboot
Description: Reboots the system.
Usage: (cmd-reboot nil)
---------
cmd-log
Description: Views system logs.
Usage: (cmd-log nil)
---------
cmd-dmesg
Description: Displays kernel messages.
Usage: (cmd-dmesg nil)
---------
cmd-ifconfig
Description: Configures network interfaces.
Usage: (cmd-ifconfig '("<args>"))

<args>: Arguments for the ifconfig command.
---------
cmd-ping
Description: Tests network connectivity.
Usage: (cmd-ping '("<destination>" "<options>"))

<destination>: The destination to ping.
<options>: Additional options for the ping command.
---------
cmd-netstat
Description: Displays network statistics.
Usage: (cmd-netstat '("<options>"))

<options>: Additional options for the netstat command.
---------
cmd-ps
Description: Lists running processes.
Usage: (cmd-ps nil)
---------
cmd-kill
Description: Sends a signal to a process to terminate it.
Usage: (cmd-kill '("<pid>" "<signal>"))

<pid>: The process ID to send the signal to.
<signal>: The signal to send (default is SIGTERM).
---------
cmd-df
Description: Displays disk usage information.
Usage: (cmd-df nil)
---------
cmd-du
Description: Displays directory size information.
Usage: (cmd-du '("<directory>"))

<directory>: The directory to calculate the size of (default is the current directory).
---------
cmd-free
Description: Displays memory usage information.
Usage: (cmd-free nil)
---------
cmd-top
Description: Displays real-time process and system information.
Usage: (cmd-top nil)
---------
cmd-uname
Description: Displays system information.
Usage: (cmd-uname nil)
---------
cmd-who
Description: Displays user information.
Usage: (cmd-who nil)
---------
cmd-whoami
Description: Displays the current user.
Usage: (cmd-whoami nil)
--------
initialize-network-interface command:

Description: Initializes a network interface by creating an instance of the network-interface class. The user provides the MAC address and IP address interactively during runtime.

Usage:
(initialize-network-interface)

The function prompts the user to enter the MAC address (e.g., “00:11:22:33:44:55”) and the IP address (e.g., “192.168.1.100”).
The entered values are used to create a network interface object with the specified MAC address and IP address.
--------
