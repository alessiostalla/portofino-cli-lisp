# portofino-cli
Command-line utilities for [ManyDesigns Portofino](https://github.com/ManyDesigns/Portofino).

This is a work in progress. Only few commands are supported at the moment and error handling is not very user-friendly (but it's improving).

Generally, commands work by connecting to a Portofino running server – by default, `http://localhost:8080/`. You can change the address with `--url http://...`.

In the future, we may also support automatically launching a Portofino server against a given directory as soon as the first command is issued. However, that likely requires modifications or extensions to Portofino itself.

## Usage
Invoking the cli without any arguments prints some help text that you can use to discover commands.
```
$ ./portofino
Usage: 

NAME:
  portofino - The Portofino CLI

USAGE:
  portofino [global-options] [<command>] [command-options] [arguments ...]

OPTIONS:
      --help              display usage information and exit
      --version           display version and exit
  -U, --url <VALUE>       URL of the Portofino application [env: $PORTOFINO_URL]
  -p, --password <VALUE>  password to log in
  -u, --username <VALUE>  username to log in

COMMANDS:
  new     Create a new Portofino project
  action  Commands for working with resource-actions
  db      Commands for working with databases
  login   Login to a running Portofino instance
  logout  Log out of the application, i.e. delete the stored authentication
          token

AUTHORS:
Alessio Stalla <alessiostalla@gmail.com>
```
 
Similarly you can discover sub-commands by typing them:
```
$ ./portofino action
Usage: 

NAME:
  portofino action - Commands for working with resource-actions

USAGE:
  portofino [global-options] action [<command>] [command-options] [arguments ...]

OPTIONS:
      --help              display usage information and exit
      --version           display version and exit
  -U, --url <VALUE>       URL of the Portofino application [env: $PORTOFINO_URL]
  -p, --password <VALUE>  password to log in
  -u, --username <VALUE>  username to log in

COMMANDS:
  list-types  List resource-action types
  create      Create a new resource-action
  delete      Delete a resource-action
```

An interactive REPL is planned but not yet available.

## Installation

We don't provide (yet) binary packages, though I'm working on it. So, you'll have to build portofino-cli yourself. Please follow the build instructions.

__Note__ on Windows, you must have LibSSL 1.x installed. If the binary fails with an error loading libssl DLLs, please install OpenSSL __version 1.1.1__ from https://slproweb.com/products/Win32OpenSSL.html __(version 3.x won't fix the issue).__

## Building

First, clone this repo if you haven't done already:
```
git clone https://github.com/alessiostalla/portofino-cli
```
Make sure you have Roswell installed by following [its installation guide](https://github.com/roswell/roswell/wiki/Installation).

Run the following command (using a Bash-compatible shell which should also be available in recent versions of Windows with WSL):
```
ros run -- --eval "(progn (asdf:load-asd \"`pwd`/portofino-cli.asd\") (ql:quickload :portofino-cli) (asdf:make :portofino-cli/executable) (quit))"
```

An executable file named "portofino" should be created. On Windows, rename it to "portofino.exe".

You may want to add the executable to your PATH environment variable so that you'll be able to use the command `portofino` in every directory. Otherwise, you'll have to specify the command's path each time, e.g., `/home/alessio/portofino`.
 
## License
 
Portofino-cli is licensed under the AGPL. For normal use on your local computer this means that you're basically free to use it without problems. If, however, you build and distribute any kind of tool or service on top of it, you'll need to comply with the license and release its source code.
You can open an issue if you'd like to use portofino-cli as a component in a tool or service and require a more business-friendly license.
 
## Donations

You can help me develop and maintain this and other projects by donating to [my Patreon](https://www.patreon.com/alessiostalla).
