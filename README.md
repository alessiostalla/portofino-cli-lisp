# portofino-cli-lisp

**Note this is no longer maintained and it's replaced by https://github.com/alessiostalla/portofino-cli**. This was an older implementation in Common Lisp before I developed portofino-client in TypeScript. Since development resources are limited, I decided to rewrite the CLI in TypeScript to leverage and test the TS client. However, I'm keeping this one around for:

1. History.
2. Hopefully serving as a nice starting example of how to design, build and distribute command-line Lisp applications, as it wasn't an easy road for me when I started.
3. Maybe one day when I have more time/money I'll resume work on this to have an alternative that doesn't require a Node installation, may support an interactive mode more easily, and contribute a tiny bit to the diffusion of Lisp.

------

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

We release [binary packages for Windows, Linux and OS X](https://github.com/alessiostalla/portofino-cli/releases). They don't require any installation (but see below for notes). You can just unpack them and add them to your PATH environment variable. 

If the pre-built binaries don't work for you, you can build portofino-cli from source on your machine, see [the next section](#building).

### Notes
__On Windows, you must have LibSSL 1.x installed.__ If the binary fails with an error loading libssl DLLs, please install OpenSSL __version 1.1.1__ from https://slproweb.com/products/Win32OpenSSL.html __(version 3.x won't fix the issue).__

__We don't sign (yet) the OSX binaries.__ Your OS will likely refuse to execute them. You may have to right-click on the "portofino" command-line executable and explicitly ask to Open it. OSX will ask for confirmation and only after you've given your consent it will allow you to run the executable from the command line.

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
