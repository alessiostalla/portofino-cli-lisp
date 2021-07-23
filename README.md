# portofino-cli
Command-line utilities for ManyDesigns Portofino.

This is a work in progress. Only few commands are supported at the moment and error handling is not very user-friendly.

Generally, commands work by connecting to a Portofino running server – by default, `http://localhost:8080/`. You can change the address with a few cli switches:
```
--host www.example.com
--port 12345
--path my-app/api
--protocol https
```
The above will connect to a Portofino instance running at `https://www.example.com:12345/my-app/api`.

## Usage
Invoking the cli without any arguments prints some help text that you can use to discover commands.
```
$ ./portofino
Please, specify a command.

Here is a list of all supported commands:

These commands are supported:

 * action - Commands for working with resource-actions
 * login - Login to a running Portofino instance
 * logout - Log out deleting the stored token
 * new - Create a new Portofino project
 ```
 
Similarly you can discover sub-commands by typing them:
```
$ ./portofino action
Please, specify a command.

Here is a list of all supported commands:

These commands are supported:

 * create - Create a new resource-action
 * delete - Delete an action
 * list-types - List resource-action types
 ```
 
 An interactive REPL is planned but not yet available.
