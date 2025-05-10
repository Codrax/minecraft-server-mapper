# Minecraft Server Mapper
An cross-platform software that allows directing multiple minecraft servers to specific ports and addresses

## Another project
This app may seem similar to [minecraft-server-map](https://github.com/Codrax/minecraft-server-map), and it essentially does the same thing. The primary differences are that this version was written in Embarcadero Delphi and that version was written in Lazarus. This version is cross-platform and has more features

## Setting up the mapper:
To set up the application do as follows:
1) Move the binary to a folder where It can write It's configs file
2) Run the setup mode by providing the `--help` parameter
3) Enter all the information to first set up the configuration files

You can edit the configs at any time by changing the files, also, the files can be stored in another folder and you can specify a custom path for each using the appropiate parameter.

## Parameters
| Parameter | Description |
| ------------- | ------------- |
| -logfile | Echo log statements to log file. |
| -no-echo | Do not echo to the console. |
| -service-mode | Start application in service mode. Which disables input reading. |
| -config-file <path>  | Provide custom location for config file. |
| -default-mappings-file <path> | Provide custom location for default mapping file. |
| -mappings-file <path> | Provide custom location for the mappings file. |
| -create | Start application in setup mode. Which lets you create files and manage settings. |
| -open-dir | The working directory of the application, also where the files are searched for. |
| -help | Show this help dialog. |
| -version | Output version. |

Please keep in mind that Windows uses `-` for parameter prefixes and Linux uses `--`.

## Function
This application, uses a modified `TIdMappedPortTCP` from Indy, which captures the first handshake the client sends to the server, afterwhich, the Outbound connections is either established or discarded If there is a valid match for the adress.
![image](https://github.com/Codrax/minecraft-server-map/assets/68193064/91b6a2cf-4ca6-4c61-baa8-3a26145c8f21)

## Examples
The `config.json` file
```
{
    "listen-port": 25565,
    "file-monitor-delay": 2000,
    "allow-default-mapping": false,
    "expression-match": ""
}
```

The `mapping-default.json` file
```
{
    "host": "localhost",
    "port": 0
}
```

The `mappings.json` file
```
[
    {
        "address": "subdomain.website.com",
        "host": "localhost",
        "port": 25570
    }
]
```
