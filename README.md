# Description

Download attributes as files from an LDAP server.

# Usage

```
$ ldapget --help
Get values from LDAP

Usage: ldapget [--version] [--help] [-h|--host HOST] [-p|--port PORT]
               [-d|--dn DN] (-P|--project PROJECT) [-a|--attribute ATTR]
               (-o|--outdir OUT)

Available options:
  --version                Show version
  --help                   Show this help text
  -h,--host HOST           Hostname of the LDAP server (default: "localhost")
  -p,--port PORT           Port on the LDAP server to connect to (default: 389)
  -d,--dn DN               LDAP Distinguished
                           Name (default: "dc=futuregrid,dc=org")
  -P,--project PROJECT     Project ID
  -a,--attribute ATTR      Attribute to select (default: "sshPublicKey")
  -o,--outdir OUT          Output directory
```

# Example

## Download SSH Public keys for all users in a FutureSystems project

```
$ ldapget --host localhost --port 9389 --project fg475 --outdir public_keys
```

This will download the public keys for all users registered to the `fg475` project into:

```
public_keys/<username>/[0..N-1]
```

where `N` is the number of keys the user has uploading using the portal.


# Suggestion

You may need to forward access to the LDAP server.
This can be done using SSH Port Forwarding with a proxy server:

- `ldap`: the server running ldap
- `local`: your local machine
- `proxy`: a machine with SSH access to `ldap` and `local`

First: forward `ldap:389` to `proxy:9389`:

```
user@proxy $ ssh -g -L \*:9389:ldap:389 localhost -v
```

Next: forward the port the the `local` machine:

```
user@proxy $ ssh -g -R 9389:localhost:9389 local -v
```
