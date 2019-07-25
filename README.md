# anonymizer

Atidot Anonymizer Monad and tools

## Goals

1. Iteration on data keys in various data formats, such as: csv, json, xml & etc.
1. guranteed privacy on each operation
1. allow easy anonymization of data by analysing the key
1. easy declarative programming interface

## Code Examples

TBA

## modes

### Notebook Server

#### How to run

```bash
cd build
make notebook
```

The notebook server will be opened in browser

#### how to Run in a Docker

```bash
cd build
make notebook-docker
docker run -it -v </path/to/notebooks/folder>:/notebooks --network="host" anonymizer-notebook
```

#### Usage

TBA

### Swagger Spec

To print Swagger spec of the API server, run:

```bash
cd build
make swagger
```

### api-server

the anonymizer API server runs in a docker

#### How to Run

```bash
cd build
make api-server
docker run -it  -v /home/talz/.anonymizer-secrets:/var/.secrets --network="host" anonymizer-api
```

notes:

* `host-secret-folder` must contain the following files: `ssl-key.key`,`ssl-cert.cert`, `hashing-key.key`
* port is set defaultly to 443

#### Modifing docker environment variables

additional control over the server is given by the following env variables:

* SSL_KEY
* SSL_CERT
* HASH_KEY
* PORT

for example, if we wanted to run the docker on port 444:

```bash
docker run -it  -v /home/talz/.anonymizer-secrets:/var/.secrets --network="host" -e PORT=444 anonymizer-api
```

#### Usage

TBA

## roadmap & future tasks

TBA

1. better wrappers for the monad
1. different carriers hold properties in different places in the xml, other expected places to hold the data contain null then, and we should ignore them and check the next possible path

solution: incrementing operator

2. before hashing, we want to attach unique id (anonymized uuid) to each policy
   then, we should hash
   then we should send back:
   1. the anonymized xml
   2. the unanonymized xml with the anonymization uuid in it (per policy)

3.
path =~ ["Address"] <- this can be addressed to, without the brackets
a.k.a

path =~~ "Address"

4. maybe hash may be depend on depth in the tree

for example:



<a>
<b>
tal
<\b>
<\a>

should be hashed for the depth of 2, and

<a>
<b>
<c>
tal
<\c>
<\b>
<\a>

shouldn't be hashed for the depth of 2


1. allow interface to figure to analyze statically the types of the columns

maybe a different plugin, that sends report to the anonymizer, and the anonymizer uses that report to hash