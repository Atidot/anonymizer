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

* `host-secret-folder` must contain the following files: `ssl-key.key`,`ssl-cert.crt`, `hashing-key.key`
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
