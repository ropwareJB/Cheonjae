
## Cheonjae

A daemon for monitoring language exchange applications and adding new vocabulary to a data store, such as Anki.

### Building

```
make bin
```

### Running

```
cd ./bin
export TANDEM_SESSION="AAAAA...."
./cheonjae
```

### TODO

- Inputs
	- Monitor Tandem
		- Continously monitor conversation list
		- Continuously monitor conversation
	- Monitor HelloTalk

- Outputs
	- Ingest into Anki
	- Ingest into Quizlet

