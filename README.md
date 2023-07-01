
## Cheonjae

A daemon for monitoring an input source, automatically translating it via GPT, and adding the new source-translation pair to a language learning data store, such as Anki.

### Building

```
make bin
```

### Running

```
cd ./bin
export GPT_KEY="sk-...."
./cheonjae -i ./someText.txt
```

### TODO

- Inputs
	- Text File

- Outputs
	- Ingest into Anki
	- Ingest into Quizlet

