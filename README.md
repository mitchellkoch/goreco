# GoReCo

GoReCo is a gold standard evaluation for relation extraction consisting of exhaustive annotations of the 128 documents from the ACE 2004 newswire corpus for 48 relations based on Freebase properties.

## How to Evaluate Extractions

Export a file of extractions in JSON or EDN format, and run the goreco script on it. Specify whether you are using the dev set or the test set.

```sh
./goreco.sh evaluate test extractions.edn
```
