# GoReCo

GoReCo is a gold standard evaluation for relation extraction consisting of exhaustive annotations of the 128 documents from the [ACE 2004 newswire corpus](https://catalog.ldc.upenn.edu/LDC2005T09) for 48 relations based on Freebase properties.

## How to Evaluate Extractions

Export a file of extractions in JSON or EDN format, and run the goreco script on it. Specify whether you are using the dev set or the test set.

```sh
./goreco.sh evaluate test extractions.edn
```

# License

Copyright (c) 2014, University of Washington

Code by Mitchell Koch. Annotations by Anand Mohan, Mitchell Koch, and Graeme Britz.

Code released under the MIT license: http://www.opensource.org/licenses/mit-license.php

Annotation data released under the Creative Commons Attribution 4.0 International license: http://creativecommons.org/licenses/by/4.0/
