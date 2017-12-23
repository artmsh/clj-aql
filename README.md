# clj-aql

Clojure language extension that provide idiomatic syntax for [ArangoDB](https://arangodb.com/) query language.

[![CircleCI](https://circleci.com/gh/artmsh/clj-aql.svg?style=svg)](https://circleci.com/gh/artmsh/clj-aql)

## Installation
[Clojars](https://clojars.org/clj-aql)

```clojure
[clj-aql "0.1.1-SNAPSHOT"]
```

## Usage

```clojure
(require [clj-aql.core :refer :all])

(FOR [u] :IN "users"
  (RETURN u.name))
```

`FOR` is a macro which expands to following structure:
```clojure
{:query
"FOR u IN users
RETURN u.name"
:args {}}
```

More documentation coming soon.
## License

Copyright © 2017 Artem Mishchenko

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
