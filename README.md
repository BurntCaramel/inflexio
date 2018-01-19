# Inflexio

Pluralize and singularize words in Elm.

Based on [nurugger07/inflex](https://github.com/nurugger07/inflex) and [Active Support](https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb).

```elm
import Inflexio.Pluralize exposing (singularize, pluralize)

singularize "birds"
-- "bird"

singularize "radii"
-- "radius"

singularize "sheep"
-- "sheep"

pluralize "bird"
-- "birds"

pluralize "radius"
-- "radii"

pluralize "sheep"
-- "sheep"
```
