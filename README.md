# purescript-kanren

A [μKanren](https://github.com/jasonhemann/microKanren) implementation
in PureScript.

## Usage

```purescript
import Kanren

-- q is 5 or q is 6
run do
  q ← fresh
  q ?== 5 ?|| q ?== 6

-- [5,6] (two answers)

-- q is 5 and q is 6 (impossible)
run do
  q ← fresh
  q ?== 5 ?&& q ?== 6

-- [] (no answers)

import Data.Array ((..))

-- a and b appended are 1..6
run do
  q ← fresh
  a ← fresh
  b ← fresh
  q ?== [a, b]
  appendo a b (1..6)

-- [[[],[1,2,3,4,5,6]],
--  [[1],[2,3,4,5,6]],
--  [[1,2],[3,4,5,6]],
--  [[1,2,3],[4,5,6]],
--  [[1,2,3,4],[5,6]],
--  [[1,2,3,4,5],[6]],
--  [[1,2,3,4,5,6],[]]]
```

## Missing Features

The amount of data types available are restricted by the `LogicValue`
implementation: strings, integers, and cons lists only.

## Licence

Copyright 2016 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.
