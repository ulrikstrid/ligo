---
id: include
title: Including Other Contracts
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

Let us say that we have a contract that is getting a too large. If it
has a modular structure, you might find it useful to use the
`#include` statement to split the contract up over multiple files.

You take the code that you want to include and put it in a separate
file, for example `included.ligo`:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>

<TabItem value="pascaligo">

```pascaligo

// Demonstrate PascaLIGO inclusion statements, see includer.ligo

const foo : int = 144
```

</TabItem>
<TabItem value="cameligo">

```cameligo
// Demonstrate CameLIGO inclusion statements, see includer.mligo

let foo : int = 144
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
// Demonstrate ReasonLIGO inclusion statements, see includer.religo

let foo : int = 144;
```

</TabItem>
</Tabs>


And then you can include this code using the `#include` statement like so:

<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>

<TabItem value="pascaligo">

```pascaligo
#include "included.ligo"

const bar : int = foo
```

</TabItem>
<TabItem value="cameligo">

```cameligo
#include "included.mligo"

let bar : int = foo
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo
#include "included.religo"

let bar : int = foo;
```

</TabItem>
</Tabs>
