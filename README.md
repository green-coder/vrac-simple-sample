## Vrac's simple demo

This is a simple "frontend only" webapp project, used as a demo for the Vrac library.

It is divided into 3 files which can be compared side by side.

### Reference implementation in Re-frame

Inside `src/simple/reframe_demo.cljs`.

A simple app written in Re-frame, following the guidelines of Re-frame on how to handle data.

### Conceptual implementation in Vrac

Inside `src/simple/vrac_demo.cljc`.

The same app but implemented via Vrac components.
It is a conceptual source code, it does not run yet.

### Vrac compiled into Re-frame (crafted by hand for now)

Inside `src/simple/vrac_compiled_to_reframe_demo.cljs`

Vrac components can be compiled into running code.
This part shows how it looks like when Vrac uses Re-frame as a compilation target.
It is very instructive as it shows how Vrac handles the data under the hood.

## How to run the Reference and Vrac demos

Compile the source code via Shadow-CLJS:

```shell
npm install
npx shadow-cljs watch :reference-demo :vrac-demo
```

Then browse the page at [http://localhost:3000/](http://localhost:3000/)
