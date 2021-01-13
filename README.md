## Setup

```sh
npm install
npm run clean; npm run start
npx webpack -w
open localhost:4500
```

## Avenues for complexity

- Mobile device support

- Use position: absolute for rendering text, and do layoutting ourselves.
  Implement HitTesting as well since we will no longer have that from the
  browser
  (https://github.com/pixijs/pixi.js/blob/dev/packages/interaction/src/TreeSearch.ts)

- Cross-browser support (for eg:
  https://stackoverflow.com/questions/4194163/detect-printable-keys/38802011#comment89017864_38802011)

- Implement MarkDown support

- Use WebGL
